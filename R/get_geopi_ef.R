#'
#' @description
#'   The function `get_geopi_ef` is a more efficient version of `get_geopi` that avoids pinging the GeoPi website excess times. It is a more recent development and may eventually replace `get_geopi`.
#'
#' @param gdot_pi GDOT Project ID. Can take a list or vector of IDs.
#' @param session If NULL (the default), creates a new session. Can provide a session made with `polite::bow`. In line with polite/ethical scraping, you can use an existing session or the function will create one for you.
#' @param features Project features desired to retrieve. Can choose between "overview" (project name, description, etc.), "phases" (phases, their years, and money allocated), and "documents" (information about what documents GeoPI has for the project).
#' @param doc_mode If "documents" is chosen, the doc_mode conveys what information to retrieve. Options are "cr_only" (description of all files under "approved concept reports"), "cr_check" (simple TRUE/FALSE for if the project has approved concept reports), and "doc_summary" (the name, file path, and type of all project documents).
#' @param geometry if FALSE (the default), do not return spatial date. if TRUE, uses the `get_geopi_sf` function to add a sf tibble named "geometry" to the output list.
#' @param gather_date Date information is gathered from GeoPI. Defaults to today.
#' @param pi_check Check if the PI is a valid format. Defaults to TRUE.
#'
#' @rdname get_geopi
#'
#' @export
#'
get_geopi_ef <- function(gdot_pi, session = NULL, features = c("overview", "phases", "documents"), doc_mode = c("cr_only", "cr_check", "doc_summary"), geometry = FALSE, pi_check=TRUE, gather_date=NULL) { # , output = "by" ## needs an "output" value to change if the results are by PI or by overview/phase/documents

  features <- rlang::arg_match(features, multiple = TRUE)

  if(pi_check){
    pi_review <- check_pi(gdot_pi)

    invalid <- gdot_pi[!pi_review]
    if(length(invalid)>0){
      warning(sprintf("Invalid PI#s: %s",paste(invalid, collapse=", ")))
    }
    gdot_pi <- names(pi_review)[pi_review]
  }

  doc_mode <- rlang::arg_match(doc_mode)

  if (is.null(session)) {
    session <- polite::bow("https://www.dot.ga.gov/applications/geopi/Pages/Dashboard.aspx")}
  if(is.null(gather_date)){
    gather_date <- lubridate::today()
  }
  gdot_pi <- unique(gdot_pi)

  page_scrapes <- purrr::map(
    gdot_pi,
    ~polite::scrape(session, query = list(ProjectId = .x)),
    .progress = list(
      type = "iterator",
      format = "Loading project pages {cli::pb_bar} {cli::pb_percent}"
    )
  )

  geopi_results <- list()

  if ("overview" %in% features) {
    geopi_results$overview <- tryCatch(get_geopi_overview_ef(gdot_pi = gdot_pi, page_scrape = page_scrapes, gather_date = gather_date), error = function(e) paste("overview", e))
  }

  if ("phases" %in% features) {
    geopi_results$phases <- tryCatch(get_geopi_phase_ef(gdot_pi = gdot_pi, page_scrape = page_scrapes, gather_date = gather_date), error = function(e) paste("phases", e))
  }
  if ("documents" %in% features) {
    geopi_results$documents <- tryCatch(get_geopi_docs_ef(gdot_pi = gdot_pi, page_scrape = page_scrapes, dcmode = doc_mode, gather_date = gather_date), error = function(e) paste("documents", e))
  }
  if (geometry == TRUE) {
    geopi_results$geometry <- tryCatch(get_geopi_sf(gdot_pi = gdot_pi, pi_check = pi_check), error = function(e) paste("geometry", e))
  }

  return(geopi_results)
}


#'
#' @param gdot_pi GDOT Project ID. Can take a list or vector of IDs.
#' @param gather_date Date information is gathered from GeoPI. Defaults to today.
#' @param page_scrape Scraped GeoPI Page supplied by `get_geopi_ef`. Only present in `get_geopi_overview_ef`.
#'
#' @rdname get_geopi_overview
#'
get_geopi_overview_ef <- function(gdot_pi, page_scrape, gather_date = NULL) {

  if(is.null(gather_date)){
    gather_date <- lubridate::today()
  }

  n_iter <- length(gdot_pi)
  #
  #   pb <- progress::progress_bar$new(format = "(:spin) [:bar] :percent [Elapsed time: :elapsedfull || Estimated time remaining: :eta]",
  #                          total = n_iter,
  #                          complete = "=",   # Completion bar character
  #                          incomplete = "-", # Incomplete bar character
  #                          current = ">",    # Current bar character
  #                          clear = FALSE,    # If TRUE, clears the bar when finish
  #                          width = 100)      # Width of the progress bar

  geopi_data <- purrr::map2_dfr(
    .x = gdot_pi,
    .y = page_scrape,
    .f = function(gdot_pi = .x, page_scrape = .y) {
      # pb$tick

      # message("Fetching project data for PI:", gdot_pi)

      # table of project details (location, etc). column 1 and 3 are headings, 2 and 4 are details.
      # row 1 of all columns is the project name
      tryCatch(
        {
          project_overview <- page_scrape %>%
            rvest::html_node("#ctl00_ctl51_g_f10b03f0_acac_4f79_94ee_72b988c6533b_ctl09 > table") %>%
            rvest::html_table()

          # table of project description
          project_description <- page_scrape %>%
            rvest::html_node("#ctl00_ctl51_g_fa9abf0f_0f34_4a8a_8910_0a437a68f590_ctl09 > table") %>%
            rvest::html_table()


          proj_desc_tidy <- dplyr::bind_rows(
            tibble::tibble(name = "Project Name", value = project_overview[[1, 1]]),
            project_overview[2:nrow(project_overview), c("X1", "X2")] %>% dplyr::rename("name" = "X1", "value" = "X2"),
            project_overview[2:7, c("X3", "X4")] %>% dplyr::rename("name" = "X3", "value" = "X4"), # nrow(project_overview)
            tibble::tibble(name = "Project Description", value = project_description[[2, 1]])
          ) %>%
            dplyr::mutate(
              name = stringr::str_replace(name, ":", ""),
              value = dplyr::case_when(
                stringr::str_detect(value, "^format") ~ stringr::str_extract(value, "(?<=\\').*(?=\\')"),
                value == "%" ~ "0%",
                .default = value
              )
            )

          proj_desc_wide <- proj_desc_tidy %>%
            tidyr::pivot_wider() %>%
            tibble::as_tibble(.name_repair = ~ vctrs::vec_as_names(..., repair = "universal", quiet = T)) %>%
            dplyr::mutate(
              dplyr::across(
                .cols = c(Right.of.Way.Authorization, Notice.to.Proceed.Date, Current.Completion.Date, Work.Completion.Date),
                .fns = \(x) lubridate::parse_date_time(x, orders = c("m/d/Y HMS p")) %>% lubridate::as_date()
              ),
              Construction.Contract.Amount = as.numeric(Construction.Contract.Amount),
              Gather.Date = gather_date
            )

          project_details_df <- proj_desc_wide %>%
            dplyr::select(Project.ID, tidyselect::everything())
          return(project_details_df)
        },
        error = function(cond) {
          message(paste("No overview available for PI:", gdot_pi, sep = " "))
          project_details_df <- tibble::tibble(
            Project.ID = gdot_pi,
            Project.Name = "No Content Available"
          )
          return(project_details_df)
        }
      )
    },
    .progress = list(
      type = "iterator",
      format = "Getting project details {cli::pb_bar} {cli::pb_percent}"
    )
  )
  return(geopi_data)
}


#'
#' @param gdot_pi GDOT Project ID. Can take a list or vector of IDs.
#' @param gather_date Date information is gathered from GeoPI. Defaults to today.
#' @param page_scrape Scraped GeoPI Page supplied by `get_geopi_ef`. Only present in `get_geopi_phase_ef`.
#'
#' @rdname get_geopi_phase
#'
get_geopi_phase_ef <- function(gdot_pi, page_scrape, gather_date = NULL) {

  if(is.null(gather_date)){
    gather_date <- lubridate::today()
  }

  n_iter <- length(gdot_pi)

  geopi_data <- purrr::map2_dfr(
    .x = gdot_pi,
    .y = page_scrape,
    .f = function(gdot_pi = .x, page_scrape = .y) {
      # message("Fetching phase data for PI:", gdot_pi)

      # table of project details (location, etc). column 1 and 3 are headings, 2 and 4 are details.
      # row 1 of all columns is the project name

      # Table of activity / program year / cost estimate / date of estimate
      phase_details <- page_scrape %>%
        rvest::html_node("#ctl00_ctl51_g_b8d3a566_e67d_4ccb_95d8_24b07c277a46_g_b8d3a566_e67d_4ccb_95d8_24b07c277a46DataGrid_ctl00 > tbody") %>%
        rvest::html_table()
      names(phase_details) <- page_scrape %>%
        rvest::html_node("#ctl00_ctl51_g_b8d3a566_e67d_4ccb_95d8_24b07c277a46_g_b8d3a566_e67d_4ccb_95d8_24b07c277a46DataGrid_ctl00 > thead") %>%
        rvest::html_table()%>%
        names()

      phase_page_count <- page_scrape %>%
        rvest::html_node("#ctl00_ctl51_g_b8d3a566_e67d_4ccb_95d8_24b07c277a46_g_b8d3a566_e67d_4ccb_95d8_24b07c277a46DataGrid_ctl00 > tfoot > tr > td > table > tbody > tr > td > div.rgWrap.rgInfoPart")%>%
        rvest::html_text2()%>%
        stringr::str_replace_all("\\r","")%>%
        stringr::str_trim()


      if(phase_details[[1,1]] %in% c("There are no items to show in this view.","()")){
        message("No phase data for PI:", gdot_pi)

        phase_details_missing <- tibble::tibble(
          Program.Year = NA_integer_,
          Date.of.Last.Estimate = lubridate::NA_Date_,
          Cost.Est.USD = NA_real_,
          Phase = "No Project Information on GeoPI",
          Project.ID = gdot_pi,
          Gather.Date = gather_date
        ) %>%
          tidyr::nest(Phase.Details = c(Phase, Program.Year, Date.of.Last.Estimate, Cost.Est.USD)) %>%
          dplyr::select(Project.ID, tidyselect::everything())
      }else{
      phase_details_clean <- phase_details %>%
        tibble::as_tibble(.name_repair = ~ vctrs::vec_as_names(..., repair = "universal", quiet = T)) %>%
        dplyr::mutate(
          Date.of.Last.Estimate = lubridate::mdy(Date.of.Last.Estimate),
          Cost.Est.USD = stringr::str_replace_all(Cost.Estimate, "[\\$,]", "") %>% as.numeric(),
          Phase = stringr::str_extract(Activity, "[:alpha:]+")
        ) %>%
        dplyr::select(-Activity, -Cost.Estimate) %>%
        dplyr::mutate(
          Project.ID = gdot_pi,
          Gather.Date = gather_date
        ) %>%
        tidyr::nest(Phase.Details = c(Phase, Program.Year, Date.of.Last.Estimate, Cost.Est.USD)) %>%
        dplyr::select(Project.ID, tidyselect::everything())

      if(!is.na(phase_page_count)){
        phase_details_clean <- phase_details_clean%>%
          dplyr::mutate(
            Surplus.Records = phase_page_count
          )
      }

      return(phase_details_clean)
      }
    },
    .progress = list(
      type = "iterator",
      format = "Getting project phase details {cli::pb_bar} {cli::pb_percent}"
    )
  )
  return(geopi_data)
}

#'
#' @param gdot_pi GDOT Project ID. Can take a list or vector of IDs.
#' @param dcmode if "cr_only" (the default), returns the filepath and filenames of all approved concept reports. If "cr_check", merely returns T/F for each project if it has a concept report. If "doc_summary", returns the file name, type, and path for all project documents.
#' @param gather_date Date information is gathered from GeoPI. Defaults to today.
#' @param page_scrape Scraped GeoPI Page supplied by `get_geopi_ef`. Only present in `get_geopi_docs_ef`.
#'
#' @rdname get_geopi_docs
#'
get_geopi_docs_ef <- function(gdot_pi, page_scrape, dcmode = c("cr_only", "cr_check", "doc_summary"), gather_date = NULL) {
  dcmode <- rlang::arg_match(dcmode)

  if(is.null(gather_date)){
    gather_date <- lubridate::today()
  }

  geopi_data <- purrr::map2_dfr(
    .x = gdot_pi,
    .y = page_scrape,
    .f = function(gdot_pi = .x, page_scrape = .y) {
      message("Checking CR for PI:", gdot_pi)

      document_inclusion <- page_scrape %>%
        rvest::html_node("#ctl00_ctl51_g_c9069b02_6998_4434_b04d_f108ef5b6961_g_c9069b02_6998_4434_b04d_f108ef5b6961DataGrid_ctl00 > tbody") %>%
        rvest::html_table()%>%
        dplyr::mutate(...1 = NULL, X1 = NULL, dplyr::across(.cols = tidyselect::everything(), .fns = \(x) dplyr::na_if(x, "")))

      names(document_inclusion) <- page_scrape %>%
        rvest::html_node("#ctl00_ctl51_g_c9069b02_6998_4434_b04d_f108ef5b6961_g_c9069b02_6998_4434_b04d_f108ef5b6961DataGrid_ctl00 > thead") %>%
        rvest::html_table() %>%
        tibble::as_tibble(.name_repair = ~ vctrs::vec_as_names(..., repair = "universal", quiet = T)) %>%
        names()%>%
        .[-1]

      document_inclusion <- document_inclusion %>%
        tidyr::fill(Project.Documents) %>%
        dplyr::filter(Project.Documents != FILE_PATH)

      if(any(stringr::str_detect(document_inclusion$Project.Documents,"Showing\\s\\d+"))){
        document_inclusion <- document_inclusion%>%
          dplyr::mutate(
            Surplus.Records = stringr::str_extract(Project.Documents,"Showing\\s\\d+\\sof\\s\\d+\\sitems"),
            Project.Documents = stringr::str_replace(Project.Documents, "\\s\\(Showing \\d+ of \\d+ items\\. Group continues on the next page\\.\\)","")
          )
      }

      if (dcmode == "cr_check") {
        cr_tbl <- tibble::tibble(
          Project.ID = gdot_pi,
          CR.Exists = dplyr::if_else(nrow(document_inclusion[which(document_inclusion$Project.Documents == "Approved Concept Reports"), ]) > 0, TRUE, FALSE),
          Gather.Date = gather_date
        )
        return(cr_tbl)
      } else if (dcmode == "cr_only") {
        concept_reports <- document_inclusion %>%
          dplyr::mutate(Project.Documents = stringr::str_replace_all(Project.Documents, "\\s", "\\.")) %>%
          dplyr::select(
            Doc.Type = Project.Documents,
            File.Path = FILE_PATH,
            File.Name = FILE_NAME
          ) %>%
          dplyr::filter(Doc.Type == "Approved.Concept.Reports") %>%
          tidyr::separate_wider_regex(
            File.Name,
            c(Project.ID = "[:digit:]{6,7}-?", "[\\s_]+", File.Type = "[:alpha:]*\\&*[:alpha:]*", "[\\s_]+", File.Date = "[:alnum:]+", "\\.pdf"),
            cols_remove = F, too_few = "align_start"
          ) %>%
          dplyr::mutate(
            Gather.Date = gather_date
          )
        if(nrow(concept_reports)==0){
          message("No concept reports found for PI:", gdot_pi)
        }
        return(concept_reports)
      } else if (dcmode == "doc_summary") {
        if (nrow(document_inclusion) == 0) {
          document_df <- tibble::tibble(
            Doc.Type = "No.Documents",
            n = 0,
            Project.ID = gdot_pi,
            Gather.Date = gather_date
          )
          message("No documents found for PI:", gdot_pi)
        } else {
          document_df <- document_inclusion %>%
            dplyr::mutate(Project.Documents = stringr::str_replace_all(Project.Documents, "\\s", "\\.")) %>%
            dplyr::select(
              Doc.Type = Project.Documents
            ) %>%
            dplyr::count(Doc.Type) %>%
            dplyr::mutate(
              Project.ID = gdot_pi,
              Gather.Date = gather_date
            )
        }

        return(document_df)
      }
    },
    .progress = list(
      type = "iterator",
      format = "Getting project document details {cli::pb_bar} {cli::pb_percent}"
    )
  )
  return(geopi_data)
}
