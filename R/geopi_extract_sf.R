#' Get GeoPI Spatial Data
#'
#' Get the spatial geometry of a GDOT project. `get_geopi_sf2` uses the httr2 package.
#'
#' @param gdot_pi GDOT Project ID. Can be a single PI or a vector or list of PIs. If more than one PI is provided, each project will get its own row. get_geopi_sf2 can only take a single
#'
#' @return An sf tibble
#' @export
#'
#' @examples
#' \dontrun{
#' get_geopi_sf(gdot_pi = "0000820")
#' }
get_geopi_sf <- function(gdot_pi) {
  req_url <- polite_request("https://rnhp.dot.ga.gov/")

  gdot_pis <- unique(gdot_pi)

  all_sf <- purrr::map_dfr(
    .x = gdot_pis,
    .f = function(gdot_pi = .x){
      message("Fetching spatial data for PI:", gdot_pi)
      resp <- req_url %>%
        httr2::req_url_path("hosting/rest/services/GEOPI_APP/MapServer/0/query") %>%
        httr2::req_url_query(
          `f` = "json",
          `where` = glue::glue("PROJECT_ID=\'{gpi}\'", gpi = gdot_pi),
          `returnGeometry` = "true",
          `spatialRel` = "esriSpatialRelIntersects"
        ) %>%
        httr2::req_perform()

      gpi_sf <- sf::read_sf(resp$url) %>%
        dplyr::mutate(
          Project.ID = gdot_pi
        )
      return(gpi_sf)
    }) %>%
    dplyr::group_by(Project.ID) %>%
    dplyr::summarise()%>%
    sf::st_cast("MULTILINESTRING")
  return(all_sf)
}

polite_request <- polite::politely(httr2::request, verbose = T)


polite_get_geopi_sf <- function(gdot_pi) {
  req_url <- polite_request("https://rnhp.dot.ga.gov/")

  gdot_pis <- unique(gdot_pi)

  all_sf <- purrr::map_dfr(
    .x = gdot_pis,
    .f = function(gdot_pi = .x){
      message("Fetching spatial data for PI:", gdot_pi)
      resp <- req_url %>%
        httr2::req_url_path("hosting/rest/services/GEOPI_APP/MapServer/0/query") %>%
        httr2::req_url_query(
          `f` = "json",
          `where` = glue::glue("PROJECT_ID=\'{gpi}\'", gpi = gdot_pi),
          `returnGeometry` = "true",
          `spatialRel` = "esriSpatialRelIntersects"
        ) %>%
        httr2::req_perform()

      gpi_sf <- sf::read_sf(resp$url) %>%
        dplyr::mutate(
          Project.ID = gdot_pi
        )
      return(gpi_sf)
    }) %>%
    dplyr::group_by(Project.ID) %>%
    dplyr::summarise()%>%
    sf::st_cast("MULTILINESTRING")
  return(all_sf)
}


#' Get GeoPI Data
#'
#' Get the available GeoPI data for a project. Adds the field `Gather.Date` with the current date to document the date of extraction.
#'
#' @param gdot_pi GDOT Project ID. Can be a single PI or a vector or list of PIs. If more than one PI is provided, each project will get its own row.
#' @param geometry if FALSE (the default), return a regular tibble of GeoPI data. if TRUE, uses the get_geopi_sf function to return an sf tibble with simple feature geometry in the 'geometry' column.
#'
#' @return A tibble or an sf tibble depending on if geometry is FALSE or TRUE
#' @export
#'
#' @examples
#' \dontrun{
#' get_geopi(gdot_pi = "0000820")
#' }
get_geopi <- function(gdot_pi, geometry = FALSE) {
  session <- polite::bow("https://www.dot.ga.gov/applications/geopi/Pages/Dashboard.aspx")

  gdot_pi <- unique(gdot_pi)

  geopi_data <- purrr::map_dfr(
    .x = gdot_pi,
    .f = function(gdot_pi = .x) {
      gather_date <- lubridate::today()

      message("Fetching project data for PI:", gdot_pi)

      # table of project details (location, etc). column 1 and 3 are headings, 2 and 4 are details.
      # row 1 of all columns is the project name

      project_scrape <- polite::scrape(session, query = list(ProjectId = gdot_pi))

      project_overview <- project_scrape %>%
        rvest::html_node("#ctl00_ctl51_g_f10b03f0_acac_4f79_94ee_72b988c6533b_ctl09 > table") %>%
        rvest::html_table()

      # table of project description
      project_description <- project_scrape %>%
        rvest::html_node("#ctl00_ctl51_g_fa9abf0f_0f34_4a8a_8910_0a437a68f590_ctl09 > table") %>%
        rvest::html_table()
      # Table of activity / program year / cost estimate / date of estimate
      phase_details <- project_scrape %>%
        rvest::html_node("#ctl00_ctl51_g_b8d3a566_e67d_4ccb_95d8_24b07c277a46_g_b8d3a566_e67d_4ccb_95d8_24b07c277a46DataGrid_ctl00") %>%
        rvest::html_table()

      proj_desc_tidy <- dplyr::bind_rows(
        tibble::tibble(name = "Project Name", value = project_overview[[1, 1]]),
        project_overview[2:nrow(project_overview), c("X1", "X2")] %>% dplyr::rename("name" = "X1", "value" = "X2"),
        project_overview[2:7, c("X3", "X4")] %>% dplyr::rename("name" = "X3", "value" = "X4"), # nrow(project_overview)
        tibble::tibble(name = "Project Description", value = project_description[[2, 1]])
      ) %>%
        dplyr::mutate(
          name = stringr::str_replace(name, ":", ""),
          value = dplyr::na_if(value, "formatCurrency(\'0.0000\')"),
          value = dplyr::na_if(value, "%"),
          value = dplyr::na_if(value, ""),
          value = dplyr::na_if(value, "formatDate('')")
        )

      proj_desc_wide <- proj_desc_tidy %>%
        tidyr::pivot_wider() %>%
        tibble::as_tibble(.name_repair = ~ vctrs::vec_as_names(..., repair = "universal", quiet = T)) %>%
        dplyr::mutate(
          dplyr::across(
            .cols = c(Right.of.Way.Authorization, Notice.to.Proceed.Date, Current.Completion.Date, Work.Completion.Date),
            .fns = \(x) dplyr::if_else(x=="formatDate('')" | is.na(x), lubridate::NA_Date_, lubridate::mdy_hms(x)%>%lubridate::as_date())
          ),
          Gather.Date = gather_date
        )
      phase_details_clean <- phase_details %>%
        tibble::as_tibble(.name_repair = ~ vctrs::vec_as_names(..., repair = "universal", quiet = T)) %>%
        dplyr::mutate(
          Date.of.Last.Estimate = lubridate::mdy(Date.of.Last.Estimate),
          Cost.Est.USD = stringr::str_replace_all(Cost.Estimate, "[\\$,]", "") %>% as.numeric(),
          Phase = stringr::str_extract(Activity, "[:alpha:]+")
        ) %>%
        dplyr::select(-Activity, -Cost.Estimate) %>%
        dplyr::mutate(Project.ID = gdot_pi) %>%
        tidyr::nest(Phase.Details = c(Phase, Program.Year, Date.of.Last.Estimate, Cost.Est.USD))

      document_inclusion <- project_scrape %>%
        rvest::html_node("#ctl00_ctl51_g_c9069b02_6998_4434_b04d_f108ef5b6961_g_c9069b02_6998_4434_b04d_f108ef5b6961DataGrid_ctl00") %>%
        rvest::html_table() %>%
        tibble::as_tibble(.name_repair = ~ vctrs::vec_as_names(..., repair = "universal", quiet = T)) %>%
        dplyr::mutate(...1 = NULL, dplyr::across(.cols = tidyselect::everything(), .fns = \(x) dplyr::na_if(x, ""))) %>%
        tidyr::fill(Project.Documents) %>%
        dplyr::filter(Project.Documents != FILE_PATH) %>%
        dplyr::mutate(Project.Documents = stringr::str_replace_all(Project.Documents, "\\s", "\\.")) %>%
        dplyr::select(
          DOC_TYPE = Project.Documents,
          tidyselect::everything()
        )

      project_details_df <- dplyr::full_join(proj_desc_wide, phase_details_clean, by = "Project.ID") %>%
        dplyr::mutate(
          Documents.Exists = dplyr::if_else(nrow(document_inclusion) > 0, TRUE, FALSE),
          CR.Exists = dplyr::if_else(nrow(document_inclusion[which(document_inclusion$DOC_TYPE == "Approved.Concept.Reports"), ]) > 0, TRUE, FALSE)
        ) %>%
        dplyr::select(Project.ID, tidyselect::everything())


      if (geometry == TRUE) {
        gpi_sf <- get_geopi_sf(gdot_pi)

        project_details_df <- dplyr::full_join(project_details_df, gpi_sf, by = "Project.ID") %>%
          sf::st_as_sf()
      }

      return(project_details_df)
    }
  )
  return(geopi_data)
}


#' Title
#'
#' @param gdot_pi
#'
#' @return
#' @export
#'
#' @examples
get_geopi_overview <- function(gdot_pi, session = NULL) {

  if(is.null(session)){
  session <- polite::bow("https://www.dot.ga.gov/applications/geopi/Pages/Dashboard.aspx")
}
  gdot_pi <- unique(gdot_pi)

  n_iter <- length(gdot_pi)
#
#   pb <- progress::progress_bar$new(format = "(:spin) [:bar] :percent [Elapsed time: :elapsedfull || Estimated time remaining: :eta]",
#                          total = n_iter,
#                          complete = "=",   # Completion bar character
#                          incomplete = "-", # Incomplete bar character
#                          current = ">",    # Current bar character
#                          clear = FALSE,    # If TRUE, clears the bar when finish
#                          width = 100)      # Width of the progress bar

  geopi_data <- purrr::map_dfr(
    .x = gdot_pi,
    .f = function(gdot_pi = .x) {
      # pb$tick
      gather_date <- lubridate::today()

      # message("Fetching project data for PI:", gdot_pi)

      # table of project details (location, etc). column 1 and 3 are headings, 2 and 4 are details.
      # row 1 of all columns is the project name

      project_scrape <- polite::scrape(session, query = list(ProjectId = gdot_pi))

      project_overview <- project_scrape %>%
        rvest::html_node("#ctl00_ctl51_g_f10b03f0_acac_4f79_94ee_72b988c6533b_ctl09 > table") %>%
        rvest::html_table()

      # table of project description
      project_description <- project_scrape %>%
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
            str_detect(value, "^format") ~ stringr::str_extract(value,"(?<=\\').*(?=\\')"),
            value=="%" ~ "0%",
          .default = value
          )
        )

      proj_desc_wide <- proj_desc_tidy %>%
        tidyr::pivot_wider() %>%
        tibble::as_tibble(.name_repair = ~ vctrs::vec_as_names(..., repair = "universal", quiet = T)) %>%
        dplyr::mutate(
          dplyr::across(
            .cols = c(Right.of.Way.Authorization, Notice.to.Proceed.Date, Current.Completion.Date, Work.Completion.Date),
            .fns = \(x) lubridate::parse_date_time(x, orders = c("m/d/Y HMS p"))%>%as_date()
          ),
          Construction.Contract.Amount = as.numeric(Construction.Contract.Amount),
          Gather.Date = gather_date
        )

      document_inclusion <- project_scrape %>%
        rvest::html_node("#ctl00_ctl51_g_c9069b02_6998_4434_b04d_f108ef5b6961_g_c9069b02_6998_4434_b04d_f108ef5b6961DataGrid_ctl00") %>%
        rvest::html_table() %>%
        tibble::as_tibble(.name_repair = ~ vctrs::vec_as_names(..., repair = "universal", quiet = T)) %>%
        dplyr::mutate(...1 = NULL, dplyr::across(.cols = tidyselect::everything(), .fns = \(x) dplyr::na_if(x, ""))) %>%
        tidyr::fill(Project.Documents) %>%
        dplyr::filter(Project.Documents != FILE_PATH) %>%
        dplyr::mutate(Project.Documents = stringr::str_replace_all(Project.Documents, "\\s", "\\.")) %>%
        dplyr::select(
          DOC_TYPE = Project.Documents,
          tidyselect::everything()
        )

      project_details_df <- proj_desc_wide %>%
        dplyr::mutate(
          Documents.Exists = dplyr::if_else(nrow(document_inclusion) > 0, TRUE, FALSE),
          CR.Exists = dplyr::if_else(nrow(document_inclusion[which(document_inclusion$DOC_TYPE == "Approved.Concept.Reports"), ]) > 0, TRUE, FALSE)
        ) %>%
        dplyr::select(Project.ID, tidyselect::everything())

      return(project_details_df)
    },
    .progress = list(
      type = "iterator",
      format = "Getting project details {cli::pb_bar} {cli::pb_percent}"
    )
  )
  return(geopi_data)
}


#' Get Project Phase
#'
#' @param gdot_pi
#'
#' @return
#' @export
#'
#' @examples
get_geopi_phase <- function(gdot_pi) {
  session <- polite::bow("https://www.dot.ga.gov/applications/geopi/Pages/Dashboard.aspx")

  gdot_pi <- unique(gdot_pi)

  geopi_data <- purrr::map_dfr(
    .x = gdot_pi,
    .f = function(gdot_pi = .x) {
      gather_date <- lubridate::today()

      message("Fetching phase data for PI:", gdot_pi)

      # table of project details (location, etc). column 1 and 3 are headings, 2 and 4 are details.
      # row 1 of all columns is the project name

      project_scrape <- polite::scrape(session, query = list(ProjectId = gdot_pi))

      # Table of activity / program year / cost estimate / date of estimate
      phase_details <- project_scrape %>%
        rvest::html_node("#ctl00_ctl51_g_b8d3a566_e67d_4ccb_95d8_24b07c277a46_g_b8d3a566_e67d_4ccb_95d8_24b07c277a46DataGrid_ctl00") %>%
        rvest::html_table()

      phase_details_clean <- phase_details %>%
        tibble::as_tibble(.name_repair = ~ vctrs::vec_as_names(..., repair = "universal", quiet = T)) %>%
        dplyr::mutate(
          Date.of.Last.Estimate = lubridate::mdy(Date.of.Last.Estimate),
          Cost.Est.USD = stringr::str_replace_all(Cost.Estimate, "[\\$,]", "") %>% as.numeric(),
          Phase = stringr::str_extract(Activity, "[:alpha:]+")
        ) %>%
        dplyr::select(-Activity, -Cost.Estimate) %>%
        dplyr::mutate(Project.ID = gdot_pi) %>%
        tidyr::nest(Phase.Details = c(Phase, Program.Year, Date.of.Last.Estimate, Cost.Est.USD))


      document_inclusion <- project_scrape %>%
        rvest::html_node("#ctl00_ctl51_g_c9069b02_6998_4434_b04d_f108ef5b6961_g_c9069b02_6998_4434_b04d_f108ef5b6961DataGrid_ctl00") %>%
        rvest::html_table() %>%
        tibble::as_tibble(.name_repair = ~ vctrs::vec_as_names(..., repair = "universal", quiet = T)) %>%
        dplyr::mutate(...1 = NULL, dplyr::across(.cols = tidyselect::everything(), .fns = \(x) dplyr::na_if(x, ""))) %>%
        tidyr::fill(Project.Documents) %>%
        dplyr::filter(Project.Documents != FILE_PATH) %>%
        dplyr::mutate(Project.Documents = stringr::str_replace_all(Project.Documents, "\\s", "\\."),
                     Project.ID = gdot_pi
                      ) %>%
        dplyr::select(
          DOC_TYPE = Project.Documents,
          tidyselect::everything()
        )
     # document_inclusion %>%
     #    tidyr::nest(.by = Project.ID, .key = "Documents")

      project_details_df <- phase_details_clean %>%
        dplyr::mutate(
          # Documents.Exists = dplyr::if_else(nrow(document_inclusion) > 0, TRUE, FALSE),
          CR.Exists = dplyr::if_else(nrow(document_inclusion[which(document_inclusion$DOC_TYPE == "Approved.Concept.Reports"), ]) > 0, TRUE, FALSE)
        ) %>%
        dplyr::select(Project.ID, tidyselect::everything())


      return(project_details_df)
    },
    .progress = list(
      type = "iterator",
      format = "Getting project phase details {cli::pb_bar} {cli::pb_percent}"
    )
  )
  return(geopi_data)
}

#' Check for Concept Reports
#'
#' @param gdot_pi GDOT Project ID. Can take a list or vector of IDs.
#'
#' @return tibble
#' @export
#'
#' @examples
#' \dontrun{
#' cr_check(gdot_pi = "0000820")
#' }
cr_check <- function(gdot_pi) {
  session <- polite::bow("https://www.dot.ga.gov/applications/geopi/Pages/Dashboard.aspx")

  gdot_pi <- unique(gdot_pi)

  gather_date <- lubridate::today()

  geopi_data <- purrr::map_dfr(
    .x = gdot_pi,
    .f = function(gdot_pi = .x) {
      message("Checking CR for PI:", gdot_pi)
      document_inclusion <- polite::scrape(session, query = list(ProjectId = gdot_pi)) %>%
        rvest::html_node("#ctl00_ctl51_g_c9069b02_6998_4434_b04d_f108ef5b6961_g_c9069b02_6998_4434_b04d_f108ef5b6961DataGrid_ctl00") %>%
        rvest::html_table() %>%
        tibble::as_tibble(.name_repair = ~ vctrs::vec_as_names(..., repair = "universal", quiet = T)) %>%
        dplyr::mutate(...1 = NULL, dplyr::across(.cols = tidyselect::everything(), .fns = \(x) dplyr::na_if(x, ""))) %>%
        tidyr::fill(Project.Documents) %>%
        dplyr::filter(Project.Documents != FILE_PATH) %>%
        dplyr::mutate(Project.Documents = stringr::str_replace_all(Project.Documents, "\\s", "\\.")) %>%
        dplyr::select(
          DOC_TYPE = Project.Documents,
          tidyselect::everything()
        )

      concept_reports <- document_inclusion %>%
        dplyr::filter(DOC_TYPE == "Approved.Concept.Reports") %>%
        tidyr::separate_wider_regex(
          FILE_NAME,
          c(Project.ID = "[:digit:]{6,7}-?", "_", File.Type = "[:alpha:]*", "_", File.Date = "[:alnum:]+", "\\.pdf"),
          cols_remove = F
        )

      cr_tbl <- tibble::tibble(
        Project.ID = gdot_pi,
        CR.Exists = dplyr::if_else(nrow(concept_reports) > 0, TRUE, FALSE)
      )
      return(cr_tbl)
    }
  )
  return(geopi_data)
}
