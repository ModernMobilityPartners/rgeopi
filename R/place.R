#' Format sf for PLA+CE GIS Tool
#'
#' @param project_sf sf object of project geometry with Project ID in column "Project.ID" - Result of `get_geopi_sf`
#'
#' @returns sf object
#' @export
format_place_gis <- function(project_sf, project_overview = NULL) {
  if (!is.null(project_overview)) {
    project_overview <- project_overview %>%
      dplyr::mutate(
        road_name = dplyr::case_when(
          stringr::str_detect(Project.Name, "@") ~ stringr::str_squish(stringr::str_split_1(Project.Name, "@")[1]),
          .default = Project.Name
        ),
        proj_begin = dplyr::case_when(
          stringr::str_detect(Project.Name, "@") ~ stringr::str_squish(stringr::str_split_1(Project.Name, "@")[1]),
          .default = NA_character_
        ),
        proj_end = dplyr::case_when(
          stringr::str_detect(Project.Name, "@") ~ stringr::str_squish(stringr::str_split_1(Project.Name, "@")[2]),
          .default = NA_character_
        )
      ) %>%
      dplyr::select(
        project_id = Project.ID, road_name, proj_begin, proj_end
      )
  } else {
    project_overview <- data.frame(
      "project_id" = project_sf$Project.ID,
      "road_name" = rep(NA_character_, nrow(project_sf)), # required: project road/route Name:
      "proj_begin" = rep(NA_character_, nrow(project_sf)), # optional: Project Begin Terminus/Cross Street 1
      "proj_end" = rep(NA_character_, nrow(project_sf)) # optional: Project End Terminus/Cross Street 2
    )
  }

  project_sf <- project_sf %>%
    dplyr::rename(
      project_id = Project.ID
    ) %>%
    dplyr::bind_cols(
      data.frame(
        "proj_elem" = NA_integer_, # required: project element id 0, 1, 2, 3...
        "proj_type" = NA_character_, # required: project element type
        "proj_sub" = NA_character_, # required: project element sub-type
        "PE_year" = NA_integer_, # optional: Year, PE is likely to begin
        "est_name" = NA_character_, # optional: Estimators Name
        "est_org" = NA_character_, # optional: Estimators Organization
        "study_name" = NA_character_ # optional: Associated Study
      )
    ) %>%
    dplyr::full_join(
      project_overview, by = dplyr::join_by(project_id)
    ) %>%
    dplyr::select(
      project_id, proj_elem, proj_type, proj_sub, road_name, proj_begin, proj_end, PE_year, est_name, est_org, study_name
    ) %>%
    dplyr::mutate(
      proj_elem = dplyr::row_number(), .by = project_id
    )


  return(project_sf)
}
