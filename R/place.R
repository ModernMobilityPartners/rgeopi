#' Format sf for PLA+CE GIS Tool
#'
#' @param project_sf sf object of project geometry with Project ID in column "Project.ID" - Result of `get_geopi_sf`
#'
#' @returns sf object
#' @export
format_place_gis <- function(project_sf) {
  project_sf <- project_sf %>%
    dplyr::rename(
      project_id = Project.ID
    ) %>%
    dplyr::bind_cols(
      data.frame(
        "proj_elem" = NA_integer_, # required: project element id 0, 1, 2, 3...
        "proj_type" = NA_character_, # required: project element type
        "proj_sub" = NA_character_, # required: project element sub-type
        "road_name" = NA_character_, # required: project road/route Name:
        "proj_begin" = NA_character_, # optional: Project Begin Terminus/Cross Street 1
        "proj_end" = NA_character_, # optional: Project End Terminus/Cross Street 2
        "PE_year" = NA_integer_, # optional: Year, PE is likely to begin
        "est_name" = NA_character_, # optional: Estimators Name
        "est_org" = NA_character_, # optional: Estimators Organization
        "study_name" = NA_character_ # optional: Associated Study
      )
    )
  return(project_sf)
}
