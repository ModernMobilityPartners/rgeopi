
#' Check if sf exists
#'
#' Check if shapefile available on GeoPI without downloading
#'
#' @param gdot_pi GDOT Project ID. Can be a single PI or a vector or list of PIs. If more than one PI is provided, each project will get its own row.
#' @param pi_check Check if the PI is a valid format. Defaults to TRUE.
#'
#' @return A tibble
#' @export
#'
#' @examples
#' check_geopi_sf(c("0015478","0001111"))
check_geopi_sf <- function(gdot_pi, pi_check= TRUE) {
  if(pi_check){
    pi_review <- check_pi(gdot_pi)

    invalid <- gdot_pi[!pi_review]
    if(length(invalid)>0){
      warning(sprintf("Invalid PI#s: %s",paste(invalid, collapse=", ")))
    }
    gdot_pi <- names(pi_review)[pi_review]
  }

  if(length(gdot_pi)==0){
    stop("No valid PIs")

  }

  req_url <- polite_request("https://rnhp.dot.ga.gov/")

  gdot_pis <- unique(gdot_pi)

  all_sf <- purrr::map_dfr(
    .x = gdot_pis,
    .f = function(gdot_pi = .x) {
      message("Fetching spatial data for PI:", gdot_pi)
      resp <- req_url %>%
        httr2::req_url_path("hosting/rest/services/GEOPI_APP/MapServer/0/query") %>%
        httr2::req_url_query(
          `f` = "json",
          `where` = glue::glue("PROJECT_ID=\'{gpi}\'", gpi = gdot_pi),
          `returnGeometry` = "false",
          `spatialRel` = "esriSpatialRelIntersects"
        ) %>%
        httr2::req_perform()

      gpi_sf <- sf::read_sf(resp$url)

      if(nrow(gpi_sf)==0){
        gpi_sf <- tibble::tibble(PROJECT_ID = gdot_pi, EXISTS = FALSE)
      }else{
        gpi_sf$EXISTS <- TRUE
      }

      return(gpi_sf)
    }
  ) %>%
    dplyr::distinct()
  return(all_sf)
}
