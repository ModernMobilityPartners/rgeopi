#' geopi session
#'
#' `geopi_session` masks `polite::bow`, creating a polite session with GeoPI. This can be plugged into any of the `get_geopi` family of functions.
#'
#' @param ... any arguments passed to `polite::bow` can be passed to
#'
#' @return object of class `polite`, `session`
#' @export
#'
#' @examples
#' \donttest{
#' session <- geopi_session()
#' session
#' }
geopi_session <- function(...) {
  geopi_sess <- polite::bow(url = "https://www.dot.ga.gov/applications/geopi/Pages/Dashboard.aspx", ...)
  return(geopi_sess)
}
