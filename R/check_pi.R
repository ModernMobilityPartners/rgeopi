#' Check if PI is possible based on format
#'
#' GDOT PIs are seven character IDs formatted as 1) numbers with leading zeros, 2) six numbers and a "-", and 3) six numbers with a leading M or S. Checks the format to skip running a GeoPI call if the project ID is not possible for the system.
#'
#' @param gdot_pi GDOT PI. If not seven digits, will be padded if it's numeric
#'
#' @return True/False
#' @export
#'
#' @examples
#' check_pi("0012345")
#' check_pi("546540-")
#' check_pi("M023424")
#' check_pi("FAKE523")
check_pi <- function(gdot_pi){
  gdot_pi <- ifelse(nchar(gdot_pi)==7, gdot_pi, ifelse(!is.na(suppressWarnings(as.numeric(gdot_pi))), suppressWarnings(sprintf("%07d",as.numeric(gdot_pi))),gdot_pi ))
  a1 <- nchar(gdot_pi)==7
  a2 <- suppressWarnings(!is.na(as.numeric(gdot_pi))) & substr(gdot_pi,1,1)==0
  a3 <- suppressWarnings(!is.na(as.numeric(substr(gdot_pi,1,6)))) & substr(gdot_pi,7,7)=="-"
  a4 <- suppressWarnings(!is.na(as.numeric(substr(gdot_pi,2,7)))) & substr(gdot_pi,1,1) %in% c("S","M")

  results <- a1 & (a2 | a3 | a4)
  names(results) <- gdot_pi
  return(results)
}
