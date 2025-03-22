#' Download NHS England data for a specified statistical area
#'
#' @param area (chr) String name for statistical work area
#' @param dest (chr) String file path for destination folder to download
#' files to.
#' @param skip_existing (lgl) Whether to avoid re-downloading a file if it's
#' already in the `dest` folder, default `TRUE`.
#'
#' @return (data.frame) Download imformatio
#'
#' @export
#'
#' @examples
nhs_download <- function(area, dest = ".", skip_existing = TRUE) {
  area_config <- load_yaml_config("areas.yaml")

  area_config
}

