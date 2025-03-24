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
nhs_download <- function(area, dest = ".", skip_existing = TRUE) {
  area_config <- load_yaml_config("areas.yaml")
  area_config <- area_config[[area]]

  url <- paste0(default_url, area_config$url)

  dl_urls <- links_by_regex(url, area_config$patterns)
  file_names = basename(dl_urls)
  dest_file = file.path(dest, file_names)

  meta <- data.frame(dl_urls,
                     file_names,
                     dest_file)

  meta
}

