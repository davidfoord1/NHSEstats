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
  area <- tolower(area)

  if (!is.character(area) || length(area) != 1L) {
    stop("`area` must be a single string.")
  }

  if (!is.character(dest) || length(dest) != 1L) {
    stop("`dest` must be a single string.")
  }

  if (!is.logical(skip_existing) || length(skip_existing) != 1L) {
    stop("`skip_existing` must be a single logical value.")
  }

  config <- load_yaml_config("areas.yaml")

  if (!area %in% names(config)) {
    stop("`area` not found in config file.")
  }

  area_config <- area_config[[area]]

  url <- paste0(default_url, area_config$url)

  url_list <- links_by_regex(url, area_config$patterns)
  flat_links <- flatten_nested_links(url_list)
  metadata <- as.data.frame(do.call(rbind, flat_links))

  colnames(metadata) <- paste0("url_", seq_len(ncol(metadata)))
  colnames(metadata)[ncol(metadata)] <- "dl_link"

  metadata$file_name <- basename(metadata$dl_link)
  metadata$dest_name <- file.path(dest, metadata$file_name)

  metadata
}

