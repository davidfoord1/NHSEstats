#' Download NHS England data for a specified statistical area
#'
#' Downloads one or more files from NHS England's published statistics website
#' for a given statistical work area (e.g., referral-to-treatment, A&E, diagnostics).
#' This function uses a recursive pattern-matching approach to locate file links
#' from area-specific web pages defined in a YAML configuration file.
#'
#' If `skip_existing` is `TRUE`, files already present in the destination
#' directory will be skipped. A metadata data frame is returned summarising
#' all discovered files and their download status.
#'
#' @param area Character string. The name of the statistical area to download,
#'   matching an entry in the YAML config (e.g. `"rtt"`).
#'
#' @param dest Character string. Folder path where downloaded files will be saved.
#'   Will be created if it does not exist.
#'
#' @param skip_existing Logical. Whether to avoid re-downloading files that
#'   already exist in the `dest` folder. Defaults to `TRUE`.
#'
#' @return A data frame with one row per file, containing:
#' \describe{
#'   \item{dl_link}{The URL of the downloaded (or skipped) file}
#'   \item{dest_name}{Relative path to downloaded file (best on `dest`).}
#'   \item{file_name}{The filename portion of the download}
#'   \item{pre_existing}{TRUE if the file already existed before download}
#'   \item{success}{TRUE if the download succeeded; NA if skipped}
#'   \item{time}{Download duration in seconds (NA if skipped)}
#' }
#'
#' @examples
#' \dontrun{
#' nhs_download("rtt", dest = "data/RTT")
#' }
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

  area_config <- config[[area]]

  url <- paste0(default_url, area_config$url)

  url_list <- links_by_regex(url, area_config$patterns)
  flat_links <- flatten_nested_links(url_list)
  metadata <- as.data.frame(do.call(rbind, flat_links))

  colnames(metadata) <- paste0("url_", seq_len(ncol(metadata)))
  colnames(metadata)[NCOL(metadata)] <- "dl_link"

  # filter out duplicates
  metadata <- metadata[!duplicated(metadata$dl_link), ]

  metadata$file_name <- basename(metadata$dl_link)
  metadata$dest_name <- file.path(dest, metadata$file_name)

  if (!dir.exists(dest)) dir.create(dest, recursive = TRUE)

  metadata$pre_existing <- file.exists(metadata$dest_name)

  if (skip_existing) {
    to_download <- metadata[!metadata$pre_existing, ]
  } else {
    to_download <- metadata
  }

  if (NROW(to_download) > 0L) {
    dl_metadata <- curl::multi_download(
      to_download$dl_link,
      to_download$dest_name
    )
  } else {
    # empty data.frame if there are none to download
    # i.e. they have all already exist
    dl_metadata <- data.frame(
      url = character(),
      success = logical(),
      status_code = integer(),
      resumefrom = numeric(),
      destfile = character(),
      error = character(),
      type = character(),
      modified = character(),
      time = numeric(),
      headers = I(list())
    )
  }

  metadata <- merge(metadata,
                    dl_metadata,
                    all.x = TRUE,
                    by.x = "dl_link",
                    by.y = "url")

  metadata[c("dl_link",
             "dest_name",
             "file_name",
             "pre_existing",
             "success",
             "time")]
}

