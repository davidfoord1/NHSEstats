#' Nested search for links by regex pattern
#'
#' @param base_url (chr) String url to search for patterns.
#'
#' @param patterns (chr) Vector of regex patterns to search for. The final
#'   string should be the pattern for the file download link, any prior strings
#'   will be treated as pages to search through in a nested fashion.
#'
#' @return (chr) Vector of filtered link strings.
#'
#' @keywords internal
links_by_regex <- function(base_url, patterns) {
  urls <- links_in_page(base_url)

  for (i in seq_along(patterns)) {
    matches <- grepl(patterns[[i]], urls)
    urls <- unique(urls[matches])

    if (i < length(patterns)) {
      urls <- unlist(lapply(urls, links_in_page))
    }
  }

  urls
}
