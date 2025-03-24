#' Recursively extract nested links using regex patterns
#'
#' Performs a recursive link search starting from one or more input URLs,
#' applying a sequence of regular expression patterns to progressively filter
#' and descend into nested web pages. Each pattern in the `patterns` vector
#' corresponds to a depth level in the traversal.
#'
#' The last pattern is expected to be for download links, which are returned as
#' a character vector of matching links. At higher levels, it returns a named
#' list, where each name is a URL that matched at that depth, and each value is
#' the result of the recursive search beneath it.
#'
#' @param urls Character vector of URLs to search. At the top level, this will
#'   typically be a single page URL; at deeper levels, it represents pages
#'   matched from a previous pattern.
#'
#' @param patterns Character vector of regular expression patterns to apply.
#'   The function applies one pattern per recursion depth. The final pattern
#'   should match the actual downloadable files (e.g., ZIPs), and earlier
#'   patterns should match nested page URLs (e.g., year or month pages).
#'
#' @param depth Integer indicating the current recursion level (used internally).
#'   Users should not normally need to set this manually.
#'
#' @return A nested list structure:
#'   - At the last level, a character vector of matched file URLs.
#'   - At intermediate levels, a named list where each name is a matched URL and
#'     each value is the result of the next recursive step.
#'
#' @keywords internal
links_by_regex <- function(urls, patterns, depth = 1L) {
  # Get all links on the current page
  links <- links_in_page(urls)
  matches <- grepl(patterns[depth], links)
  matching_links <- unique(links[matches])

  # Base case
  # If this is the last pattern, just return the matching links
  if (depth == length(patterns)) {
    return(matching_links)
  }

  # For each matching link at this level, recurse into next level
  result <- vector("list", length(matching_links))
  names(result) <- matching_links

  for (i in seq_along(matching_links)) {
    child_url <- matching_links[[i]]
    result[[i]] <- links_by_regex(child_url, patterns, depth + 1L)
  }

  result
}
