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

#' Flatten a nested list of URLs into rows of ancestry paths
#'
#' This internal function converts a recursively nested list of URLs — typically
#' created by [links_by_regex()] — into a flat structure, where each row
#' represents a full ancestry path from the root (e.g. landing page) to a final
#' download link.
#'
#' The input list should reflect the structure of successive regex matches at
#' each level of nesting, where intermediate list names correspond to matched
#' page URLs, and final elements are character vectors of file URLs (e.g. ZIPs).
#'
#' @param nested A nested list of character vectors and/or lists, where each
#'   level corresponds to a match from a recursive regex search. Typically the
#'   output of [links_by_regex()].
#'
#' @param path A character vector representing the ancestry path taken to reach
#'   the current level. Used internally during recursion and should not usually
#'   be set by the user.
#'
#' @return A list of character vectors, where each vector represents a row of
#'   links from top-level pages down to a final download URL. The result can be
#'   converted into a data.frame using [do.call(rbind, ...)].
#'
#' @keywords internal
flatten_nested_links <- function(nested, path = character()) {
  if (is.null(nested)) return(rows)

  # Final level - a character vector
  # Output ancestor path with each link string
  if (is.character(nested)) {
    return(lapply(nested, \(link) c(path, link)))
  }

  # Intermediate levels - list to traverse
  rows <- list()

  for (i in seq_along(nested)) {
    name <- names(nested)[i]
    child <- nested[[i]]

    child_rows <- flatten_nested_links(child, c(path, name))

    rows <- c(rows, child_rows)
  }

  rows
}
