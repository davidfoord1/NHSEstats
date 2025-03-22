#' Extract all links from a web page
#'
#' Parses a page using [rvest], pulling the href attribute value from all <a>
#' elements in the page. Only keeps strings starting with "https" so ignores
#' e.g. header links.
#'
#' @param url Web address to search for links
#'
#' @returns (chr) Vector of link strings
#'
#' @keywords internal
links_in_page <- function(url) {
  links <- url |>
    rvest::read_html() |>
    rvest::html_elements("a") |>
    rvest::html_attr("href") |>
    stats::na.omit()

  is_https <- startsWith(links, "https://")

  links[is_https]
}
