#' Get FTSE 350 tickers
#'
#' This gets all FTSE 350 ticker symbols from Wikipedia.
#'
#' @return tibble
#' @export
#' @examples
#' ftse_350_tickers()
ftse_350_tickers <- function() {
  url_100 <- "https://en.wikipedia.org/wiki/FTSE_100_Index#List_of_FTSE_100_companies"
  url_250 <- "https://en.wikipedia.org/wiki/FTSE_250_Index#List_of_FTSE_250_Index_companies"
  ftse_100 <- read_html(url_100) %>%
    html_element("#constituents") %>%
    html_table() %>%
    select(name = Company, symbol = EPIC) %>%
    as_tibble()
  ftse_250 <- read_html(url_250) %>%
    html_element("#constituents") %>%
    html_table() %>%
    select(name = Company, symbol = 2) %>%
    as_tibble()
  return(bind_rows(ftse_100, ftse_250))
}
