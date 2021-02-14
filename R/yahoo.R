yahoo_summary_simple <- function(symbol, modules) {
  modules <- paste0(modules, collapse = ",")
  url <- glue::glue("https://query2.finance.yahoo.com/v10/finance/quoteSummary/{symbol}?modules={modules}")
  res <- fromJSON(url)$quoteSummary$result
  flat <- purrr::flatten(res)
  flat <- lapply(flat, function(e) {
    if (is.list(e)) {
      return(e[["raw"]])
    } else {
      return(e)
    }
  })
  clean <- flat[-which(duplicated(names(flat)))]
  clean["maxAge"] <- NULL
  clean <- purrr::compact(clean)
  return(as_tibble(clean))
}

#' Get Yahoo Finance summary data
#'
#' @param symbols ticker symbols
#' @param modules Yahoo Finance modules
#' @return tibble
#' @export
#' @examples
#' yahoo_summary(c("AAPL", "MSFT"))
yahoo_summary <- function(symbols, modules = c("defaultKeyStatistics", "financialData", "price", "quoteType", "summaryDetail")) {
  rows <- purrr::map(.x = symbols, .f = ~yahoo_summary_simple(.x, modules))
  return(bind_rows(rows))
}
