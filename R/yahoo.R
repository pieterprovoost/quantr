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

yahoo_financials_simple <- function(symbol, reporting) {
  modules <- ifelse(
    reporting == "annual",
    "balanceSheetHistory,cashflowStatementHistory,incomeStatementHistory",
    "balanceSheetHistoryQuarterly,cashflowStatementHistoryQuarterly,incomeStatementHistoryQuarterly"
  )
  url <- glue::glue("https://query2.finance.yahoo.com/v10/finance/quoteSummary/{symbol}?modules={modules}")
  res <- fromJSON(url)$quoteSummary$result
  flat <- lapply(res, function(x) {
    x[[1]][[1]] %>% select(-maxAge)
  })
  df <- suppressMessages(Reduce(full_join, flat)) %>%
    flatten() %>%
    select(-contains(".fmt"), -contains(".longFmt")) %>%
    mutate(endDate.raw = as.POSIXct(endDate.raw, origin = "1970-01-01"))
  colnames(df) <- gsub(".raw", "", colnames(df))
  df$symbol <- symbol
  return(as_tibble(df))
}

#' Get Yahoo Finance financials (income statement, balance sheet, cash flow)
#'
#' @param symbols ticker symbols
#' @param reporting reporting period ("annual" or "quarterly")
#' @return tibble
#' @export
#' @examples
#' yahoo_financials(c("AAPL", "MSFT"))
yahoo_financials <- function(symbols, reporting = "annual") {
  rows <- purrr::map(.x = symbols, .f = ~yahoo_financials_simple(.x, reporting))
  return(bind_rows(rows))
}
