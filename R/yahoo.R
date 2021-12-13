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
    "incomeStatementHistory,balanceSheetHistory,cashflowStatementHistory",
    "incomeStatementHistoryQuarterly,balanceSheetHistoryQuarterly,cashflowStatementHistoryQuarterly"
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
  df <- df %>%
    mutate(freeCashflow = totalCashFromOperatingActivities + capitalExpenditures)
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
#' yahoo_financials(c("AAPL", "MSFT"), "quarterly")
yahoo_financials <- function(symbols, reporting = "annual") {
  rows <- purrr::map(.x = symbols, .f = ~yahoo_financials_simple(.x, reporting))
  return(bind_rows(rows))
}

#' Get historical prices from Yahoo Finance
#'
#' @param interval interval ("1d", "1wk", or "1mo")
#' @param days how many days to go back
#' @param end_date end date (default = Sys.time())
#' @return tibble
#' @export
yahoo_history <- function(symbol, interval = "1d", days = 30, end_date = Sys.time()) {
  period1 <- as.integer(end_date - as.difftime(days, unit = "days"))
  period2 <- as.integer(end_date)
  url <- glue::glue("https://query1.finance.yahoo.com/v7/finance/download/{symbol}?period1={period1}&period2={period2}&interval={interval}&events=history")
  read.csv(url) %>%
    select(date = Date, open = Open, high = High, low = Low, close = Close, adjusted_close = Adj.Close, volume = Volume) %>%
    mutate(symbol = symbol) %>%
    mutate_at(c("open", "close", "high", "low", "adjusted_close", "volume"), as.numeric) %>%
    mutate(date = parse_date_time2(date, "Ymd")) %>%
    as_tibble()
}

#' Search Yahoo Finance for tickers
#'
#' @param q search query (for example, ISIN, ticker symbol)
#' @return tibble
#' @export
yahoo_search <- function(q) {
  url <- glue::glue("https://query2.finance.yahoo.com/v1/finance/search?q={q}&newsCount=0")
  fromJSON(url)$quotes %>%
    as_tibble()
}

#' Plot key financials from Yahoo Finance
#'
#' @param financials output from yahoo_financials()
#' @return plot
#' @export
plot_yahoo_financials <- function(financials) {
  f <- financials %>%
    select(symbol, endDate, totalRevenue, netIncome, freeCashflow) %>%
    mutate(endDate = factor(as.Date(endDate), levels = as.character(as.Date(unique(endDate))))) %>%
    tidyr::gather(metric, value, 3:5) %>%
    mutate(metric = factor(metric, levels = c("totalRevenue", "netIncome", "freeCashflow")))
  ggplot(data = f) +
    geom_bar(aes(x = endDate, y = value, fill = metric), stat = "identity", position = "dodge") +
    scale_fill_manual(values = wesanderson::wes_palette("GrandBudapest1", n = 3)) +
    scale_y_continuous(labels = scales::unit_format(unit = "M", scale = 1e-6)) +
    facet_wrap(~symbol, ncol = 1, scales = "free")
}

extract_fin_row <- function(html, metric) {
  html %>% html_elements(xpath = glue::glue("//*[@title = '{metric}']/../..//*[@*='fin-col']")) %>% html_text() %>% gsub(",", "", .) %>% as.numeric()
}

#' Get Yahoo Finance financials from web page
#'
#' @param symbol ticker symbol
#' @return tibble
#' @export
yahoo_financials_web <- function(symbol) {
  html <- read_html(glue::glue("https://finance.yahoo.com/quote/{symbol}/financials"))
  headers <- html %>% html_element(xpath = "//*[text() = 'ttm']/../..") %>% html_children() %>% html_text()
  stopifnot(headers[1] == "Breakdown")
  thousands <- html %>% html_element(xpath = "//*[text() = 'All numbers in thousands']") %>% class() == "xml_node"
  suppressWarnings({
    basic_eps <- extract_fin_row(html, "Basic EPS")
    diluted_eps <- extract_fin_row(html, "Diluted EPS")
    basic_average_shares <- extract_fin_row(html, "Basic Average Shares") * 10^(thousands * 3)
    diluted_average_shares <- extract_fin_row(html, "Diluted Average Shares") * 10^(thousands * 3)
  })
  return(tibble(
    endDate = lubridate::parse_date_time(headers[-(1:2)], "%m%d%y"),
    basicEps = basic_eps[-1],
    dilutedEps = diluted_eps[-1],
    basicAverageShares = basic_average_shares[-1],
    dilutedAverageShares = diluted_average_shares[-1]
  ))
}
