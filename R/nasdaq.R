get_json <- function(url) {
  res <- httr::GET(url, httr::user_agent("Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7)"))
  text <- httr::content(res, "text", encoding = "UTF-8")
  return(fromJSON(text))
}

#' Get Nasdaq tickers
#'
#' This gets all Nasdaq ticker symbols from the Nasdaq website.
#'
#' @param exchange exchange ("nasdaq", "nyse", or "amex")
#' @return data frame
#' @export
#' @examples
#' nasdaq_tickers()
nasdaq_tickers <- function(exchange = "nasdaq") {
  url <- glue::glue("https://api.nasdaq.com/api/screener/stocks?tableonly=true&limit=25&exchange={exchange}&download=true")
  res <- get_json(url)$data$rows
  result <- res %>%
    select(
      symbol,
      name,
      last = lastsale,
      change = netchange,
      volume = volume,
      market_cap = marketCap,
      country,
      industry,
      sector,
      ipo_year = ipoyear
    ) %>%
    mutate(
      last = parse_number(last),
      change = as.numeric(change),
      market_cap = as.numeric(market_cap),
      ipo_year = as.integer(ipo_year)
    ) %>%
    as_tibble()
  return(result)
}

#' Get NYSE tickers
#'
#' This gets all NYSE ticker symbols from the Nasdaq website.
#'
#' @return data frame
#' @export
#' @examples
#' nyse_tickers()
nyse_tickers <- function() {
  nasdaq_tickers("nyse")
}
