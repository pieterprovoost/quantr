#' Get Xetra tickers
#'
#' This gets all Xetra ticker symbols from the Xetra website.
#'
#' @return data frame
#' @export
#' @examples
#' xetra_tickers()
xetra_tickers <- function() {
  url <- "https://www.xetra.com/resource/blob/1528/fd276b284c2590e4645b8be8e1e5e601/data/t7-xetr-allTradableInstruments.csv"
  res <- httr::GET(url)
  text <- rawToChar(httr::content(res, "raw"))
  df <- read.csv(text = text, sep = ";", skip = 2)
  result <- df %>%
    select(
      name = Instrument,
      isin = ISIN,
      symbol = Mnemonic,
      mic = MIC.Code,
      mic_primary = Primary.Market.MIC.Code,
      mic_reporting = Reporting.Market,
      currency = Settlement.Currency,
      instrument_type = Instrument.Type
    ) %>%
    mutate(market = "Xetra") %>%
    as_tibble()
  return(result)
}
