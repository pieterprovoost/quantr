#' Get Euronext tickers
#'
#' This gets all Euronext ticker symbols from the Euronext website. Possible MIC codes
#' are ALXB, ALXL, ALXP, XPAR, XAMS, XBRU, XLIS, XMLI, MLXB, ENXB, ENXL, TNLA, TNLB,
#' XLDN, XESM, XMSM, XATL, VPXB, XOSL, XOAS, MERK.
#'
#' @param mics MIC codes
#' @return tibble containing symbols and some metrics
#' @export
#' @examples
#' euronext_tickers("XBRU")
euronext_tickers <- function(mics = c("ALXB", "ALXL", "ALXP", "XPAR", "XAMS", "XBRU", "XLIS", "XMLI", "MLXB", "ENXB", "ENXL", "TNLA", "TNLB", "XLDN", "XESM", "XMSM", "XATL", "VPXB", "XOSL", "XOAS", "MERK")) {
  mics_string <- paste0(mics, collapse = ",")
  body <- list(
    "iDisplayLength" = "100",
    "iDisplayStart" = "0",
    "args[initialLetter]" = NULL,
    "args[fe_type]" = "csv",
    "args[fe_layout]" = "ver",
    "args[fe_decimal_separator]" = ".",
    "args[fe_date_format]" = "d/m/y"
  )
  url <- glue::glue("https://live.euronext.com/pd/data/stocks/download?mics={mics_string}&display_datapoints=dp_stocks&display_filters=df_stocks")
  res <- httr::POST(url, body = body, encode = "multipart")
  text <- rawToChar(httr::content(res, "raw"))
  df <- read.csv(text = text, sep = ";")[-(1:3),]
  result <- df %>%
    select(
      name = Name,
      isin = ISIN,
      symbol = Symbol,
      market = Market,
      currency = Trading.Currency,
      open = Open,
      high = High,
      low = Low,
      last = Last,
      last_datetime = Last.Date.Time,
      timezone = Time.Zone,
      volume = Volume,
      turnover = Turnover
    ) %>%
    mutate_at(vars(open, high, low, last, volume, turnover), as.numeric) %>%
    mutate(timezone = replace(timezone, timezone %in% c("IST", "BST"), "WET")) %>%
    rowwise() %>%
    mutate(last_datetime = parse_date_time2(last_datetime, "dmy HM", tz = timezone)) %>%
    select(-timezone) %>%
    as_tibble()
  return(result)
}

#' Get Euronext bond tickers
#'
#' This gets all Euronext bond ticker symbols from the Euronext website. Possible MIC codes
#' are ALXB, ALXL, ALXP, XPAR, XAMS, XBRU, XLIS, XMLI, MLXB, ENXB, ENXL, TNLA, TNLB, XLDN,
#' XHFT, VPXB, XOSL, XOAM.
#'
#' @param mics MIC codes
#' @return tibble containing symbols and some metrics
#' @export
#' @examples
#' euronext_bonds("XBRU")
euronext_bonds <- function(mics = c("ALXB", "ALXL", "ALXP", "XPAR", "XAMS", "XBRU", "XLIS", "XMLI", "MLXB", "ENXB", "ENXL", "TNLA", "TNLB", "XLDN", "XHFT", "VPXB", "XOSL", "XOAM")) {
  mics_string <- paste0(mics, collapse = ",")
  body <- list(
    "iDisplayLength" = "10000",
    "iDisplayStart" = "0",
    "args[initialLetter]" = NULL,
    "args[fe_type]" = "csv",
    "args[fe_layout]" = "ver",
    "args[fe_decimal_separator]" = ".",
    "args[fe_date_format]" = "d/m/y"
  )
  url <- glue::glue("https://live.euronext.com/pd/data/bond/download?mics={mics_string}&display_datapoints=dp_bond&display_filters=df_bond")
  res <- httr::POST(url, body = body, encode = "multipart")
  text <- rawToChar(httr::content(res, "raw"))
  text_fixed <- stringr::str_replace_all(text, ";-\n", "\n")
  df <- read.csv(text = text_fixed, sep = ";")[-(1:3),]
  result <- df %>%
    select(
      name = Name,
      isin = ISIN,
      symbol = Symbol,
      market = Market,
      maturity = Maturity,
      currency = Trading.Currency,
      open = Open,
      high = High,
      low = Low,
      last = Last,
      last_datetime = Last.Date.Time,
      timezone = Time.Zone,
      volume = Volume,
      issuer = Issuer
    ) %>%
    mutate_at(vars(open, high, low, last, volume), as.numeric) %>%
    mutate(timezone = replace(timezone, timezone %in% c("IST", "BST"), "WET")) %>%
    rowwise() %>%
    mutate(last_datetime = parse_date_time2(last_datetime, "dmy HM", tz = timezone)) %>%
    mutate(maturity = parse_date_time2(maturity, "dmy")) %>%
    select(-timezone) %>%
    as_tibble()
  return(result)
}
