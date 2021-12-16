#' Score a stock on eight criteria
#'
#' @param symbol symbol
#' @return list
#' @export
eight_pillars <- function(symbol) {

  fin <- yahoo_financials(symbol)
  fin_web <- yahoo_financials_web(symbol)

  price <- yahoo_history(symbol, interval = "1d", days = 1, end_date = Sys.time()) %>%
    arrange(desc(date)) %>%
    head(1)

  # 1
  pe_ratio <- price$open / mean(fin_web$dilutedEps, na.rm = TRUE)
  pe_ratio_ok <- pe_ratio <= 22

  # 2
  roic <- mean(fin$freeCashflow / (fin$longTermDebt + fin$totalStockholderEquity), na.rm = TRUE)
  roic_ok <- roic >= 0.09

  # 3
  mod <- lm(fin$totalRevenue ~ fin$endDate)
  revenue_growth <- coef(mod)[["fin$endDate"]] / mean(fin$totalRevenue, na.rm = TRUE) * as.numeric(days(365))
  revenue_growth_ok <- revenue_growth >= 0

  # 4
  mod <- lm(fin$netIncome ~ fin$endDate)
  net_income_growth <- coef(mod)[["fin$endDate"]] / mean(fin$netIncome, na.rm = TRUE) * as.numeric(days(365))
  net_income_growth_ok <- net_income_growth >= 0

  # 5
  mod <- lm(fin_web$dilutedAverageShares ~ fin_web$endDate)
  shares_outstanding <- coef(mod)[["fin_web$endDate"]] / mean(fin_web$dilutedAverageShares, na.rm = TRUE) * as.numeric(days(365))
  shares_outstanding_ok <- shares_outstanding <= 0

  # 7
  mod <- lm(fin$freeCashflow ~ fin$endDate)
  fcf_growth <- coef(mod)[["fin$endDate"]] / mean(fin$freeCashflow, na.rm = TRUE) * as.numeric(days(365))
  fcf_growth_ok <- fcf_growth >= 0

  # 8
  pfcf <- price$open / mean(fin$freeCashflow / fin_web$dilutedAverageShares, na.rm = TRUE)
  pfcf_ok <- pfcf <= 20

  return(list(
    pe_ratio = pe_ratio,
    pe_ratio_ok = pe_ratio_ok,
    roic = roic,
    roic_ok = roic_ok,
    revenue_growth = revenue_growth,
    revenue_growth_ok = revenue_growth_ok,
    net_income_growth = net_income_growth,
    net_income_growth_ok = net_income_growth_ok,
    shares_outstanding = shares_outstanding,
    shares_outstanding_ok = shares_outstanding_ok,
    fcf_growth = fcf_growth,
    fcf_growth_ok = fcf_growth_ok,
    pfcf = pfcf,
    pfcf_ok = pfcf_ok
  ))
}
