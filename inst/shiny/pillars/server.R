library(dplyr)
library(ggplot2)

server <- function(input, output) {
  df <- reactiveValues(pillars = data.frame(metric = "No data", value = "", score = ""), price = NA, financials = NA, name = "", exchange = "")

  observeEvent(input$go, {

    pillars <- quantr::eight_pillars(input$symbol)
    df$pillars <- data.frame(
      metric = c("PE ratio", "ROIC", "Revenue growth", "Net income growth", "Shares outstanding", "FCF growth", "P/FCF"),
      value = c(pillars$pe_ratio, pillars$roic, pillars$revenue_growth, pillars$net_income_growth, pillars$shares_outstanding, pillars$fcf_growth, pillars$pfcf),
      score = c(pillars$pe_ratio_ok, pillars$roic_ok, pillars$revenue_growth_ok, pillars$net_income_growth_ok, pillars$shares_outstanding_ok, pillars$fcf_growth_ok, pillars$pfcf_ok)
    ) %>%
      mutate(
        score = ifelse(score == TRUE, "✅", "❌")
      )

    history <- quantr::yahoo_history(input$symbol, interval = "1wk", days = 365 * 3)
    df$price <- ggplot(history, aes(date, close)) +
      geom_line() +
      expand_limits(y = 0) +
      ggtitle(input$symbol)

    fin <- yahoo_financials(input$symbol) %>%
      select(symbol, endDate, totalRevenue, netIncome, freeCashflow) %>%
      mutate(endDate = factor(as.Date(endDate), levels = rev(as.character(as.Date(unique(endDate)))))) %>%
      tidyr::gather(metric, value, 3:5) %>%
      mutate(metric = factor(metric, levels = c("totalRevenue", "netIncome", "freeCashflow")))
    df$financials <- ggplot(data = fin) +
      geom_bar(aes(x = endDate, y = value, fill = metric), stat = "identity", position = "dodge", width = 0.4) +
      scale_y_continuous(labels = scales::unit_format(unit = "M", scale = 1e-6)) +
      scale_fill_manual(values = c("#2F4858", "#86BBD8", "#33658A")) +
      ggtitle(input$symbol)

    s <- yahoo_summary(input$symbol)
    df$name = s$longName
    df$exchange = s$exchangeName

  })

  output$table <- renderTable(df$pillars)
  output$plot <- renderPlot(df$price)
  output$financials <- renderPlot(df$financials)
  output$name <- renderText(df$name)
  output$exchange <- renderText(df$exchange)

}
