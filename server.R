library(fredr)
library(zoo)
library(ggplot2)
library(data.table)
library(shiny)
library(xts)
library(DT)
library(plotly)
library(memoise)

API_KEY = "<YOUR API KEY FROM FRED>"

Sys.setenv(FRED_API_KEY = API_KEY)


getSOFRData <- function() {
  # Fetch SOFR data from the API
  SOFR <- fredr(
    series_id = "SOFR",
    observation_start = as.Date("2018-04-01"),
    observation_end = Sys.Date(),
    frequency = "m",
    aggregation_method = "avg"
  )
  # Process the data
  SOFR <- na.omit(SOFR)
  SOFR <- data.table(SOFR[, c("date", "value")])
  setnames(SOFR, c("date", "value"))
  
  # Calculate the day-to-day change in rates
  SOFR$RateChange <- c(NA, diff(SOFR$value))
  
  return(SOFR)
}

memo_getSOFRData <- memoise(getSOFRData)
SOFR <- memo_getSOFRData()


ggplot(SOFR, aes(x=date, y=value)) +
  geom_line() +
  labs(title="Historical Interest Rates", x="Date", y="Interest Rate (%)") +
  theme_minimal()


swap_strategy <- function(fixedRate, notionalAmount, SOFRData) {
  # Convert the annual fixed rate percentage to a decimal
  fixedRateDecimal <- fixedRate / 100
  
  # Convert SOFR rates to decimal
  SOFRData[, value := value / 100]
  
  # Calculate monthly fixed payments based on the fixed rate and notional amount
  monthlyFixedPayment <- (fixedRateDecimal / 12) * notionalAmount
  
  # Calculate monthly floating payments based on SOFR rates and notional amount
  monthlyFloatingPayments <- (SOFRData$value / 12) * notionalAmount
  
  # Calculate the difference between fixed and floating payments for each month
  paymentDifferences <- monthlyFloatingPayments - monthlyFixedPayment
  
  # Add the results back into the SOFRData table
  SOFRData[, `:=`(fixedPayment = monthlyFixedPayment,
                  floatingPayment = monthlyFloatingPayments,
                  paymentDifference = paymentDifferences)]
  
  return(SOFRData)
}

vainilla_swap = function(notional, clientRateCurrent, clientRateAlter, BankFee, swapRateReceive) {
  clientRateCurrent = clientRateCurrent / 100
  clientRateAlter = clientRateAlter / 100
  BankFee = BankFee / 100
  swapRateReceive = swapRateReceive / 100
  saving = (swapRateReceive - clientRateCurrent + clientRateAlter - BankFee) * notional
  return (saving)
}

# Revised Duration Hedging Function
calculate_duration_hedging <- function(total_liability, bond_price, duration_liability, duration_bond, hedge_percent) {
  hedge_amount = total_liability * (hedge_percent/100)
  num_contracts = (hedge_amount*duration_liability) / (bond_price*duration_bond)
  total_cost = num_contracts * bond_price
  duration_assets_in = duration_bond
  duration_liability_out = duration_liability
  
  list(
    duration_assets_in = duration_assets_in,
    duration_liability_out = duration_liability_out,
    num_contracts = num_contracts,
    total_cost = total_cost
  )
}


function(input, output) {
  swapData <- reactive({
    SOFR <- getSOFRData()
    result <- swap_strategy(input$fixedRate, input$notionalAmountVanilla, SOFR)
    result
  })
  
  output$swapStrategyResults <- renderDT({
    datatable(swapData()[, .(date, fixedPayment, floatingPayment, paymentDifference)],
              options = list(pageLength = 10, searching = FALSE,
                             columnDefs = list(list(className = 'dt-right', targets = 1:4)),
                             language = list(info = "_START_ to _END_ of _TOTAL_ months",
                                             lengthMenu = "Show _MENU_ months"))) %>%
      formatCurrency(c("fixedPayment", "floatingPayment", "paymentDifference"),
                     digits = 2)
  })
  
  output$netFlowSummary <- renderText({
    totalNetImpact <- sum(swapData()$paymentDifference)
    paste("Total net impact: $", format(round(totalNetImpact, 2), big.mark = ","),
          ifelse(totalNetImpact >= 0,
                 "Positive value indicates net inflow.",
                 "Negative value indicates net outflow."))
  })
  
  output$swapStrategyPlot <- renderPlotly({
    p <- ggplot(swapData(), aes(x = date, y = paymentDifference)) +
      geom_line() +
      geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
      labs(title = "Profit/Loss Over Time from Swap Strategy",
           x = "Date", y = "P&L ($)",
           caption = "Positive = Profit, Negative = Loss") +
      theme_light() +
      scale_y_continuous(labels = scales::dollar)
    ggplotly(p)
  })
  
  savingsReactive <- reactive({
    vainilla_swap(
      input$notionalAmountVanilla,
      input$RateCurrent,
      input$RateAlter,
      input$BankFee,
      input$swapRateReceive
    )
  })
  
  output$swapDetails <- renderPrint({
    # Display swap details based on user inputs
    cat("Notional Amount: $", input$notionalAmountVanilla, "\n")
    cat("Current Rate: ", input$RateCurrent, "%\n")
    cat("Premium above SOFR: ", input$RateAlter, "%\n")
    cat("Receiving Swap Rate: ", input$swapRateReceive, "%\n")
    cat("Bank Fee: ", input$BankFee, "%\n")
  })
  
  output$swapCashFlows <- renderPlotly({
    # Generate cash flow plot based on user inputs
    cashFlows <- data.frame(
      Period = 1:12,
      FixedLeg = input$RateCurrent / 100 * input$notionalAmountVanilla / 12,
      FloatingLeg = rnorm(12, mean = 0.03, sd = 0.01) * input$notionalAmountVanilla / 12
    )
    cashFlows$NetCashFlow <- cashFlows$FloatingLeg - cashFlows$FixedLeg
    
    plot_ly(cashFlows, x = ~Period, y = ~FixedLeg, type = 'scatter', mode = 'lines', name = 'Fixed Leg') %>%
      add_trace(y = ~FloatingLeg, name = 'Floating Leg') %>%
      add_trace(y = ~NetCashFlow, name = 'Net Cash Flow') %>%
      layout(title = 'Swap Cash Flows', xaxis = list(title = 'Period'), yaxis = list(title = 'Cash Flow'))
  })
  
  output$breakEvenPoint <- renderPrint({
    # Calculate and display break-even point
    fixedRate <- input$RateCurrent / 100
    floatingRate <- input$RateAlter / 100
    breakEvenRate <- fixedRate - floatingRate
    
    cat("Break-even Floating Rate: ", sprintf("%.2f%%", breakEvenRate * 100), "\n")
    cat("If the floating rate exceeds this level, the swap will be profitable.")
  })
  
  output$riskMetrics <- renderPrint({
    # Compute and display risk metrics
    notional <- input$notionalAmountVanilla
    clientRateCurrent <- input$RateCurrent / 100
    clientRateAlter <- input$RateAlter / 100
    bankFee <- input$BankFee / 100
    swapRateReceive <- input$swapRateReceive / 100
    tenor_years <- 1 # Assuming a 1-year swap
    
    # Calculate present value of fixed and floating cash flows over the swap tenor
    pvFixed <- notional * clientRateCurrent / (1 + clientRateCurrent)
    pvFloating <- notional * (swapRateReceive + clientRateAlter) / (1 + clientRateAlter)
    pv <- pvFloating - pvFixed - notional * bankFee
    
    # Calculate DV01 assuming a linear approximation (without considering convexity)
    # Calculate DV01 as a negative of the change in the present value for a 1 basis point increase in the fixed rate
    one_basis_point = 0.0001
    pv_if_rate_increases <- notional * (clientRateCurrent + one_basis_point) / (1 + clientRateCurrent + one_basis_point)
    dv01 <- pv_if_rate_increases - pvFixed
    
    cat("Present Value (PV): $", formatC(pv, format = "f", big.mark = ",", digits = 2), "\n")
    cat("DV01 (Dollar Value of 1 basis point): $", formatC(dv01, format = "f", big.mark = ",", digits = 2), "\n")
  })
  
  output$comparativePlot <- renderPlotly({
    req(input$compareWith)
    # Generate comparative plot based on compareWith input
    vanillaSwapSavings <- savingsReactive()
    durationHedgeSavings <- calculate_duration_hedging(
      input$total_liability,
      input$bond_price,
      input$duration_liability,
      input$duration_bond,
      input$hedge_percent / 100
    )$total_cost
    
    if (input$compareWith == "Market Rates") {
      # Comparison with market rates
      marketRates <- data.frame(
        Rate = c("Current Rate", "SOFR", "Swap Rate"),
        Value = c(input$RateCurrent, input$RateAlter, input$swapRateReceive)
      )
      
      plot_ly(marketRates, x = ~Rate, y = ~Value, type = 'bar') %>%
        layout(title = 'Comparative Analysis - Market Rates',
               xaxis = list(title = 'Rate'),
               yaxis = list(title = 'Value (%)'))
    } else if (input$compareWith == "Other Strategies") {
      # Comparison between Vanilla Swap and Duration Hedge
      strategyComparison <- data.frame(
        Strategy = c("Vanilla Swap", "Duration Hedge"),
        Savings = c(vanillaSwapSavings, durationHedgeSavings)
      )
      
      plot_ly(strategyComparison, x = ~Strategy, y = ~Savings, type = 'bar') %>%
        layout(title = 'Comparative Analysis - Vanilla Swap vs Duration Hedge Savings',
               xaxis = list(title = 'Strategy'),
               yaxis = list(title = 'Savings'))
    }
  })
  
  
  output$vanillaSwapResults <- renderUI({
    savings <- savingsReactive()
    formatted_savings <- formatC(savings, format = "f", big.mark = ",", digits = 0)
    HTML(paste("<div style='font-size:19.5px;'>",
               "<strong>Vanilla Swap Total Savings:</strong> <strong>$",formatted_savings, "</strong>",
               "</div>"))
  })
  
  
  
  
  hedgeResultsReactive <- reactive({
    duration_hedging(
      input$faceValue,
      input$couponRate / 100,
      input$currentYTM / 100,
      input$maturity,
      input$hedgeYTM / 100
    )
  })
  
  output$hedge_results <- renderPrint({
    results <- calculate_duration_hedging(
      input$total_liability,
      input$bond_price,
      input$duration_liability,
      input$duration_bond,
      input$hedge_percent / 100
    )
    cat("Duration of Assets Coming In: ", results$duration_assets_in, " years\n",
        "Duration of Liability Going Out: ", results$duration_liability_out, " years\n",
        "Number of Asset Contracts to Purchase: ", results$num_contracts, "\n",
        "Total Cost: $", formatC(results$total_cost, format = "f", big.mark = ",", digits = 2), "\n")
  })
  
  output$hedge_analysis_plot <- renderPlotly({
    results <- calculate_duration_hedging(
      input$total_liability,
      input$bond_price,
      input$duration_liability,
      input$duration_bond,
      input$hedge_percent / 100
    )
    
    data <- data.frame(
      Category = c("Duration of Assets In", "Duration of Liability Out"),
      Value = c(results$duration_assets_in, results$duration_liability_out)
    )
    
    plot_ly(data, x = ~Category, y = ~Value, type = 'bar', marker = list(color = 'rgb(158,202,225)', line = list(color = 'rgb(8,48,107)', width = 1.5))) %>%
      layout(title = "Duration Hedging Analysis", yaxis = list(title = "Years"))
  })
  
}