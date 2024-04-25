library(fredr)
library(zoo)
library(ggplot2)
library(data.table)
library(shiny)
library(xts)
library(DT)
library(plotly)
library(memoise)

fluidPage(
  
  tags$head(
    tags$style(HTML("
      .note {
        display: inline-block;
        vertical-align: top;
        margin-left: 20px;
        margin-top: 5px;
        font-style: italic;
      }
      .entries-info {
        align-self: center;
      }
      .flex-container {
        display: flex;
        justify-content: space-between;
        align-items: center;
      }
    "))
  ),
  titlePanel("Interest Rate Hedging Strategy Analysis"),
  tabsetPanel(
    tabPanel("Introduction",
             h3("Welcome to the Interest Rate Hedging Strategy Analysis App"),
             p("This app allows you to explore and compare different interest rate hedging strategies."),
             p("The available strategies are:"),
             tags$ul(
               tags$li("Vanilla Swap: Exchange fixed interest rate payments for floating interest rate payments based on SOFR."),
               tags$li("Duration Hedging: Hedge interest rate risk by matching the duration of assets and liabilities.")
             ),
             tags$p(tags$b("Here are two scenario examples illustrating how our app can be utilized for hedging strategies:")),
             tags$p(tags$b("Vainilla Swap Strategy:"), "Imagine a small business has taken out a $500,000 variable rate loan tied to the SOFR (Secured Overnight Financing Rate), which is currently at 1.5% annually. The business owner anticipates that the interest rates might rise over the next year, which could increase their loan payments. To hedge against this risk, they decide to use our app to enter into a swap agreement where they pay a fixed rate of 2% annually while receiving the variable SOFR rate. By using our app, they lock in their total interest expenses, ensuring predictable payments even if the SOFR increases beyond 2%. This hedge is particularly beneficial for budgeting and financial planning in uncertain economic times."),
             tags$p(tags$b("Duration Hedging Strategy:"), "Consider a pension fund that has liabilities with an average duration of 12 years. The current assets they hold have a shorter average duration of 7 years. To reduce the risk of interest rate changes affecting their ability to meet these liabilities, they use our app to calculate a duration hedging strategy. The app advises purchasing additional government bonds to increase the asset duration. The pension fund inputs their total liabilities, current bond prices, and the proportion of liabilities they wish to hedge. Our app calculates that they need to purchase bonds worth $20 million to balance the duration of assets with their liabilities. This strategy helps the pension fund stabilize the value of their fund relative to their obligations, making it less sensitive to interest rate fluctuations."),
             h4("To get started, select a strategy from the 'Strategy Analysis' tab and adjust the input parameters to see the results."),
             
    ),
    tabPanel("Strategy Analysis",
             sidebarLayout(
               sidebarPanel(
                 selectInput("strategySelection", "Select Strategy:",
                             choices = c("Vanilla Swap", "Duration Hedging", "Comparative Analysis")),
                 conditionalPanel(
                   condition = "input.strategySelection == 'Vanilla Swap'",
                   sliderInput("notionalAmountVanilla", "Notional Amount:",
                               min = 1e5, max = 1e7, value = 1e6,
                               step = 1e5, pre = "$", sep = ","),
                   sliderInput("RateCurrent", "Current Rate (%):",
                               min = 0.5, max = 10,
                               value = 3, step = 0.1, post = "%"),
                   sliderInput("RateAlter", "Premium above SOFR (%):",
                               min = 0.1, max = 5,
                               value = 3, step = 0.1, post = "%"),
                   sliderInput("swapRateReceive", "Receiving Swap Rate (%):",
                               min = 0.5, max = 10,
                               value = 3, step = 0.1, post = "%"),
                   sliderInput("BankFee", "Bank Fee (%):",
                               min = 0.5, max = 10,
                               value = 0.5, step = 0.1, post = "%")
                 ),
                 conditionalPanel(
                   condition = "input.strategySelection == 'Duration Hedging'",
                   sliderInput("total_liability", "Total Liability Value ($):", min = 100000, max = 10000000, value = 5000000),
                   sliderInput("bond_price", "Price of Bond/STRIP ($):", min = 1, max = 200, value = 1),
                   sliderInput("duration_liability", "Duration of Liability (Years):", min = 1, max = 30, value = 15),
                   sliderInput("duration_bond", "Duration of Bond/STRIP (Years):", min = 1, max = 30, value = 5),
                   sliderInput("hedge_percent", "Percentage of the Liability to Hedge (%):", min = 0, max = 100, value = 100, step = 1)
                 )
               ),
               
               mainPanel(
                 conditionalPanel(
                   condition = "input.strategySelection == 'Vanilla Swap'",
                   
                   tabsetPanel(
                     tabPanel("Vanilla Swap",
                              uiOutput("vanillaSwapResults"),
                              div(class = "note-container", 
                                  "Note: If saving is negative, it means both sides are losing. This swap is not suggested."
                              ),
                              fluidRow(
                                column(6,
                                       h4("Swap Details"),
                                       verbatimTextOutput("swapDetails"),
                                       h4("Swap Cash Flows"),
                                       plotlyOutput("swapCashFlows")
                                ),
                                
                                column(6,
                                       h4("Break-even Analysis"),
                                       div(verbatimTextOutput("breakEvenPoint"), class = "break-even-analysis"),
                                       h4("Risk Metrics"),
                                       verbatimTextOutput("riskMetrics")
                                )
                              ),
                              
                              tags$style(HTML("
                           .break-even-analysis {
                             word-wrap: break-word;
                             white-space: normal;
                           }
                         "))
                     ),
                     tabPanel("Cash Flow Chart and Table",
                              sliderInput("fixedRate", "Fixed Rate (%):",
                                          min = 0.5, max = 10,
                                          value = 3, step = 0.1, post = "%"),
                              verbatimTextOutput("netFlowSummary"),
                              plotlyOutput("swapStrategyPlot"),
                              DTOutput("swapStrategyResults")
                     )
                   )
                 ),
                 conditionalPanel(
                   condition = "input.strategySelection == 'Duration Hedging'",
                   verbatimTextOutput("hedge_results"),
                   plotlyOutput("hedge_analysis_plot")
                 ),
                 conditionalPanel(
                   condition = "input.strategySelection == 'Comparative Analysis'",
                   h4("Comparative Analysis"),
                   selectInput("compareWith", "Compare With:",
                               choices = c("Market Rates", "Other Strategies")),
                   plotlyOutput("comparativePlot")
                   
                 )
               )
             )
    )
  )
)
