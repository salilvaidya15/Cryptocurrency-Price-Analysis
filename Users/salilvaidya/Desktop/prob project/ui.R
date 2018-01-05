if(!require(shiny)){
  install.packages("shiny")
  library(shiny)
}
if(!require(shinythemes)){
  install.packages("shinythemes")
  library(shinythemes)
}
if(!require(plotly)){
  install.packages("plotly")
  library(plotly)
}
#create drop down list elements
hlc <-
  c("High",
    "Low",
    "Close")
coins <-
  c(
    "BTC",
    "ETH",
    "XRP",
    "LTC"
  )

indices <- 
  c(
    "SHClose",
    "SPClose",
    "NKClose",
    "MXClose",
    "USDJPYClose",
    "USDRUBClose",
    "USDCNYClose"
  )

#create the layout of the UI
#here we are using the navbar layout
navbarPage(
  "Cryptocurrency Correlation vs Stock Indexes Dashboard",
  #apply theme
  theme = shinytheme("sandstone"),
  #create tab for single currency analysis
  tabPanel("Single Cryptocurrency Analysis",
           sidebarLayout(
             #create sidebar elements
             sidebarPanel(
               selectInput(
                 "coin",
                 label = h3("Cryptocurrency"),
                 choices = coins
               ),
               selectInput(
                 "hlc",
                 label = h3("High-Low-Close"),
                 choices = hlc
               ),
               sliderInput(
                 "bins",
                 label = "Bins",
                 min = 1,
                 max = 20,
                 value = 10,
                 step = 1
               ),
               sliderInput(
                 "ci",
                 label = "Confidence Interval",
                 min = 0,
                 max = 100,
                 value = 20,
                 step = 5
               ),
               #show the confidence intervals
               h4('Confidence Interval of Mean'),
               verbatimTextOutput("cim"),
               h4('Confidence Interval of Variance'),
               verbatimTextOutput("civ")
             ),
             mainPanel(tabsetPanel(
               #create tab for histogram
               tabPanel(
                 "Histogram & Normal Distribution",
                 h4('Histogram & Normal Distribution'),
                 plotlyOutput("plot", height = 500),
                 #h4('Q-Q Plot'),
                 plotlyOutput("qqplot", height = 500)
               ),
               #create tab for regression on time
               tabPanel(
                 "Regressing returns on time",
                 h4('Regression Plot'),
                 plotlyOutput("regression", height = 500),
                 column(4,
                        h4("Intercept Estimate"),
                        verbatimTextOutput("intercept")),
                 column(4,
                        h4("Slope Estimate"),
                        verbatimTextOutput("slope")),
                 column(4,
                        h4("R-square"),
                        verbatimTextOutput("r2")),
                 h4('Residual Plot'),
                 plotlyOutput("residue1", height = 500)
               )
             ))
           )),
  #create main tab for dual currency analysis
  tabPanel("Dual Cryptocurrency Analysis",
           sidebarLayout(
             sidebarPanel(
               selectInput(
                 "currency1",
                 label = h3("First CryptoCurrency pair"),
                 choices = coins
               ),
               selectInput(
                 "currency2",
                 label = h3("Second Cryprocurrency pair"),
                 choices = coins,
                 selected = coins[2]
               ),
               sliderInput(
                 "ci2",
                 label = "Confidence Interval for t-test",
                 min = 0,
                 max = 100,
                 value = 20,
                 step = 5
               ),
               h4("T-test for equality of population means"),
               verbatimTextOutput("ttest")
             ),
             mainPanel(
               h4('Regressing first cryptocurrency returns on second'),
               plotlyOutput("regression2", height = 500),
               column(4,
                      h4("Intercept Estimate"),
                      verbatimTextOutput("intercept2")),
               column(4,
                      h4("Slope Estimate"),
                      verbatimTextOutput("slope2")),
               column(4,
                      h4("R-square"),
                      verbatimTextOutput("r22")),
               h4('Residual Plot'),
               plotlyOutput("residue2", height = 500)
             )
           )),
  #last tab for correlation analysis between cryptocurrency 
  tabPanel("Correlation between Cryptocurrencies and Major Stock Indices",
           sidebarLayout(
             sidebarPanel(
               selectInput(
                 "currency3",
                 label = h3("Cyptocurrency"),
                 choices = coins
               ),
               selectInput(
                 "StockIndices/Currency Pairs",
                 label = h3("IndicesorPairs"),
                 choices = indices
               ),
               h4('Coorelation'),
               h5('S&P 500'),
               verbatimTextOutput("corrm1"),
               h5('Shanghai Composite Index'),
               verbatimTextOutput("corrm2"),
               h5('Nikkei 225 Index'),
               verbatimTextOutput("corrm3"),
               h5('MOEX Russian Stock Index'),
               verbatimTextOutput("corrm4"),
               h5('USD/JPY Currency Pair'),
               verbatimTextOutput("corrm5"),
               h5('USD/RUB Currency Pair'),
               verbatimTextOutput("corrm6"),
               h5('USD/CNY Currency Pair'),
               verbatimTextOutput("corrm7"),
               
               h4(
                 "Our aim is to verify if the prices of major stock indices and their currency pairs with respect to USD are related to cryptocurrency changes:"
               ),
               
               h4(
                 "To check the validity of this claim, we are looking for correaltion between the same."
               ),
               h4(
                 "
                 We also try to estimate the value of the Cryptocurrency by regressing on the chosen Indices or Pairs value."
               )
               ), 
             mainPanel(
               h4('Regression Plot'),
               plotlyOutput("regressionm", height = 500),
               column(4,
                      h4("Intercept Estimate"),
                      verbatimTextOutput("interceptm")),
               column(4,
                      h4("Slope Estimate"),
                      verbatimTextOutput("slopem")),
               column(4,
                      h4("R - square"),
                      verbatimTextOutput("r2m")),
               h4('Residual Plot'),
               plotlyOutput("residuem", height = 500)
             )
             ))
  )
