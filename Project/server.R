function(input, output, session){
  #compute the bin sizes from the number of bins
  compute_bins <- function(x, n) {
    list(
      start = min(x),
      end = max(x),
      size = (max(x) - min(x)) / n
    )
  }
  
  #read the crypto csv
  crypto <- read.csv("crypto_final_usd_1.csv")
  # change in to date format
  crypto$Date <- as.Date(crypto$Date, format = "%m/%d/%y")
  #omit the na values
  crypto <- na.omit(crypto)
  
  #read the coin name
  coin <- reactive({
    paste(input$coin, input$hlc, sep = ".")
  })
  
  #calculate the returns
  returns <-
    reactive({
      log(crypto[, eval(coin())][-1] / crypto[, eval(coin())][-length(crypto[, eval(coin())])])
    })
  n <- reactive({
    length(returns())
  })
  #calculate the upper limit for the mean CI
  ci <- reactive({
    qt(input$ci / 200 + 0.5, n() - 1) * sd(returns()) / sqrt(n())
  })
  #upper limit for var CI
  up <- reactive({
    qchisq((1 - (100 - input$ci) / 200), n() - 1)
  })
  #lower limit for the var CI
  down <- reactive({
    qchisq((100 - input$ci) / 200, n() - 1)
  })
  
  #qq
  
  
  
  #create the histogram and plot a normal dist
  output$plot <- renderPlotly({
    x <- returns()
    fit <- dnorm(x, mean = mean(x), sd = sd(x))
    xbins <- compute_bins(x, input$bins)
    plot_ly(x = x) %>%
      add_histogram(xbins = xbins, name = "Histogram") %>%
      add_lines(y = fit,
                color = "#C9EFF9",
                name = "Normal") %>%
      layout(yaxis = list(overlaying = "y", side = "left"))
  })
  
  #calculate the CI for mean
  output$cim <- renderPrint({
    as.symbol(paste("(",
                    mean(returns()) - ci(),
                    ",",
                    mean(returns()) + ci(),
                    ")"))
  })
  #calculate the CI for var
  output$civ <- renderPrint({
    as.symbol(paste(
      "(",
      var(returns()) * (n() - 1) / down(),
      ",",
      var(returns()) * (n() - 1) / up(),
      ")"
    ))
  })
  
  #regression wrt time
  regtime <- reactive({
    dates <- crypto$Date[2:length(crypto$Date)]
    lm(returns() ~ dates)
  })
  
  #calculate the intercept
  output$intercept <-
    renderPrint(as.symbol(summary(regtime())$coefficients[1]))
  
  #calculate the slope
  output$slope <-
    renderPrint(as.symbol(summary(regtime())$coefficients[2]))
  
  #finding the r-square
  output$r2 <- renderPrint(as.symbol(summary(regtime())$r.squared))
  
  #plot the residual graph
  output$residue1 <- renderPlotly({
    dates <- crypto$Date[2:length(crypto$Date)]
    fit <- density(summary(regtime())$residuals)
    plot_ly(x = ~ dates) %>%
      add_lines(
        x = fit$x,
        y = fit$y,
        fill = "tozeroy",
        line = list(color = "#5E88FC")
      )
  })
  
  #plot regression
  output$regression <- renderPlotly({
    dates <- crypto$Date[2:length(crypto$Date)]
    regtime <- lm(returns() ~ dates)
    plot_ly(x = ~ dates) %>%
      add_markers(
        y = ~ returns(),
        showlegend = TRUE,
        name = "Actual Return",
        marker = list(size = 5,
                      color = 'rgba(0,0,255,1)')
      ) %>%
      add_lines(
        y =  ~ fitted(regtime),
        line = list(color = '#FF00FF'),
        name = "Estimated Return",
        showlegend = TRUE
      ) %>%
      layout(
        yaxis = list(zeroline = TRUE, title = "Log Returns"),
        xaxis = list(zeroline = TRUE, title = "Date")
      )
  })
  
  #read crypto currency 1
  currency1 <- reactive({
    paste(input$currency1, "Close", sep = ".")
  })
  #read cryptocurrency 2
  currency2 <- reactive({
    paste(input$currency2, "Close", sep = ".")
  })
  #calculate return for currency 1
  returns1 <-
    reactive({
      log(crypto[, eval(currency1())][-1] / crypto[, eval(currency1())][-length(crypto[, eval(currency1())])])
    })
  #calculate return for currency 2
  returns2 <-
    reactive({
      log(crypto[, eval(currency2())][-1] / crypto[, eval(currency2())][-length(crypto[, eval(currency2())])])
    })
  
  #perform the t test
  output$ttest <- renderPrint({
    p <- t.test(returns1(), returns2())$p.val
    if (p > (1 - (input$ci2 / 100))) {
      as.symbol("Means are Equal")
    }
    else{
      as.symbol("Means are Unequal")
    }
  })
  
  #regress 2nd returns on the first
  reg2 <- reactive({
    lm(returns1() ~ returns2())
  })
  
  #calculate the intercept
  output$intercept2 <-
    renderPrint(as.symbol(summary(reg2())$coefficients[1]))
  
  #slope
  output$slope2 <-
    renderPrint(as.symbol(summary(reg2())$coefficients[2]))
  
  #rsquared
  output$r22 <- renderPrint(as.symbol(summary(reg2())$r.squared))
  
  #residuals
  output$residue2 <- renderPlotly({
    fit <- density(summary(reg2())$residuals)
    plot_ly(x = ~ returns2()) %>%
      add_lines(
        x = fit$x,
        y = fit$y,
        fill = "tonexty",
        line = list(color = "#5E88FC")
      )
  })
  
  #plot the regression
  output$regression2 <- renderPlotly({
    reg2 <- lm(returns1() ~ returns2())
    plot_ly(x = ~ returns2()) %>%
      add_markers(
        y = ~ returns1(),
        showlegend = TRUE,
        name = "Actual Cryptocurrency 2 Return",
        marker = list(size = 5,
                      color = 'rgba(0,0,255,1)')
      ) %>%
      add_lines(
        y =  ~ fitted(reg2),
        line = list(color = '#07A4B5'),
        name = "Estimated Cryptocurrency 2 Return",
        showlegend = TRUE
      ) %>%
      layout(
        yaxis = list(zeroline = TRUE, title = "1st Cryptocurrency Returns"),
        xaxis = list(zeroline = TRUE, title = "2nd Cryptocurrency Returns")
      )
  })
  
  ################3rd part of the code
  
  currency3 <- reactive({
    paste(input$currency3, "Close", sep = ".")
  })
  
  indices <- reactive({
    paste(input$`StockIndices/Currency Pairs`)
  })
  
  #regression on SPClose value
  regm <- reactive({
    lm(crypto[, eval(currency3())] ~ crypto[, indices()])
  })
  
  
  #calculate the corelation
  output$corrm1 <-
    renderPrint(as.symbol(cor(crypto[, eval(currency3())], crypto[, "SPClose"])))
  output$corrm2 <-
    renderPrint(as.symbol(cor(crypto[, eval(currency3())], crypto[, "SHClose"])))
  output$corrm3 <-
    renderPrint(as.symbol(cor(crypto[, eval(currency3())], crypto[, "NKClose"])))
  output$corrm4 <-
    renderPrint(as.symbol(cor(crypto[, eval(currency3())], crypto[, "MXClose"])))
  output$corrm5 <-
    renderPrint(as.symbol(cor(crypto[, eval(currency3())], crypto[, "USDJPYClose"])))
  output$corrm6 <-
    renderPrint(as.symbol(cor(crypto[, eval(currency3())], crypto[, "USDRUBClose"])))
  output$corrm7 <-
    renderPrint(as.symbol(cor(crypto[, eval(currency3())], crypto[, "USDCNYClose"])))
  
  output$interceptm <-
    renderPrint(as.symbol(summary(regm())$coefficients[1]))
  output$slopem <-
    renderPrint(as.symbol(summary(regm())$coefficients[2]))
  output$r2m <- renderPrint(as.symbol(summary(regm())$r.squared))
  
  output$residuem <- renderPlotly({
    fit <- density(summary(regm())$residuals)
    plot_ly(x = ~ crypto[, eval(indices())]) %>%
      add_lines(
        x = fit$x,
        y = fit$y,
        fill = "tozeroy",
        line = list(color = "#5E88FC")
      )
  })
  
  output$regressionm <- renderPlotly({
    regm <- lm(crypto[, eval(currency3())] ~ crypto[, eval(indices())])
    plot_ly(x = ~ crypto[, eval(indices())]) %>%
      add_markers(
        y = ~ crypto[, eval(currency3())],
        showlegend = TRUE,
        name = "Actual Cryptocurrency Value",
        marker = list(size = 5,
                      color = 'rgba(255, 100, 193, 1)')
      ) %>%
      add_lines(
        y =  ~ fitted(regm),
        line = list(color = '#07A4B5'),
        name = "Estimated Cryptocurrency Value",
        showlegend = TRUE
      ) %>%
      layout(
        yaxis = list(zeroline = TRUE, title = "Cryptocurrency Value"),
        xaxis = list(zeroline = TRUE, title = "Indices or Pair Value")
      )
  })
}
