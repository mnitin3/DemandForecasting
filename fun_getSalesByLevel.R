getSalesByLevel <- function(data, category, forMonths) {
  categorylist <-  as.character(unique(data[, category]))
  filename <- paste(category, ".csv")
  colname = c("Month 1",
              "Month 2",
              "Month 3",
              "Month 4",
              "Month 5",
              "Month 6",
              category)
  
  
  fmm <- data.frame(
    month1 = c(1),
    month2 = c(1),
    month3 = c(1),
    month4 = c(1),
    month5 = c(1),
    month6 = c(1),
    cat    = c("a"),
    stringsAsFactors = FALSE
  )
  
  fmm[0, ]
  
  for (i in 1:length(categorylist)) {
    print(paste("Forecasting for", categorylist[i]))
    rodb <- data[data[, category] == categorylist[i], ]
    
    rodbagg <-
      aggregate(rodb$`Retail Quantity`,
                by = list(date = rodb$date),
                FUN = sum)
    ts2 <- ts(rodbagg[, -1],
              frequency = 12,
              start = min(rodbagg$date))
    
    tsc <- tsclean(ts2)
    
    if (length(tsc) > 7) {
      adf <- adf.test(tsc)
      
      if (adf$p.value > .05) {
        adf <- 1
        j = 1
        while (adf < 0.05) {
          aate  <-  diff(tsc, differences = j)
          j < j + 1
          adf <- adf.test(tsc)
        }
      }
    }
    
    ar.fit <- auto.arima(tsc, seasonal = TRUE)
    fcast <- forecast(ar.fit, h = forMonths)
    plot(fcast, main = paste("forecast for ", categorylist[i]))
    fmm[i, ] <-
      cbind(round(t(as.data.frame(fcast$mean)), digits = 0), categorylist[i])
  }
  
  fmm <- sapply(fmm, function(x) {
    replace(x, x < 0, 0)
  })
  write.csv(fmm,
            file = filename,
            row.names = FALSE,
            col.names = colname)
  
  print("Forecasting has been completed successfully.")
  
}
