getSalesBySubLevel <-
  function(data, parentcat, subcategory, forMonth) {
    parentlist <-  unique(data[, parentcat])
    
    filename <- paste(parentcat, "_", subcategory, ".csv")
    colname = c("Month 1",
                "Month 2",
                "Month 3",
                "Month 4",
                "Month 5",
                "Month 6",
                subcategory,
                parentcat)
    
    fmm <- data.frame(
      month1 = c(1),
      month2 = c(1),
      month3 = c(1),
      month4 = c(1),
      month5 = c(1),
      month6 = c(1)
    )
    
    fmm2 <- data.frame(
      month1 = c(1),
      month2 = c(1),
      month3 = c(1),
      month4 = c(1),
      month5 = c(1),
      month6 = c(1),
      subcategory = c(""),
      warehouse = c(""), stringsAsFactors = FALSE
    )
    
    for (k in 1:length(parentlist)) {
      print(parentlist[k])
      fmm <- fmm[0, ]
      rodw <- data[data[, parentcat] == parentlist[k], ]
      subcategorylist     <-  unique(rodw[, subcategory])
      
      for (i in 1:length(subcategorylist)) {
        brnd <- subcategorylist[i]
        print(brnd)
        rodb <- rodw[rodw[, subcategory] == subcategorylist[i], ]
        rodbagg <-
          aggregate(rodb$`Retail Quantity`,
                    by = list(date = rodb$date),
                    FUN = sum)
        ts2 <- ts(rodbagg[, -1],
                  frequency = 12,
                  start = min(rodbagg$date))
        tsc <- tsclean(ts2)
        
        if(length(tsc) >7){
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
        
        fcast <- forecast(ar.fit, h = forMonth)
        plot(fcast, main = paste("Forecast for ", brnd, " in ", as.character(parentlist[k])))
        
        fmm[i,]                    <- as.list(round(t(as.data.frame(fcast$mean)), digits = 0))
        fmm$subcategory[i]         <- as.character(subcategorylist[i])
        fmm$warehouse[i]           <- as.character(parentlist[k])
      }
      
      if (is.na(fmm2)) {
        fmm2 <- fmm
      } else {
        fmm2 <- rbind(fmm2, fmm)
      }
      
    }
    
    colnames(fmm2) <- colname
    
    fmm2 <- sapply(fmm2, function(x){replace(x, x <0,0)})    
    write.csv(fmm2[-1,], file = filename, row.names = FALSE)
    
    print("Forecasting has been completed successfully.")
  }
