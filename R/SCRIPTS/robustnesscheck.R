

countylist = unique(dat_fert$location)

noamom_fert <- setdiff(countylist, unique(anom_fert$location))

noamom_mort <- setdiff(countylist, unique(anom_mort$location))

# dat <- ts(dat$Target)
# fit <- tso(dat, cval=3.5)
# sdval <- sqrt(fit$fit$sigma2)
# arima.sim(n=length(fit$y), model=list(fit$fit), sd = sdval) + as.vector(fit$yadj)
# results_noutliers <- data.frame()
# y2 <- matrix(nrow = 1000, ncol=180)
for(this.state in noamom_mort){
  print(this.state)
  dat <- ts(filter(dat_mort, location == this.state)$Target)
  fit <- tso(dat, cval=3.5)
  sdval <- sqrt(fit$fit$sigma2)

for(i in 1:1000){
  tryCatch({
    # y <- arima.sim(model = list(ar = 0.7, ma = -0.4), n = 180)
    y <- arima.sim(n=length(fit$y), model=list(fit$fit), sd = sdval) + as.vector(fit$yadj)
    
    df2<- tso(y, cval = 3.5)$outliers
    df2$iteration <- i
    df2$type <- "mortality"
    results_noutliers <- bind_rows(results_noutliers, df2)
    # print(i)
  } , error=function(e){#cat("ERROR :",conditionMessage(e), "\n")
    })
}
}

for(this.state in noamom_fert){
  print(this.state)
  dat <- ts(filter(dat_fert, location == this.state)$Target)
  fit <- tso(dat, cval=3.5)
  sdval <- sqrt(fit$fit$sigma2)
  
  for(i in 1:1000){
    tryCatch({
      # y <- arima.sim(model = list(ar = 0.7, ma = -0.4), n = 180)
      y <- arima.sim(n=length(fit$y), model=list(fit$fit), sd = sdval) + as.vector(fit$yadj)
      
      df2<- tso(y, cval = 3.5)$outliers
      df2$iteration <- i
      df2$type <- "fertility"
      results_noutliers <- bind_rows(results_noutliers, df2)
      # print(i)
    } , error=function(e){#cat("ERROR :",conditionMessage(e), "\n")
    })
  }
}

z <- results_noutliers %>%
  group_by(type) %>%
  dplyr::summarise(tot = n()) %>%
  mutate(timesteps = if_else(type == "fertility", 180*1000*33, 228*1000*8)) %>%
  dplyr::summarise(tot = sum(tot),
                   timesteps = sum(timesteps)) %>%
  mutate(FNR = tot/timesteps)


results_AO <- data.frame()
for(this.state in noamom_mort){
  print(this.state)
  dat <- ts(filter(dat_mort, location == this.state)$Target)
  fit <- tso(dat, cval=3.5)
  sdval <- sqrt(fit$fit$sigma2)
for(i in 1:1000){
  tryCatch({
    y <- dat
   
    y[150] <- y[150]+3.5*sdval
  
    df2<- tso(y, cval = 3.5)$outliers
    df2$iteration <- i
    
    results_AO <- bind_rows(results_AO, df2)
    # print(i)
  } , error=function(e){#cat("ERROR :",conditionMessage(e), "\n")
    })
}
}

for(this.state in noamom_fert){
  print(this.state)
  dat <- ts(filter(dat_fert, location == this.state)$Target)
  fit <- tso(dat, cval=3.5)
  sdval <- sqrt(fit$fit$sigma2)
  for(i in 1:1000){
    tryCatch({
      y <- dat
  
      y[150] <- y[150] + 3.5*sdval

      
      df2<- tso(y, cval = 3.5)$outliers
      df2$iteration <- i
      results_AO <- bind_rows(results_AO, df2)
      # print(i)
    } , error=function(e){#cat("ERROR :",conditionMessage(e), "\n")
    })
  }
}

write.csv(results_noutliers, "./R/DATA-PROCESSED/robustness_nooutliers.csv")
write.csv(results_AO, "./R/DATA-PROCESSED/robustness_AO.csv")

  nrow(results_AO[which(results_AO$time == 150),]) / (41*1000)
  
  results_noutliers <- read_csv("./R/DATA-PROCESSED/robustness_nooutliers.csv")
  results_AO <-   read_csv("./R/DATA-PROCESSED/robustness_AO.csv")
