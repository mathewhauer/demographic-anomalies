###------Simulation Power-----
## @knitr SimulationPower

set.seed(1)
results_noutliers <- data.frame()
for(i in 1:1000){
  tryCatch({
    y <- arima.sim(model = list(ar = 0.7, ma = -0.4), n = 180)
    df2<- tso(y, cval = 3.5)$outliers
    df2$iteration <- i
    results_noutliers <- bind_rows(results_noutliers, df2)
  } , error=function(e){})
}

results_AO <- data.frame()
for(i in 1:1000){
  tryCatch({
    y <- arima.sim(model = list(ar = 0.7, ma = -0.4), n = 180)
    y[150] <- 4
    df2<- tso(y, cval = 3.5)$outliers
    df2$iteration <- i
    results_AO <- bind_rows(results_AO, df2)
  } , error=function(e){})
}

# False negative rate
nrow(results_noutliers) / (180*1000)
# Statistical power
nrow(results_AO[which(results_AO$time == 150),])