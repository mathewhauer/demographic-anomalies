###------Mort Loop-----
## @knitr MortLoop

dat <-
  ###FERTILITY
  bind_rows(read_tsv("./R/DATA-RAW/Fertility/Natality, 2003-2006,states.txt"),
            read_tsv("./R/DATA-RAW/Fertility/Natality, 2007-2017,states.txt"))%>%
  mutate(Month2 = str_pad(`Month Code`,2 , pad = "0"),
         Month3 = as.yearmon(paste0(Year,"-", Month2)),
         location = paste0("X", `State Code`)
  ) %>%
  dplyr::select(Year = Month3, location, Target = Births) %>%
  na.omit %>%
  arrange(location, Year)

countylist = unique(dat$location)

df <- data.frame()
df_sums <- data.frame()
sigma <- 3.5

for(this.state in unique(countylist)){
  set.seed(1)
  print(this.state)
  tryCatch({
    df2<- setNames(data.frame(matrix(ncol = 5, nrow = 0)), c("type", "ind", "time", "coefhat", "tstat"))
    
    state <- this.state
    dat2 <- filter(dat, location == state)
    dat3 <- ts(dat2$Target, start = year(min(dat$Year)), end = c(year(max(dat$Year)),12), frequency = 12)
    outlier.county <- tsoutliers::tso(dat3,types = c("AO","LS","TC"),cval = sigma, maxit.iloop=10)
    df2<- outlier.county$outliers
    df2$location <- state
    df <- bind_rows(df, df2)
    
    df_sums2 <- setNames(data.frame(matrix(ncol=3, nrow=1)), c("y", "yadj", "loc"))
    df_sums2$ydiff <- sum(outlier.county$effects)
    df_sums2$yadj <- sum(outlier.county$yadj)
    df_sums2$loc <- this.state
    
  } , error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
}

# df_sums2 <- df_sums %>%
#   mutate(excessmort = y - yadj,
#          excessmortper = (excessmort/y)*100)

write_rds(df, "./R/DATA-PROCESSED/fertility_anomalies")
# write_rds(df_sums2, "./R/DATA-PROCESSED/fertility_anomalies_sums")

dat <- 
  ###MORTALITY 
  # read_tsv("DATA/Mortality/Underlying Cause of Death, 1999-2017,AL-CT.txt") %>%
  read_tsv("./R/DATA-RAW/Mortality/Underlying Cause of Death, 1999-2017,states.txt") %>%
  mutate(Month2 = substr(`Month Code`,6,7),
         Month3 = as.yearmon(paste0(Year,"-", Month2)),
         # location = paste0("X", `County Code`)
         location = paste0("X", `State Code`)
  ) %>%
  dplyr::select(Year = Month3, location, Target = Deaths) %>%
  na.omit %>%
  arrange(location, Year)

countylist = unique(dat$location)

df <- data.frame()
sigma <- 3.5

for(this.state in unique(countylist)){
  set.seed(1)
  print(this.state)
  tryCatch({
    df2<- setNames(data.frame(matrix(ncol = 5, nrow = 0)), c("type", "ind", "time", "coefhat", "tstat"))
    state <- this.state
    dat2 <- filter(dat, location == state)
    dat3 <- ts(dat2$Target, start = year(min(dat$Year)), end = c(year(max(dat$Year)),12), frequency = 12)
    outlier.county <- tsoutliers::tso(dat3,types = c("AO","LS","TC"),cval = sigma, maxit.iloop=10)
    df2<- outlier.county$outliers
    df2$location <- state
    df <- bind_rows(df, df2)
  } , error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
}
write_rds(df, "./R/DATA-PROCESSED/mortality_anomalies")
# write_rds(df_sums, "./R/DATA-PROCESSED/mortality_anomalies_sum")

# 
# 
# 
# write_rds(outlier.county, "./R/DATA-PROCESSED/NewHampshire_outliers")
# plot(outlier.county <- tsoutliers::tso(dat3,types = c("AO","LS","TC"),maxit.iloop=10))
# title(main = "Kansas - Monthly Births")
# 
# 
# d <- read_rds("./R/DATA-PROCESSED/mortality_anomalies")
# 
# plot(d) 
# title(main = "New Hampshire - Monthly Deaths",
#       sub = "+9.7k expected deaths (+14%)")