###------Anomalies Detection-----
## @knitr AnomaliesDetection

# Reading the fertility data in
dat <-
  ###FERTILITY
  bind_rows(read_tsv("../R/DATA-RAW/Fertility/Natality, 2003-2006,states.txt"),
            read_tsv("../R/DATA-RAW/Fertility/Natality, 2007-2017,states.txt"))%>%
  mutate(Month2 = str_pad(`Month Code`,2 , pad = "0"),
         Month3 = as.yearmon(paste0(Year,"-", Month2)),
         location = paste0("X", `State Code`)
  ) %>%
  dplyr::select(Year = Month3, location, Target = Births) %>%
  na.omit %>%
  arrange(location, Year)

# Generating a list of States
countylist = unique(dat$location)

# Setting up the data frames to collect the results
df <- data.frame()
df_sums <- data.frame()
# Setting the t-stat level.
sigma <- 3.5
# Looping through the states. For illustrative purposes, this loops through the 1st 5 states.
for(this.state in unique(countylist)[1:5]){
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
    df_sums2$y <- sum(outlier.county$y)
    df_sums2$ydiff <- sum(outlier.county$effects)
    df_sums2$ydiffabs <- sum(abs(outlier.county$effects))
    df_sums2$yadj <- sum(outlier.county$yadj)
    df_sums2$loc <- this.state
    df_sums <- bind_rows(df_sums, df_sums2)
  } , error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

head(df)
head(df_sums)

# Writing the results to the hard drive
# write_rds(df, "../R/DATA-PROCESSED/fertility_anomalies")
# write_rds(df_sums, "../R/DATA-PROCESSED/fertility_anomalies_sums")

dat <- 
  ###MORTALITY 
  read_tsv("../R/DATA-RAW/Mortality/Underlying Cause of Death, 1999-2017,states.txt") %>%
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
df_sums <- data.frame()
sigma <- 3.5

for(this.state in unique(countylist)[1:5]){
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
    df_sums2$y <- sum(outlier.county$y)
    df_sums2$ydiff <- sum(outlier.county$effects)
    df_sums2$ydiffabs <- sum(abs(outlier.county$effects))
    df_sums2$yadj <- sum(outlier.county$yadj)
    df_sums2$loc <- this.state
    df_sums <- bind_rows(df_sums, df_sums2)
      } , error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}
head(df)
head(df_sums)
# write_rds(df, "../R/DATA-PROCESSED/mortality_anomalies")
# write_rds(df_sums, "../R/DATA-PROCESSED/mortality_anomalies_sum")