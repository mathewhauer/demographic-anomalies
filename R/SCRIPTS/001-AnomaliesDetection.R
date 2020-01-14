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

for(i in 1:length(unique(countylist))){

  tryCatch({
    df2<- setNames(data.frame(matrix(ncol = 5, nrow = 0)), c("type", "ind", "time", "coefhat", "tstat"))
    dat2 <- filter(dat, location == paste0(countylist[i]))
    dat3 <- ts(dat2$Target, start = year(min(dat$Year)), end = c(year(max(dat$Year)),12), frequency = 12)
    outlier.county <- tsoutliers::tso(dat3,types = c("AO","LS","TC"),cval = sigma, maxit.iloop=10)
    
    # df_sums2 <- data.frame(type = NA, tot = NA, abstot = NA, location = NA)
    df_sums2 <- data.frame(y = NA, yadj=NA)
    if(!is.null(outlier.county$times)){
      
      df2<- outlier.county$outliers
      # df_sums2 <- data.frame(y=as.matrix(outlier.county$y), 
      #                 yadj = as.matrix(outlier.county$yadj),
      #                 month = as.Date(time(outlier.county$y))) %>%
      #   mutate(ind = as.numeric(rownames(.)),
      #          diff = y - yadj) %>%
      #   left_join(., outlier.county$outliers) %>%
      #   filter(diff != 0) %>%
      #   fill(type) 
        # group_by(type) %>%
        # dplyr::summarise(tot = sum(diff),
        #                  abstot = sum(abs(diff)))
      
      # df_sums2$location <- paste0(countylist[i])
      df_sums2$y <- sum(outlier.county$y)
      df_sums2$yadj <- sum(outlier.county$yadj)
      
    } else {
      df2 <- rbind(df2, data.frame(type = NA,
                                   ind = NA,
                                   time = NA,
                                   coefhat = NA,
                                   tstat = NA))
      # df_sums2 <- rbind(df_sums2)
      df_sums2 <- rbind(df_sums2, data.frame(y=NA, yadj=NA))
      
    }
    
    # df2$location <- paste0(countylist[i])
    # df_sums2$location <- paste0(countylist[i])
    # df <- bind_rows(df, df2)
    # df_sums <- bind_rows(df_sums, df_sums2)
    df2$location <- paste0(countylist[i])
    df_sums2$location <- paste0(countylist[i])
    df <- bind_rows(df, df2)
    df_sums <- bind_rows(df_sums, df_sums2)
  }
  , error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
}

df_sums2 <- df_sums %>%
  mutate(excessmort = y - yadj,
         excessmortper = (excessmort/y)*100)

write_rds(df, "./R/DATA-PROCESSED/fertility_anomalies")
write_rds(df_sums2, "./R/DATA-PROCESSED/fertility_anomalies_sums")

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
df_sums <- data.frame()
sigma <- 3.5

for(i in 1:length(unique(countylist))){
  set.seed(1)
  tryCatch({
    df2<- setNames(data.frame(matrix(ncol = 5, nrow = 0)), c("type", "ind", "time", "coefhat", "tstat"))
    dat2 <- filter(dat, location == paste0(countylist[i]))
    dat3 <- ts(dat2$Target, start = year(min(dat$Year)), end = c(year(max(dat$Year)),12), frequency = 12)
    outlier.county <- tsoutliers::tso(dat3,types = c("AO","LS","TC"),cval = sigma, maxit.iloop=10)
    
    df_sums2 <- data.frame(type = NA, tot = NA, abstot = NA, location = NA)
    if(!is.null(outlier.county$times)){
      
      df2<- outlier.county$outliers
      df_sums2 <- data.frame(y=as.matrix(outlier.county$y), 
                             yadj = as.matrix(outlier.county$yadj),
                             month = as.Date(time(outlier.county$y))) %>%
        mutate(ind = as.numeric(rownames(.)),
               diff = y - yadj) %>%
        left_join(., outlier.county$outliers) %>%
        filter(diff != 0) %>%
        fill(type) %>%
        group_by(type) %>%
        dplyr::summarise(tot = sum(diff),
                         abstot = sum(abs(diff)))
      
      df_sums2$location <- paste0(countylist[i])
      
    } else {
      df2 <- rbind(df2, data.frame(type = NA,
                                   ind = NA,
                                   time = NA,
                                   coefhat = NA,
                                   tstat = NA))
      df_sums2 <- rbind(df_sums2)
      
    }
    
    df2$location <- paste0(countylist[i])
    df_sums2$location <- paste0(countylist[i])
    df <- bind_rows(df, df2)
    df_sums <- bind_rows(df_sums, df_sums2)
  }
  , error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
}
write_rds(df, "./R/DATA-PROCESSED/mortality_anomalies")
write_rds(df_sums, "./R/DATA-PROCESSED/mortality_anomalies_sum")

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