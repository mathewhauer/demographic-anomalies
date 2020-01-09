

dat <- 
 ###MORTALITY 
  # read_tsv("DATA/Mortality/Underlying Cause of Death, 1999-2017,AL-CT.txt") %>%
  read_tsv("DATA/Mortality/Underlying Cause of Death, 1999-2017,states.txt") %>%
  mutate(Month2 = substr(`Month Code`,6,7),
         Month3 = as.yearmon(paste0(Year,"-", Month2)),
         # location = paste0("X", `County Code`)
         location = paste0("X", `State Code`)
         ) %>%
  dplyr::select(Year = Month3, location, Target = Deaths) %>%
  na.omit %>%
  arrange(location, Year)

dat <-
  ###FERTILITY
  bind_rows(read_tsv("DATA/Fertility/Natality, 2003-2006,states.txt"),
    read_tsv("DATA/Fertility/Natality, 2007-2017,states.txt"))%>%
  mutate(Month2 = str_pad(`Month Code`,2 , pad = "0"),
         Month3 = as.yearmon(paste0(Year,"-", Month2)),
         location = paste0("X", `State Code`)
  ) %>%
  dplyr::select(Year = Month3, location, Target = Births) %>%
  na.omit %>%
  arrange(location, Year)



# timelength <- year(max(dat$Year)) - year(min(dat$Year))+1

# time.points <- seq.Date(as.Date(min(dat$Year)), by ="years", length.out = timelength )
# 
# dat<- spread(dat, location, Deaths) 

countylist = unique(dat$location)[1]

df <- data.frame()
df_sums <- data.frame()
sigma <- 6

for(i in 1:length(unique(countylist))){
  tryCatch({
df2<- setNames(data.frame(matrix(ncol = 5, nrow = 0)), c("type", "ind", "time", "coefhat", "tstat"))
dat2 <- filter(dat, location == paste0(countylist[i]))
dat3 <- ts(dat2$Target, start = year(min(dat$Year)), end = c(year(max(dat$Year)),12), frequency = 12)
outlier.county <- tsoutliers::tso(dat3,types = c("AO","LS","TC"),maxit.iloop=10)

 df_sums2 <- data.frame(y = NA, yadj=NA)
if(!is.null(outlier.county$times)){
 
  df2<- outlier.county$outliers
  df_sums2$y <- sum(outlier.county$y)
  df_sums2$yadj <- sum(outlier.county$yadj)
  } else {
   df2 <- rbind(df2, data.frame(type = NA,
                                                          ind = NA,
                                                          time = NA,
                                                          coefhat = NA,
                                                          tstat = NA))
   df_sums2 <- rbind(df_sums2, data.frame(y=NA, yadj=NA))
   
  }

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

#min
dat2 <- filter(dat, location == paste0(filter(df, coefhat == min(coefhat, na.rm = T))$location))
dat3 <- ts(dat2$Target, start = year(min(dat$Year)), end = c(year(max(dat$Year)),12), frequency = 12)
plot(outlier.county <- tsoutliers::tso(dat3,types = c("AO","LS","TC"),maxit.iloop=10))


z<-tsoutliers::tso(dat3,types = c("AO","LS","TC"),maxit.iloop=10)

target <- "X36"

#max
dat2 <- filter(dat, location == paste0(filter(df, coefhat == max(coefhat, na.rm = T))$location))
dat2 <- filter(dat, location == "X37")

dat3 <- ts(dat2$Target, start = year(min(dat$Year)), end = c(year(max(dat$Year)),12), frequency = 12)
plot(outlier.county <- tsoutliers::tso(dat3,types = c("AO","LS","TC"),maxit.iloop=10))
title(main = "North Carolina - Monthly Births",
      sub = "+58k expected births")

plot(dat3)

statelist <- unique(df_sums2$location[which(!is.na(df_sums2$y))])

pdf("myOut.pdf")
r
for(this.state in statelist){
  dat2 <- filter(dat, location == this.state)
  d3 <- filter(df_sums2, location == this.state)
  dat3 <- ts(dat2$Target, start = year(min(dat$Year)), end = c(year(max(dat$Year)),12), frequency = 12)
  plot(outlier.county <- tsoutliers::tso(dat3,types = c("AO","LS","TC"),maxit.iloop=10))
  title(main = paste(d3$location,"- Monthly Births"),
        sub = paste0(format(d3$excessmort, digits =0, big.mark = ","),
                            " expected births"))
}

dev.off()