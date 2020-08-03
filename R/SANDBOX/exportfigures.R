
dat_fert <-
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

dat_mort <- 
  ###MORTALITY 
  read_tsv("./R/DATA-RAW/Mortality/Underlying Cause of Death, 1999-2017,states.txt") %>%
  mutate(Month2 = substr(`Month Code`,6,7),
         Month3 = as.yearmon(paste0(Year,"-", Month2)),
         # location = paste0("X", `County Code`)
         location = paste0("X", `State Code`)
  ) %>%
  dplyr::select(Year = Month3, location, Target = Deaths) %>%
  na.omit %>%
  arrange(location, Year)

refstates <- read_csv("./R/DATA-RAW/ref_states.csv") %>%
  mutate(loc = paste0("X", str_pad(loc, 2, pad = "0")),
         location = loc)
  

set.seed(1)

fertility_anomalies <- read_rds( "./R/DATA-PROCESSED/fertility_anomalies")%>%
  left_join(., refstates)
df_sums2 <- read_rds("./R/DATA-PROCESSED/fertility_anomalies_sums") %>%
  left_join(., refstates)


lista <- unique(fertility_anomalies$location)
pdf(file = "fertility_anomalies.pdf",
    width=11, height=8.5)

for(this.state in lista){
  dat2 <- filter(dat_fert, location == this.state)
  d3 <- filter(df_sums2, loc == this.state)
  dat3 <- ts(dat2$Target, start = year(min(dat2$Year)), end = c(year(max(dat2$Year)),12), frequency = 12)
  outlier.county <- tsoutliers::tso(dat3,types = c("AO","LS","TC"), cval = 3.5, maxit.iloop=10)
  a <-  function() {
    plot(outlier.county)
    title(main = paste(d3$STATE_DESCR,"- Monthly Births"),
          sub = paste0(format(d3$ydiff, digits =0, big.mark = ","),
                       " expected births"))
  }
  
print(ggdraw(a))
}

dev.off()

mortality_anomalies <- read_rds( "./R/DATA-PROCESSED/mortality_anomalies")%>%
  left_join(., refstates)
df_sums2 <- read_rds("./R/DATA-PROCESSED/mortality_anomalies_sum") %>%
  left_join(., refstates)


lista <- unique(mortality_anomalies$location)
pdf(file = "mortality_anomalies.pdf",
    width=11, height=8.5)

for(this.state in lista){
  dat2 <- filter(dat_mort, location == this.state)
  d3 <- filter(df_sums2, loc == this.state)
  dat3 <- ts(dat2$Target, start = year(min(dat2$Year)), end = c(year(max(dat2$Year)),12), frequency = 12)
  outlier.county <- tsoutliers::tso(dat3,types = c("AO","LS","TC"), cval = 3.5, maxit.iloop=10)
  a <-  function() {
    plot(outlier.county)
    title(main = paste(d3$STATE_DESCR,"- Monthly Deaths"),
          sub = paste0(format(d3$ydiff, digits =0, big.mark = ","),
                       " expected deaths"))
  }
  
  print(ggdraw(a))
}

dev.off()