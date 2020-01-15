###------States Analysis  -----
## @knitr StatesAnalysis

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

set.seed(1)
# Mortality - New York
dat2 <- filter(dat_mort, location == "X36")
dat3 <- ts(dat2$Target, start = year(min(dat_mort$Year)), end = c(year(max(dat_mort$Year)),12), frequency = 12)
outlier.mort.ny <- tsoutliers::tso(dat3,types = c("AO","LS","TC"),cval = sigma, maxit.iloop=10)
write_rds(outlier.mort.ny, "./R/DATA-PROCESSED/Plots/mort_ny")

# Mortality - New Hampshire
dat2 <- filter(dat_mort, location == "X33")
dat3 <- ts(dat2$Target, start = year(min(dat_mort$Year)), end = c(year(max(dat_mort$Year)),12), frequency = 12)
outlier.mort.nh <- tsoutliers::tso(dat3,types = c("AO","LS","TC"),cval = sigma, maxit.iloop=10)
write_rds(outlier.mort.nh, "./R/DATA-PROCESSED/Plots/mort_nh")

# Fertility - Connecticut
dat2 <- filter(dat_fert, location == "X09")
dat3 <- ts(dat2$Target, start = year(min(dat_fert$Year)), end = c(year(max(dat_fert$Year)),12), frequency = 12)
outlier.fert.conn <- tsoutliers::tso(dat3,types = c("AO","LS","TC"),cval = sigma, maxit.iloop=10)
write_rds(outlier.fert.conn, "./R/DATA-PROCESSED/Plots/fert_conn")

# Fertility - Louisiana
dat2 <- filter(dat_fert, location == "X22")
dat3 <- ts(dat2$Target, start = year(min(dat_fert$Year)), end = c(year(max(dat_fert$Year)),12), frequency = 12)
outlier.fert.la <- tsoutliers::tso(dat3,types = c("AO","LS","TC"),cval = sigma, maxit.iloop=10)
write_rds(outlier.fert.la, "./R/DATA-PROCESSED/Plots/fert_la")


# Fertility - Hawaii
dat2 <- filter(dat_fert, location == "X15")
dat3 <- ts(dat2$Target, start = year(min(dat_fert$Year)), end = c(year(max(dat_fert$Year)),12), frequency = 12)
outlier.fert.hi <- tsoutliers::tso(dat3,types = c("AO","LS","TC"),cval = sigma, maxit.iloop=10)
write_rds(outlier.fert.hi, "./R/DATA-PROCESSED/Plots/fert_hi")


# Fertility - Mississippi
dat2 <- filter(dat_fert, location == "X22")
dat3 <- ts(dat2$Target, start = year(min(dat_fert$Year)), end = c(year(max(dat_fert$Year)),12), frequency = 12)
outlier.fert.ms <- tsoutliers::tso(dat3,types = c("AO","LS","TC"),cval = sigma, maxit.iloop=10)

# Mortality - Ohio
dat2 <- filter(dat_mort, location == "X39")
dat3 <- ts(dat2$Target, start = year(min(dat_mort$Year)), end = c(year(max(dat_mort$Year)),12), frequency = 12)
outlier.mort.oh <- tsoutliers::tso(dat3,types = c("AO","LS","TC"),cval = sigma, maxit.iloop=10)
write_rds(outlier.mort.oh, "./R/DATA-PROCESSED/Plots/mort_oh")
