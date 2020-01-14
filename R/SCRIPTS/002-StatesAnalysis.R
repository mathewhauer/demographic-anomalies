###------States Analysis  -----
## @knitr StatesAnalysis

# Mortality - New York
dat2 <- filter(dat_mort, location == "X36")
dat3 <- ts(dat2$Target, start = year(min(dat_mort$Year)), end = c(year(max(dat_mort$Year)),12), frequency = 12)
outlier.mort.ny <- tsoutliers::tso(dat3,types = c("AO","LS","TC"),cval = sigma, maxit.iloop=10)

# Mortality - New Hampshire
dat2 <- filter(dat_mort, location == "X33")
dat3 <- ts(dat2$Target, start = year(min(dat_mort$Year)), end = c(year(max(dat_mort$Year)),12), frequency = 12)
outlier.mort.nh <- tsoutliers::tso(dat3,types = c("AO","LS","TC"),cval = sigma, maxit.iloop=10)

# Fertility - Connecticut
dat2 <- filter(dat_fert, location == "X09")
dat3 <- ts(dat2$Target, start = year(min(dat_fert$Year)), end = c(year(max(dat_fert$Year)),12), frequency = 12)
outlier.fert.conn <- tsoutliers::tso(dat3,types = c("AO","LS","TC"),cval = sigma, maxit.iloop=10)

# Fertility - Louisiana
dat2 <- filter(dat_fert, location == "X22")
dat3 <- ts(dat2$Target, start = year(min(dat_fert$Year)), end = c(year(max(dat_fert$Year)),12), frequency = 12)
outlier.fert.la <- tsoutliers::tso(dat3,types = c("AO","LS","TC"),cval = sigma, maxit.iloop=10)


# Fertility - Hawaii
dat2 <- filter(dat_fert, location == "X15")
dat3 <- ts(dat2$Target, start = year(min(dat_fert$Year)), end = c(year(max(dat_fert$Year)),12), frequency = 12)
outlier.fert.hi <- tsoutliers::tso(dat3,types = c("AO","LS","TC"),cval = sigma, maxit.iloop=10)

# Fertility - Mississippi
dat2 <- filter(dat_fert, location == "X22")
dat3 <- ts(dat2$Target, start = year(min(dat_fert$Year)), end = c(year(max(dat_fert$Year)),12), frequency = 12)
outlier.fert.ms <- tsoutliers::tso(dat3,types = c("AO","LS","TC"),cval = sigma, maxit.iloop=10)

# Mortality - North Carolina
dat2 <- filter(dat_mort, location == "X36")
dat3 <- ts(dat2$Target, start = year(min(dat_fert$Year)), end = c(year(max(dat_fert$Year)),12), frequency = 12)
outlier.mort.nc <- tsoutliers::tso(dat3,types = c("AO","LS","TC"),cval = sigma, maxit.iloop=10)

# # Mortality - Ohio
# dat2 <- filter(dat_mort, location == "X34")
# dat3 <- ts(dat2$Target, start = year(min(dat_fert$Year)), end = c(year(max(dat_fert$Year)),12), frequency = 12)
# (outlier.mort.oh <- tsoutliers::tso(dat3,types = c("AO","LS","TC"),cval = sigma, maxit.iloop=10))