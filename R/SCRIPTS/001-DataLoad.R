###------Data Load -----
## @knitr DataLoad

dat_fert <-
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

dat_mort <- 
  ###MORTALITY 
  # read_tsv("DATA/Mortality/Underlying Cause of Death, 1999-2017,AL-CT.txt") %>%
  read_tsv("../R/DATA-RAW/Mortality/Underlying Cause of Death, 1999-2017,states.txt") %>%
  mutate(Month2 = substr(`Month Code`,6,7),
         Month3 = as.yearmon(paste0(Year,"-", Month2)),
         # location = paste0("X", `County Code`)
         location = paste0("X", `State Code`)
  ) %>%
  dplyr::select(Year = Month3, location, Target = Deaths) %>%
  na.omit %>%
  arrange(location, Year)



# df2<- setNames(data.frame(matrix(ncol = 5, nrow = 0)), c("type", "ind", "time", "coefhat", "tstat"))
# dat2 <- filter(dat_mort, location == "X36")
# dat3 <- ts(dat2$Target, start = year(min(dat_mort$Year)), end = c(year(max(dat_mort$Year)),12), frequency = 12)
# outlier.county <- tsoutliers::tso(dat3,types = c("AO","LS","TC"),cval = sigma, maxit.iloop=10)
# 
# plot(outlier.county,
#      args.lines.yadj = list(col = "black"),
#      args.lines.y = list(col = "gray80")) +
# title(main = "New York - Monthly Deaths")

