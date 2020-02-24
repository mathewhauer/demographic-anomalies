
# Reading in the mortality data
PRdeaths <- readxl::read_xlsx("./R/DATA-RAW/Deaths0918.xlsx", sheet =1)

PRdeaths <- readxl::read_xlsx("./R/DATA-RAW/deahts_pr_4Hauer.xlsx", sheet =1) %>%
  pivot_longer(-Year, names_to = "Month", values_to = "Deaths") %>%
  mutate(Month3 = AsDate(paste0(Month, " ", Year))) %>%
  filter(Year >= 1999) 

z <- ts(PRdeaths$Deaths, 
        start = year(min(PRdeaths$Month3)), 
        end = c(year(max(PRdeaths$Month3)),12), frequency = 12)

# Generating the TS outliers using just the `Deaths`
outlier.morttots <- tsoutliers::tso(ts(PRdeaths$Deaths, 
                                       start = year(min(PRdeaths$Month3)), 
                                       end = c(year(max(PRdeaths$Month3)),12), frequency = 12),  maxit.iloop=10)

# Generating the TS outliers using the `Deaths` and the regressor variable `Population_Estimate`
# outlier.mortots.popest <- tsoutliers::tso(ts(PRdeaths$Deaths), xreg = PRdeaths$Population_Estimate,  maxit.iloop=10)

# Plotting the figure for the total `Deaths`
plot(outlier.morttots)
outlier.morttots
# Summing up the difference in the Mortality using only the `Deaths`
sum(outlier.morttots$y[225:240] - outlier.morttots$yadj[225:240])

# Summing up the difference using the `Deaths` and the `Population_Estimate` regressor
sum(outlier.mortots.popest$y - outlier.mortots.popest$yadj)
