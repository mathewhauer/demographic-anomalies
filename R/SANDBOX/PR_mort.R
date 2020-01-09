
# Reading in the mortality data
PRdeaths <- readxl::read_xlsx("DATA/Deaths0918.xlsx", sheet =1)

# Generating the TS outliers using just the `Deaths`
outlier.morttots <- tsoutliers::tso(ts(PRdeaths$Deaths),  maxit.iloop=10)

# Generating the TS outliers using the `Deaths` and the regressor variable `Population_Estimate`
outlier.mortots.popest <- tsoutliers::tso(ts(PRdeaths$Deaths), xreg = PRdeaths$Population_Estimate,  maxit.iloop=10)

# Plotting the figure for the total `Deaths`
plot(outlier.morttots)

# Summing up the difference in the Mortality using only the `Deaths`
sum(outlier.morttots$y - outlier.morttots$yadj)

# Summing up the difference using the `Deaths` and the `Population_Estimate` regressor
sum(outlier.mortots.popest$y - outlier.mortots.popest$yadj)
