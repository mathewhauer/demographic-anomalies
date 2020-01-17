###------Summary Stats -----
## @knitr SumStats

mortsum <- read_rds("../R/DATA-PROCESSED/mortality_anomalies_sum")
mortsum$component = "Mortality" 
fertsum <- read_rds("../R/DATA-PROCESSED/fertility_anomalies_sums")
fertsum$component = "Fertility"

anom_mort <- read_rds("../R/DATA-PROCESSED/mortality_anomalies") %>%
  na.omit %>%
  mutate(component = "Mortality")

anom_fert <- read_rds("../R/DATA-PROCESSED/fertility_anomalies") %>%
  na.omit %>%
  mutate(component = "Fertility")

anom <- rbind(anom_mort, anom_fert)

types <- anom %>%
  group_by(component) %>%
  dplyr::summarise(`Anomalies` = n()) %>%
  ungroup()
 

sums <- rbind(mortsum, fertsum) %>%
  group_by(component) %>%
  dplyr::summarise(y = sum(y),
                   yhat = sum(yadj),
                   ydiff = round(sum(ydiff), 0),
                   ydiffabs = sum(ydiffabs)) %>%
  mutate(percent_anom = round((ydiffabs / y)*100, 3)) %>%
  left_join(., types) %>%
  mutate(y = f_mills(y,2),
         yhat = f_mills(yhat,2)) %>%
  dplyr::select(Component = component, 
                Anomalies, 
                `$y$` = y, 
                `$\\hat{y}$` = yhat, 
                `$y-\\hat{y}$` = ydiff, 
                `$|y-\\hat{y}|$` =ydiffabs, 
                `\\% of Total` = percent_anom)

