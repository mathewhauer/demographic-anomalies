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


outlier.mort.ny <- read_rds("../R/DATA-PROCESSED/Plots/mort_ny")
outlier.mort.nh <- read_rds("../R/DATA-PROCESSED/Plots/mort_nh")
outlier.fert.conn <- read_rds("../R/DATA-PROCESSED/Plots/fert_conn")
outlier.fert.la <- read_rds("../R/DATA-PROCESSED/Plots/fert_la")
outlier.fert.hi <- read_rds("../R/DATA-PROCESSED/Plots/fert_hi")
outlier.mort.oh <- read_rds("../R/DATA-PROCESSED/Plots/mort_oh")

# overall_mortality <- read_rds("../R/DATA-PROCESSED/")