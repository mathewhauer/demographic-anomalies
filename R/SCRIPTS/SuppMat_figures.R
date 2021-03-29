library(future)
library(future.apply)
library(parallel)
anom_mort <- read_rds("./R/DATA-PROCESSED/mortality_anomalies") %>%
  na.omit %>%
  mutate(component = "Mortality")

anom_fert <- read_rds("./R/DATA-PROCESSED/fertility_anomalies") %>%
  na.omit %>%
  mutate(component = "Fertility")

### Getting a FIPS list
# This will download a list of FIPS codes from online.
fipslist <- read_csv(file="https://www2.census.gov/geo/docs/reference/codes/files/national_county.txt", col_names = FALSE) %>%
  mutate(GEOID = paste0(X2, X3),
         STATECOUNTY = paste0(X1, X4)) %>%
  dplyr::rename(state = X1,
                STATEID = X2,
                CNTYID = X3,
                NAME = X4) %>%
  filter(!STATEID %in% c("60", "66", "69", "72", "74", "78")) %>%
  dplyr::select(state, STATEID) %>%
  distinct()

abstract <- rbind(anom_mort, anom_fert) %>%
  mutate(STATEID = substr(location,2,3)) %>%
  left_join(., fipslist) %>%
  dplyr::select(state, STATEID, component, time, type, tstat, coefhat, -ind, -location) %>%
  arrange(STATEID, component, time)
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

fert_sums2 <- read_rds("./R/DATA-PROCESSED/fertility_anomalies_sums") %>%
  mutate(STATEID = substr(loc,2,3)) %>%
  left_join(., fipslist)
mort_sums2 <- read_rds("./R/DATA-PROCESSED/mortality_anomalies_sum") %>%
  mutate(STATEID = substr(loc,2,3)) %>%
  left_join(., fipslist)

listb <- sort(unlist(list(paste(unique(mort_sums2$loc), "mort", sep ="_"),
paste(unique(fert_sums2$loc), "fert", sep = "_"))))
l <- listb[1:6]

# listb<- unique(lista$state)
pdf(file = "./MainDocument/anomalies2.pdf",
    width=11, height=8.5)

figs <- list()

makefigure <- function(this.outlier, titles){
  n.sims <- 1e3
  set.seed(1)
  sims.hist <- matrix(NA,nrow=n.sims,ncol=length(this.outlier$y))
  
  createsim <- function(ii){
    # set.seed(runif(1,1,1000))
    arima.sim(n=length(this.outlier$y), model=list(this.outlier$fit), sd = sdval) + as.vector(this.outlier$yadj)
  }
  # matrix(rep(createsim(),2), ncol=2)
  sdval <- sqrt(this.outlier$fit$sigma2)
  for ( ii in 1:n.sims ) {
    print(ii)
    # setWinProgressBar(pb,ii,paste(ii,"of",n.sims))
    # sims.hist[ii,] <- z <-  this.outlier$yadj+rnorm(length(this.outlier$yadj),0,sd(this.outlier$yadj))
    
    #     sims.hist[ii,] <- matrix(rep(arima.sim(n=228, model=list(this.outlier$fit), sd = sdval) + as.vector(this.outlier$yadj)),
    #                           100))
    # sims.hist <- matrix(rep(arima.sim(n=228, model=list(this.outlier$fit), sd = sdval) + as.vector(this.outlier$yadj),
    #                     10), ncol=10)
    # 
    # model <- auto.arima(ts(sims.hist[ii,],frequency=frequency(this.outlier$yadj)))
    sims.hist[ii,] <- as.vector(createsim())
  }
  
  
  conf <- c(0.95)
  ylim <- range(
    apply(sims.hist,2,quantile,prob=c((1-max(conf))/2,1-(1-max(conf))/2)))
  boundaries.hist <- apply(sims.hist,2,quantile,prob=c((1-rev(conf)[1])/2,1-(1-rev(conf)[1])/2))
  
  a <- data.frame(Y=as.matrix(this.outlier$yadj),
                  date = time(this.outlier$yadj),
                  ymin = as.matrix(boundaries.hist[1,]),
                  ymax = as.matrix(boundaries.hist[2,])
  )
  b <- data.frame(y = as.matrix(this.outlier$y),
                  date = time(this.outlier$y))
  pts <- data.frame(date = this.outlier$times,
                    y = this.outlier$y[this.outlier$outliers$ind])
  
  effectsizes <- data.frame(date =time(this.outlier$y),
                            y= this.outlier$y - this.outlier$yadj)
  
  top <- ggplot(data= a, aes(x = date, y=Y)) +
    geom_line(data= b, aes(x=date, y=y), color = "red") +
    geom_line() +
    geom_ribbon(aes(ymin = ymin, ymax = ymax), alpha = 0.2) +
    geom_point(data=pts, aes(x=date, y =y), color = "red", fill = NA, size = 2, shape =21) +
    theme_bw() +
    scale_y_continuous(labels=comma) +
    labs(x="", y ="",
         title = paste(titles, "<br>
       <span style='font-size:10pt'><span style='color:#FF0000'>**Original**</span> and **Adjusted**  series</span>"
                       # caption = "Shaded region is the 95th percentile confidence interval"
         )) +
    theme(
      plot.title = element_markdown(lineheight = 1.2),
      axis.title.x=element_blank(),
      axis.text.x=element_blank(),
      axis.ticks.x = element_blank(),
      plot.margin = unit(c(0.1, 0, 0, 0), "cm")
    )
  
  bot <- 
    ggplot(data = effectsizes, aes(x=date, y=y)) +
    geom_line(color="blue") +
    theme_bw() +
    theme(plot.margin = unit(c(0, 0, 0, 0), "cm")) +
    scale_y_continuous(labels=comma,
                       position = "right") +
    labs(x="", y ="")
  
  together <- plot_grid(top, bot, ncol=1,
                        align = "v", rel_heights = c(1,0.6))
  return(together)
}


genplots <- function(this.state){
  print(this.state)
type <- substr(this.state, 5,9)
loca <- substr(this.state, 1,3)
st <- fipslist$state[which(fipslist$STATEID == substr(this.state,2,3))]
if(type == "mort"){
       set.seed(1)
       mort <- filter(dat_mort, location == loca)
       mortd3 <- filter(mort_sums2, loc == loca)
       mortdat3 <- ts(mort$Target, start = year(min(mort$Year)), end = c(year(max(mort$Year)),12), frequency = 12)
       mortoutlier.county <- tsoutliers::tso(mortdat3,types = c("AO","LS","TC"), cval = 3.5, maxit.iloop=10)
       together <- makefigure(mortoutlier.county, paste(st,"- Monthly Deaths"))
       subcap <- paste0("Shaded region is the 95th percentile confidence interval\n",
                        paste0(ifelse(mortd3$ydiff>0,"+",""),format(mortd3$ydiff, digits =0, big.mark = ","),
                               " expected deaths"))
       ggdraw(add_sub(together, subcap, size = 8 ,x=1, hjust=1.1))
      #   a <-  function() {
      #    plot(mortoutlier.county)
      #    title(main = paste(st,"- Monthly Deaths"),
      #          sub = paste0(format(mortd3$ydiff, digits =0, big.mark = ","),
      #                       " expected deaths"))}
      # plot_grid(ggdraw(a),
      #                  ncol=1,
      #                  labels= "")
      } else{
       set.seed(1)
       fert <- filter(dat_fert, location == loca)
       fertd3 <- filter(fert_sums2, loc == loca)
       fertdat3 <- ts(fert$Target, start = year(min(fert$Year)), end = c(year(max(fert$Year)),12), frequency = 12)
       fertoutlier.county <- tsoutliers::tso(fertdat3,types = c("AO","LS","TC"), cval = 3.5, maxit.iloop=10)
       together <- makefigure(fertoutlier.county, paste(st,"- Monthly Births"))
       subcap <- paste0("Shaded region is the 95th percentile confidence interval\n",
                        paste0(ifelse(fertd3$ydiff>0,"+",""),format(fertd3$ydiff, digits =0, big.mark = ","),
                               " expected births"))
       ggdraw(add_sub(together, subcap, size = 8 ,x=1, hjust=1.1))
       
       # b <-  function() {
       #   plot(fertoutlier.county)
       #   title(main = paste(st,"- Monthly Births"),
       #         sub = paste0(format(fertd3$ydiff, digits =0, big.mark = ","),
       #                      " expected births"))}
       # plot_grid(ggdraw(b),
       #                 ncol=1,
       #                 labels= "")
                       }
}
# plan(multiprocess)
figs<- pblapply(listb, genplots)
# figs <- future_lapply(l, genplots)

library(gridExtra)
ggsave(
  filename = "plots.pdf", 
  plot = marrangeGrob(figs, nrow=2, ncol=2), 
  width = 15, height = 9
)