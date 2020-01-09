

dat <- read_tsv("DATA/Compressed Mortality, 1999-2016.txt") %>%
  mutate(Year = as.Date(as.yearmon(as.numeric(Year))),
         location = paste0("X", `County Code`)) %>%
  dplyr::select(Year, location, Deaths) %>%
  na.omit %>%
  spread(Year, Deaths) %>%
  na.omit %>%
  gather(Year, Deaths, `1999-01-01`:`2016-01-01`)

timelength <- year(max(dat$Year)) - year(min(dat$Year))+1

time.points <- seq.Date(as.Date(min(dat$Year)), by ="years", length.out = timelength )

dat<- spread(dat, location, Deaths) 


dat <- dplyr::select(dat, -Year)







casualfunc<- function(datframe,j){
  df2 <- datframe[,i]
  df3 <- datframe[,-(i+1)]
  # set.seed(seednum)
  df <- cbind(df2, datframe[, c(sample(1:ncol(df3), sampsize, replace =F))])
  outmigdat <- zoo(cbind(df), time.points)
  
  # for(j in 9:(length(time.points)-1)){
  temp <-  data.frame(matrix( ncol =6))
  colnames(temp) <- c("county", "p", "eventyear", "iteration", "abseffect", "samplesize")
  # pre.period <- as.Date(c(time.points[1], time.points[j]))
  # post.period <- as.Date(c(time.points[j+1], time.points[length(time.points)]))
  pre.period <- as.Date(c(time.points[1], time.points[j]))
  post.period <- as.Date(c(time.points[j+1], time.points[ifelse(j+2 < length(time.points), j+2, length(time.points))]))
  
  
  tryCatch({#print(x)
    outimpact <- CausalImpact(outmigdat, pre.period, post.period)
    # return(outimpact)
  }
  , error=function(e){cat(i," ERROR :",conditionMessage(e), "\n")})
  # endtime <- Sys.time()
  # (time <- endtime - starttime)
  temp[1,2] <- ifelse(is.null(outimpact$summary$p[1]),NA,outimpact$summary$p[1])
  temp[1,1] <- colnames(df2)[1]
  temp[1,3] <-  paste0(time.points[j])
  temp[1,4] <-  i
  temp[1,5] <- outimpact$summary$AbsEffect[2]
  temp[1,6] <- sampsize
  temp
  
  return(temp)}

sampsize <- 200
seednum <- 1
system.time(casualfunc(dat,11))

(starttime<- Sys.time())
results2 <-data.frame(matrix(nrow = ncol(dat), ncol =6))
colnames(results2) <- c("county", "p", "eventyear", "abseffect", "samplesize")

results2 <- foreach(
  # i = 1:10,
  i = 1:(ncol(dat)-1),
  .combine = rbind, 
  .errorhandling = "stop", 
  .packages = c("data.table", "doParallel", "foreach", "reshape2", "stringi", "stringr", "zoo", "tidyverse","CausalImpact", "bsts")) %dopar% {
    
   d <- casualfunc(dat, 11)
   return(d)
    
  }



(endtime <- Sys.time())
(time <- endtime - starttime)

results2 <- arrange(results2, p)
results2 <- filter(results2, p<0.01)

figmake<- function(datframe,j, l, i){
  df2 <- datframe[,i]
  df3 <- datframe[,-(i+1)]
  set.seed(seednum)
  df <- cbind(df2, datframe[, c(sample(1:ncol(df3), sampsize, replace =F))])
  outmigdat <- zoo(cbind(df), time.points)
  
  # for(j in 9:(length(time.points)-1)){
  temp <-  data.frame(matrix( ncol =5))
  colnames(temp) <- c("county", "p", "eventyear", "iteration", "abseffect")
  # pre.period <- as.Date(c(time.points[1], time.points[j]))
  # post.period <- as.Date(c(time.points[j+1], time.points[length(time.points)]))
  pre.period <- as.Date(c(time.points[1], time.points[j]))
  post.period <- as.Date(c(time.points[j+1], time.points[ifelse(j+l < length(time.points), j+l, length(time.points))]))
  
  
  tryCatch({#print(x)
    outimpact <- CausalImpact(outmigdat, pre.period, post.period)
    plot(outimpact) + theme_bw() + labs(title = paste0(colnames(df2), " Deaths counts for event-year ",
                                                       year(post.period[1])-1,
                                                       " p-val: ",
                                                       round(outimpact$summary$p[1], digits=3)))
  }
  , error=function(e){cat(i," ERROR :",conditionMessage(e), "\n")})
  # # endtime <- Sys.time()
  # # (time <- endtime - starttime)
  # temp[1,2] <- ifelse(is.null(outimpact$summary$p[1]),NA,outimpact$summary$p[1])
  # temp[1,1] <- colnames(df2)[1]
  # temp[1,3] <-  paste0(time.points[j])
  # temp[1,4] <-  i
  # temp[1,5] <- outimpact$summary$AbsEffect[2]
  # temp
  # 
  # return(temp)
  }

figmake(dat, 9, 4, 1093)

figmake(dat, 11,4, 1439)
figmake(dat, 11,4, 214)

pdf("plots.pdf", onefile = TRUE)
for(this.iteration in unique(results2$iteration)){
  print(figmake(dat, 11,4, this.iteration))
}
# pdf("plots.pdf", onefile = TRUE)
# figmake(dat, 11,4, unique(results2$iteration)[1])
dev.off()

# 06079
# 18029
# 13123
# 44001
# 42017
# 28029
# 18085
# 21163
# 13281
# 21211
# 20149
#22093
#26165
#27069
#18175
#46093
#13057
#28121
#49015
#37059
#48113
#29213
#48029
#24031

