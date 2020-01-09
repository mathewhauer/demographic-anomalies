

migdat <- read_tsv("DATA/county_migration_data.txt")



inmig <- filter(migdat, !origin == destination) %>%
  group_by(destination) %>%
  dplyr::select(-origin) %>%
  summarise_all(sum) %>%
  mutate(direction = "inmigrants") %>%
  gather(Years, Migrants, `1990`:`2010`) %>%
  dplyr::select(location = destination, everything()) %>%
  mutate(Year = as.Date(as.yearmon(as.numeric(Years))))

outmig <- filter(migdat, !origin == destination) %>%
  # mutate(state = substr(origin, 1,2))
  group_by(origin) %>%
  dplyr::select(-destination, -origin) %>%
  summarise_all(sum)  %>%
  mutate(direction = "outmigrants")%>%
  gather(Years, Migrants, `1990`:`2010`) %>%
  dplyr::select(location = origin, everything()) %>%
  mutate(Year = as.Date(as.yearmon(as.numeric(Years))),
         location = paste0("X", location))

set.seed(1)

outmig <- outmig %>%
  dplyr::select(-Years, -direction) %>%
  spread(location, Migrants) %>%
  dplyr::select(-Year) 
time.points <- seq.Date(as.Date("1990-01-01"), by ="years", length.out = 21)
# df2 <- outmig[,i]
# df3 <- outmig[,-(i+1)]
# df <- cbind(df2, outmig[, c(sample(1:ncol(df3), 100, replace =F))])
# outmigdat <- zoo(cbind(df), time.points)

results <- data.frame(matrix( ncol =3))
colnames(results) <- c("county", "p", "eventyear")

# for (i in 1:ncol(outmig)) {
#   df2 <- outmig[,i]
#   df3 <- outmig[,-(i+1)]
#   df <- cbind(df2, outmig[, c(sample(1:ncol(df3), 100, replace =F))])
#   outmigdat <- zoo(cbind(df), time.points)
#   
#   for(j in 9:(length(time.points)-1)){
#     temp <-  data.frame(matrix( ncol =3))
#     colnames(temp) <- c("county", "p", "eventyear")
#     pre.period <- as.Date(c(time.points[1], time.points[j]))
#     post.period <- as.Date(c(time.points[j+1], time.points[length(time.points)]))
#     
#     
#     outimpact <- CausalImpact(outmigdat, pre.period, post.period)
#     temp[1,2] <- outimpact$summary$p[1]
#     temp[1,1] <- colnames(df2)[1]
#     temp[1,3] <-  paste0(time.points[j])
#     results <- rbind(results, temp)
#   }
# }
sampsize <- 100
seednum <- 1
j <- 11
 (starttime<- Sys.time())
results <-data.frame(matrix( ncol =5))
colnames(results) <- c("county", "p", "eventyear", "abseffect")
results <- foreach(
  # i = 1:ncol(outmig), 
  i = 1:(ncol(outmig)-1),
        .combine = rbind, 
        .errorhandling = "stop", 
        .packages = c("data.table", "doParallel", "foreach", "reshape2", "stringi", "stringr", "zoo", "tidyverse","CausalImpact", "bsts")) %dopar% {
        
          
          df2 <- outmig[,i]
          df3 <- outmig[,-(i+1)]
          set.seed(seednum)
          df <- cbind(df2, outmig[, c(sample(1:ncol(df3), sampsize, replace =F))])
          outmigdat <- zoo(cbind(df), time.points)
          
          # for(j in 9:(length(time.points)-1)){
            temp <-  data.frame(matrix( ncol =4))
            colnames(temp) <- c("county", "p", "eventyear", "iteration")
            # pre.period <- as.Date(c(time.points[1], time.points[j]))
            # post.period <- as.Date(c(time.points[j+1], time.points[length(time.points)]))
            j <- 11
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
            temp
         
          return(temp)
            # results <- rbind(results, temp)
            
          }
        
(endtime <- Sys.time())
(time <- endtime - starttime)

results <- arrange(results, p)

write_csv(results, "DATA-PROCESSED/preliminary_results_100samp.csv")

interest <- 1
figmake <- function(interest){
  
  outmig <- filter(migdat, !origin == destination) %>%
    # mutate(state = substr(origin, 1,2)) %>%
    group_by(origin) %>%
    dplyr::select(-destination, -origin) %>%
    summarise_all(sum)  %>%
    mutate(direction = "outmigrants")%>%
    gather(Years, Migrants, `1990`:`2010`) %>%
    dplyr::select(location = origin, everything()) %>%
    mutate(Year = as.Date(as.yearmon(as.numeric(Years))),
           location = paste0("X", location))%>%
    dplyr::select(-Years, -direction) %>%
    spread(location, Migrants) %>%
    dplyr::select(-Year) 
 
 df2 <- outmig[,results$iteration[interest]]
 df3 <- outmig[,-(results$iteration[interest]+1)]
 set.seed(seednum)
 df <- cbind(df2, outmig[, c(sample(1:ncol(df3), sampsize, replace =F))])
 outmig2 <- dplyr::select(outmig, colnames(df))
 outmigdat2 <- zoo(cbind(outmig2), time.points)
  pre.period <- as.Date(c(time.points[1], ymd(results$eventyear[interest])))
  post.period <- as.Date(c(ymd(results$eventyear[interest]) + years(1), time.points[ifelse(j+2 < length(time.points), j+2, length(time.points))]))
  
  
  outimpact <- CausalImpact(outmigdat2, pre.period, post.period)
  plot(outimpact) + theme_bw() + labs(title = paste0(results$county[interest], " Out-Migration counts for event-year ",
                                                     year(results$eventyear[interest]),
                                                     " p-val: ",
                                                     round(outimpact$summary$p[1], digits=3)))
}

# summary(outimpact, "report")

results2 <- filter(results, p <0.01)

figmake(13)