
library(tsoutliers)
set.seed(1)

z <- mortality_anomalies %>%
  group_by(ind) %>%
  dplyr::summarise(number = n(),
                   targets = sum(coefhat))

z2 <- fertility_anomalies %>%
  group_by(ind) %>%
  dplyr::summarise(number = n(),
                   targets = sum(coefhat))


mort2 <- fertility_anomalies %>%
  mutate(direction = if_else(coefhat <0, "dip", "spike")) %>%
  group_by(time, direction) %>%
  dplyr::summarise(number = n(),
                   targets = sum(coefhat))

ggplot(data = mort2, aes(x = time, y = number, fill = direction)) +
  geom_bar(stat = "identity") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title = "# of States exhibiting a mortality anomaly")

ggplot(data = fertility_anomalies, aes(x = ind)) +
  geom_bar(stat = "count") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title = "# of States exhibiting a fertility anomaly")

mort <- mortality_anomalies %>%
  mutate(Clusters = case_when(
    ind %in% c(13,14,15) ~ "2000:January",
    ind %in% c(60) ~ "2003:December",
    ind %in% c(73,74,75) ~ "2005:January",
    ind %in% c(109,110,111) ~ "2008:January",
    ind %in% c(169) ~ "2013:January",
    ind %in% c(191,192,193,194) ~ "2014:November"
  )) %>%
  na.omit %>%
  mutate(GEOID = substr(loc,2,3),
         number = "a") %>%
  dplyr::select(GEOID, Clusters, number, coefhat, code = STATE_ABBREV) %>%
  distinct() %>%
  group_by(GEOID, Clusters, number, code) %>%
  dplyr::summarise(coefhat = sum(coefhat))
  # pivot_wider(names_from = Clusters, values_from = number )
  # filter(Clusters == "January 2000")

data(state_laea) 

states <- left_join(state_laea, mort) %>%
  na.omit

states2 <- state_laea

ggplot(data=states) +
  # geom_bar(mapping=aes(x=number, fill=number))
  geom_sf(data = states2, color = NA, ) +
  geom_sf(aes(fill = number),  lwd = 1/2) +
  theme_bw() +
  theme(axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        legend.position="bottom"
  )+
  guides(fill=guide_legend(nrow=1,byrow=TRUE)) +
  facet_wrap(~Clusters, ncol=2)


data(us_state_grid1)

states <- left_join(us_state_grid1, mort) %>%
  na.omit

jan2000 <- left_join(us_state_grid1, mort[which(mort$Clusters == "2000:January"),])
jan20001 <- left_join(us_state_grid1, mort[which(mort$Clusters == "2003:December"),])
jan20002 <- left_join(us_state_grid1, mort[which(mort$Clusters == "2005:January"),])
jan20003 <- left_join(us_state_grid1, mort[which(mort$Clusters == "2008:January"),])
jan20004 <- left_join(us_state_grid1, mort[which(mort$Clusters == "2013:January"),])
jan20005 <- left_join(us_state_grid1, mort[which(mort$Clusters == "2014:November"),])
jan2000$number[is.na(jan2000$number)] <-"b"
jan20001$number[is.na(jan20001$number)] <-"b"
jan20002$number[is.na(jan20002$number)] <-"b"
jan20003$number[is.na(jan20003$number)] <-"b"
jan20004$number[is.na(jan20004$number)] <-"b"
jan20005$number[is.na(jan20005$number)] <-"b"

facetplot <- function(df, titles){
  a<- ggplot(data=df) +
    geom_bar(mapping=aes(x=1, fill= coefhat)) +
    scale_fill_gradient2()+
    # scale_fill_manual(values = c("black", "gray")) +
    theme_bw() +
    facet_geo(~ code) +
    theme_classic() +
    theme(plot.title = element_text(size = 20), # format the plot
          plot.margin = unit(c(1,1,1,1), "cm"),
          legend.text=element_text(size=16),
          legend.title = element_text(size=16),
          axis.title=element_blank(),
          axis.text=element_blank(),
          axis.ticks = element_blank(),
          strip.text.x = element_blank(),
          axis.line = element_blank(),
          legend.position = "bottom"
          ) +
    geom_text(aes(x=1, y=0.5, label=code), color='#ffffff', size=7)  +
    labs(title = paste0(titles))
  return(a)
}

a <- facetplot(jan2000, "2000:January")
b <- facetplot(jan20001, "2003:December")
c <- facetplot(jan20002, "2005:January")
d <- facetplot(jan20003, "2008:January")
e <- facetplot(jan20004, "2013:January")
f <- facetplot(jan20005, "2014:November")
# myplot <- plot_grid(a,b,c,d,e,f, ncol = 2)
pdf("myplot.pdf")

print(a)
print(b)
print(c)
print(d)
print(e)
print(f)
dev.off()

plot_grid(a,b,c,d,e,f, ncol = 2)
