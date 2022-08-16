##step 7
##Plots of data content standards
##doesn't work for non spatial snapshot yet

library(ggplot2)

##convert counts into proportions of cases that are T/F for each standards and plants vs animals vs all
data.qual<-dplyr::arrange(.data = data.qual, standard, group, group.type, value)
data.qual.prop<-data.qual %>% dplyr::group_by(standard, group, group.type) %>% dplyr::summarise(prop = n/sum(n)) %>% data.frame()
data.qual$prop<-data.qual.prop$prop

##write out dataset
write.csv(data.qual, paste0("Output/data.qual.",Sys.Date(),".csv"), row.names=F)
data.qual<-read.csv(paste0("Output/data.qual.",Sys.Date(),".csv"))

##add years to dat.rank
dat.rank$Year<-format(dat.rank$G_RANK_REVIEW_DATE2, format = "%Y") %>% as.numeric()

##create function to make donut charts
donut.plot <- function(data.plot, standard.plot, group.plot) {
  ##first determine whether plotting for all plants/animals or by G Rank
  data.plot <- subset(data.plot, standard==standard.plot) %>%
    dplyr::group_by(group, group.type) %>%
    dplyr::arrange(desc(value)) %>%
    #dplyr::mutate(lab.ypos = cumsum(prop) - 0.5*prop) %>%
    data.frame()
  label <- subset(data.plot, value==T | value=="0-10 years", select=c(group, group.type, prop)) %>% dplyr::group_by(group, group.type) %>% data.frame()
  colnames(label)<-c("group", "group.type","label")
  
  data.plot <- dplyr::left_join(data.plot, label)
  data.plot<-subset(data.plot, group.type==group.plot)
  
  mycols <- c("lightgrey", "seagreen4", "gold", "#0073C2FF")
  fig.temp <- ggplot(data.plot, aes(x = 2, y = prop, fill = value)) +
    geom_bar(stat = "identity", color = "white") +
    coord_polar(theta = "y", start = 0)+
    #geom_text(aes(y = lab.ypos, label = paste0("n = ", n, ", \n", round(prop*100,0), "%")), color = "white")+
    geom_text(aes(y = 1, x = 1, label = paste0(round(label*100,0), "%")), color = c("black"), size = 6) +
    facet_wrap(.~group, ncol = 5) +
    scale_fill_manual(values = mycols, name=gsub(standard.plot, pattern="_", replace=" ")) +
    theme_void() +
    xlim(.9, 2.5) +
    theme(text = element_text(size = 12), strip.text = element_text(size=12), legend.position="bottom")
  print(fig.temp)
}

##for loop that creates a donut chart for each standard and saves it
standards<-unique(data.qual$standard)
for (j in 1:length(standards)) {
  png(filename = paste0("Output/fig.", standards[j],".taxa.png"), width = 1200, height = 1200*.8, res=200)
  donut.plot(data.plot = data.qual, standard.plot = standards[j], group.plot="taxa")
  dev.off()
  
  if(nrow(subset(data.qual, standard==standards[j] & group.type=="G_Rank"))==0) {next} ##move to next standard if there are no data for various G ranks
  
  png(filename = paste0("Output/fig.", standards[j],".GRank.png"), width = 1200, height = 1200*.8, res=150)
  donut.plot(data.plot = data.qual, standard.plot = standards[j], group.plot="G_Rank")
  dev.off()
}

##Histogram of year of last review 
##Break up by G/T rank 
fig <- ggplot(data = dat.rank, aes(Year)) +
  geom_bar() +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust=1, color="black"), axis.text.y = element_text(color="black")) +
  scale_x_continuous(breaks = scales::breaks_pretty(n = 10)) +
  xlab("G Rank Review Date") +
  scale_y_continuous(expand=c(0,0), breaks = scales::breaks_pretty(n=10))
fig

##print out this version for all g ranks combined
png(filename = "Output/fig.rankreviewdate.png", width = 6.5, height = 3, units = "in", res=200)
print(fig)
dev.off()

##hist with facets
fig <- ggplot(data = dat.rank, aes(Year)) +
  geom_bar() +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust=1, color="black"), axis.text.y = element_text(color="black")) +
  scale_x_continuous(breaks = scales::breaks_pretty(n = 10)) +
  xlab("G Rank Review Date") +
  scale_y_continuous(expand=c(0,0), breaks = scales::breaks_pretty(n=5))
fig <- fig + facet_wrap(.~G_RANK, scales = "free", ncol=2) + theme(strip.background = element_rect(colour = "white", fill = "white"))
fig

##print out this version for split up g ranks
##might want to adjust scale_x_date breaks and bins
png(filename = "Output/fig.rankreviewdate.grank.png", width = 6.5, height = 9, units = "in", res=200)
print(fig)
dev.off()
