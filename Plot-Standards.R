##step 6
##Plots of data content standards
##doesn't work for non spatial snapshot yet

#data.qual<-read.csv(paste0("Output/data.qual.",Sys.Date(),".csv"))
#dat.rank<-read.csv("Output/data.rank.csv")
data.qual.taxa<- read.csv("Output/data.qual.taxa2022-08-18.csv")
data.qual.grank <- read.csv("Output/data.qual.grank.2022-08-18.csv")
dat<-read.csv("Output/PrimarySubsetGlobal.csv")

##create function to make donut charts for taxa
donut.plot.taxa <- function(data.plot, standard.plot) {
  ##first determine whether plotting for all plants/animals or by G Rank
  data.plot <- subset(data.plot, standard==standard.plot) %>%
    dplyr::group_by(taxa) %>%
    dplyr::arrange(desc(value)) %>%
    dplyr::mutate(lab.ypos = cumsum(prop) - 0.5*prop) %>%
    data.frame()
  label <- subset(data.plot, value==T | value=="0-10 years", select=c(taxa, prop)) %>% dplyr::group_by(taxa) %>% data.frame()
  colnames(label)<-c("taxa","label")
  
  data.plot <- dplyr::left_join(data.plot, label)
  
  mycols <- c("lightgrey", "seagreen4", "gold", "#0073C2FF")
  fig.temp <- ggplot(data.plot, aes(x = 2, y = prop, fill = value)) +
    geom_bar(stat = "identity", color = "white") +
    coord_polar(theta = "y", start = 0)+
    geom_text(aes(y = lab.ypos, label = format(n, big.mark=",")), color = "white")+
    geom_text(aes(y = 1, x = 1, label = paste0(round(label*100,0), "%")), color = c("black"), size = 6) +
    facet_wrap(.~taxa, ncol = 5) +
    scale_fill_manual(values = mycols, name=gsub(standard.plot, pattern="_", replace=" ")) +
    theme_void() +
    xlim(.9, 2.5) +
    theme(text = element_text(size = 12), strip.text = element_text(size=12), legend.position="bottom")
  print(fig.temp)
}

##create function to make donut charts for granks
donut.plot.grank <- function(data.plot, standard.plot) {
  ##first determine whether plotting for all plants/animals or by G Rank
  data.plot <- subset(data.plot, standard==standard.plot) %>%
    dplyr::group_by(taxa, G_RANK) %>%
    dplyr::arrange(desc(value)) %>%
    dplyr::mutate(lab.ypos = cumsum(prop) - 0.5*prop) %>%
    data.frame()
  label <- subset(data.plot, value==T | value=="0-10 years", select=c(taxa, G_RANK, prop)) %>% dplyr::group_by(taxa, G_RANK) %>% data.frame()
  colnames(label)<-c("taxa","G_RANK","label")
  
  data.plot <- dplyr::left_join(data.plot, label)
  
  mycols <- c("lightgrey", "seagreen4", "gold", "#0073C2FF")
  fig.temp <- ggplot(data.plot, aes(x = 2, y = prop, fill = value)) +
    geom_bar(stat = "identity", color = "white") +
    coord_polar(theta = "y", start = 0)+
    geom_text(aes(y = lab.ypos, label = format(n, big.mark=",")), color = "white")+
    geom_text(aes(y = 1, x = 1, label = paste0(round(label*100,0), "%")), color = c("black"), size = 6) +
    facet_wrap(taxa ~ G_RANK) +
    scale_fill_manual(values = mycols, name=gsub(standard.plot, pattern="_", replace=" ")) +
    theme_void() +
    xlim(.9, 2.5) +
    theme(text = element_text(size = 12), strip.text = element_text(size=12), legend.position="bottom")
  print(fig.temp)
}

##for loop that creates a donut chart for each standard and saves it
standards<-unique(data.qual.taxa$standard)
for (j in 1:length(standards)) {
  png(filename = paste0("Output/fig.", standards[j],".taxa.png"), width = 1200, height = 1200*.8, res=200)
  donut.plot.taxa(data.plot = data.qual.taxa, standard.plot = standards[j])
  dev.off()
  
  if(standards[j]=="G_Rank") {next} ##move to next standard if there are no data for various G ranks
  
  png(filename = paste0("Output/fig.", standards[j],".GRank.png"), width = 1200, height = 1200*1.5, res=150)
  donut.plot.grank(data.plot = data.qual.grank, standard.plot = standards[j])
  dev.off()
}

##Histogram of year of last review 
##Break up by G/T rank 
fig <- ggplot(data = dat, aes(Year)) +
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
data.plot <- subset(dat, !is.na(taxa) & !(G_RANK %in% c("GH/TH", "GNA/TNA", "GNR/TNR", "GU/TU", "GX/TX")))
fig <- ggplot(data = data.plot, aes(Year)) +
  geom_bar() +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust=1, color="black"), axis.text.y = element_text(color="black")) +
  scale_x_continuous(breaks = scales::breaks_pretty(n = 10), limits = c(min(dat$Year, na.rm=T), max(dat$Year, na.rm = T))) +
  xlab("G Rank Review Date") +
  scale_y_continuous(expand=c(0,0), breaks = scales::breaks_pretty(n=5))
fig <- fig + facet_grid(G_RANK~taxa, scales = "free") + theme(strip.background = element_rect(colour = "white", fill = "white"))
fig

##print out this version for split up g ranks
##might want to adjust scale_x_date breaks and bins
png(filename = "Output/fig.rankreviewdate.grank.png", width = 6.5, height = 9, units = "in", res=200)
print(fig)
dev.off()
