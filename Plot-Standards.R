##step 6
##Plots of data content standards
##doesn't work for non spatial snapshot yet

#data.qual<-read.csv(paste0("Output/data.qual.",Sys.Date(),".csv"))
#dat.rank<-read.csv("Output/data.rank.csv")
data.qual.taxa<- read.csv("Output/data.qual.taxa.2025-01-30.csv")
data.qual.grank <- read.csv("Output/data.qual.grank.2025-01-30.csv")
dat<-read.csv("Output/PrimarySubsetGlobal.csv")

##read in data for ecosystems
dat.ecosystems <- read.csv("Output/PrimarySubsetGlobalEcosystems.csv")
data.qual.ecosystems.taxa <- read.csv("Output/data.qual.ecosystems.taxa.2025-01-22.csv") %>% mutate(taxa = factor(taxa, levels = c("Association", "Alliance", "Group")))
data.qual.ecosystems.grank <- read.csv("Output/data.qual.ecosystems.grank.2025-01-22.csv") %>% mutate(G_RANK = factor(G_RANK, levels = c("Imperiled", "Vulnerable", "Apparently\nSecure", "GU"))) %>% mutate(taxa = factor(taxa, levels = c("Association", "Alliance", "Group")))

##create function to make donut charts for taxa
donut.plot.taxa <- function(data.plot, standard.plot) {
  ##first determine whether plotting for all plants/animals or by G Rank
  data.plot <- subset(data.plot, standard==standard.plot) 
  if (standard.plot == "G_Rank_Review_Date") {data.plot <- data.plot %>% mutate(value = factor(value, levels = c(">10 years", "0-10 years")))}
  data.plot <-  data.plot %>%
    dplyr::group_by(taxa) %>%
    dplyr::arrange(desc(value)) %>%
    dplyr::mutate(lab.ypos = cumsum(prop) - 0.5*prop) %>%
    data.frame()
  label <- subset(data.plot, value==T | value=="0-10 years", select=c(taxa, prop)) %>% dplyr::group_by(taxa) %>% data.frame()
  colnames(label)<-c("taxa","label")
  
  data.plot <- dplyr::left_join(data.plot, label) %>% replace_na(list(label = 0))
  
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
    #facet_wrap(taxa ~ G_RANK) +
    facet_grid(taxa ~ G_RANK) +
    scale_fill_manual(values = mycols, name=gsub(standard.plot, pattern="_", replace=" ")) +
    theme_void() +
    xlim(.9, 2.5) +
    theme(text = element_text(size = 12), strip.text = element_text(size=12), legend.position="bottom")
  print(fig.temp)
}

##create function to make bar charts for granks
bar.plot.grank <- function(data.plot, standard.plot) {
  data.plot <- subset(data.plot, standard==standard.plot) 
  if (standard.plot == "G_Rank_Review_Date") {data.plot <- data.plot %>% mutate(value = factor(value, levels = c(">10 years", "0-10 years")))}
  data.plot <- data.plot %>%
    dplyr::group_by(taxa, G_RANK) %>%
    dplyr::arrange(desc(value)) %>%
    dplyr::mutate(lab.ypos = cumsum(prop) - 0.5*prop) %>%
    data.frame()
  label <- subset(data.plot, value==T | value=="0-10 years", select=c(taxa, G_RANK, prop)) %>% dplyr::group_by(taxa, G_RANK) %>% data.frame()
  colnames(label)<-c("taxa","G_RANK","label")
  
  data.plot <- dplyr::left_join(data.plot, label) %>% replace_na(list(label = 0)) #%>% mutate(G_RANK = str_replace(string = G_RANK, pattern = " ", replacement = "\n"))
  
  ## Get number of Grank groups for legend placement
  n.groups<-length(unique(data.plot$G_RANK))
  
  mycols <- c("lightgrey", "seagreen4", "gold", "#0073C2FF")
  fig.temp <- ggplot(data.plot, aes(x = G_RANK, y = prop, fill = value)) +
    geom_bar(stat = "identity", color = "white") +
    geom_text(aes(y = lab.ypos, label = format(n, big.mark=",")), color = "white")+
    facet_grid(taxa~.) +
    scale_fill_manual(values = mycols, name=gsub(standard.plot, pattern="_", replace=" ")) +
    theme_classic() +
    theme(text = element_text(size = 12), strip.text = element_text(size=12), legend.position="bottom") +
    ylab("Proportion") +
    xlab("G Rank") +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
    theme(strip.background = element_blank()) +
    theme(panel.spacing = unit(1.5, "lines")) +
    scale_y_continuous(expand=c(0,0), breaks = scales::breaks_pretty(n=10))
  print(fig.temp)
}

##for loop that creates a donut chart for each standard and saves it
standards<-unique(data.qual.taxa$standard)
for (j in 1:length(standards)) {
  png(filename = paste0("Output/fig.", standards[j],".taxa.png"), width = 1200, height = 1200*.7, res=200)
  donut.plot.taxa(data.plot = data.qual.taxa, standard.plot = standards[j])
  dev.off()
  
  if(standards[j]=="G_Rank") {next} ##move to next standard if there are no data for various G ranks
  
  png(filename = paste0("Output/fig.", standards[j],".GRank.png"), width = 1200, height = 1200/1.2, res=150)
  donut.plot.grank(data.plot = data.qual.grank, standard.plot = standards[j])
  dev.off()
  
  ##check how many groups are in the plot
  n.groups<-length(unique(subset(data.qual.grank, standard == standards[j])$G_RANK))
  ##add bar plot for grank groups
  png(filename = paste0("Output/fig.", standards[j],".GRank.barplot.png"), width = 1200*n.groups/4, height = 1200*1.5, res=150*2.5)
  bar.plot.grank(data.plot = data.qual.grank, standard.plot = standards[j])
  dev.off()
}

##Histogram of year of last review 
##Break up by G/T rank 
data.plot <- subset(dat, !is.na(taxa) & !(G_RANK %in% c("GNA/TNA", "GNR/TNR", "GX/TX")))
fig <- ggplot(data = data.plot, aes(Year)) +
  geom_bar() +
  geom_bar(data=subset(data.plot, Year>=as.numeric(format(Sys.Date(), "%Y"))-10), fill="seagreen4") +
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

## Histogram in 5-year increments
data.plot <- dat %>%
  filter(!is.na(taxa) & !(G_RANK %in% c("GNA/TNA", "GNR/TNR", "GX/TX"))) %>%
  mutate(bin = cut(Year, breaks = c(seq(max(Year), min(Year), by = -5), min(Year)-1)))

fig <- ggplot(data = data.plot, aes(x = bin)) +
  #geom_histogram(binwidth = 5) +
  geom_histogram(stat = "count") +
  #scale_x_continuous(breaks = seq(min(data.plot$Year), max(data.plot$Year), by = 5), labels = function(x) paste(x, "-", x + 4)) +
  #scale_x_continuous(breaks = scales::breaks_pretty(n = 10)) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust=1, color="black"), axis.text.y = element_text(color="black")) +
  xlab("G Rank Review Date") +
  scale_y_continuous(expand=c(0,0), breaks = scales::breaks_pretty(n=10))
fig



##hist with facets
data.plot <- subset(dat, !is.na(taxa) & !(G_RANK %in% c("GNA/TNA", "GNR/TNR", "GX/TX")))
fig <- ggplot(data = data.plot, aes(Year)) +
  geom_bar() +
  geom_bar(data=subset(data.plot, Year>=as.numeric(format(Sys.Date(), "%Y"))-10), fill="seagreen4") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust=1, color="black"), axis.text.y = element_text(color="black")) +
  scale_x_continuous(breaks = scales::breaks_pretty(n = 10), limits = c(min(data.plot$Year, na.rm=T)-1, max(data.plot$Year, na.rm = T)+1)) +
  xlab("G Rank Review Date") +
  scale_y_continuous(expand=c(0,0), breaks = scales::breaks_pretty(n=5))
fig <- fig + facet_grid(G_RANK~taxa, scales = "free") + theme(strip.background = element_rect(colour = "white", fill = "white"))
fig

##print out this version for split up g ranks
##might want to adjust scale_x_date breaks and bins
png(filename = "Output/fig.rankreviewdate.grank.png", width = 6.5, height = 9, units = "in", res=200)
print(fig)
dev.off()

##CREATE PLOTS FOR ECOSYSTEMS DATA
standards<-unique(data.qual.ecosystems.taxa$standard)
for (j in 1:length(standards)) {
  png(filename = paste0("Output/fig.", standards[j],".ecosystems.taxa.png"), width = 1200, height = 1200*.7, res=200)
  donut.plot.taxa(data.plot = data.qual.ecosystems.taxa, standard.plot = standards[j])
  dev.off()
  
  if(standards[j]=="G_Rank") {next} ##move to next standard if there are no data for various G ranks
  
  png(filename = paste0("Output/fig.", standards[j],".GRank.png"), width = 1200, height = 1200/1.2, res=150)
  donut.plot.grank(data.plot = data.qual.ecosystems.grank, standard.plot = standards[j])
  dev.off()
  
  ##check how many groups are in the plot
  n.groups<-length(unique(subset(data.qual.ecosystems.grank, standard == standards[j])$G_RANK))
  ##add bar plot for grank groups
  png(filename = paste0("Output/fig.ecosystems.", standards[j],".GRank.barplot.png"), width = 1200*n.groups/4, height = 1200*1.9, res=150*2.5)
  bar.plot.grank(data.plot = data.qual.ecosystems.grank, standard.plot = standards[j])
  dev.off()
}

##HISTOGRAMS FOR ECOSYSTEMS
##Histogram of year of last review 
##Break up by G/T rank 
data.plot <- dat.ecosystems %>% rename(taxa = CLASSIFICATION_LEVEL_NAME) %>% subset(!is.na(taxa) & !is.na(G_RANK))
fig <- ggplot(data = data.plot, aes(Year)) +
  geom_bar() +
  geom_bar(data=subset(data.plot, Year>=as.numeric(format(Sys.Date(), "%Y"))-10), fill="seagreen4") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust=1, color="black"), axis.text.y = element_text(color="black")) +
  scale_x_continuous(breaks = scales::breaks_pretty(n = 10)) +
  xlab("G Rank Review Date") +
  scale_y_continuous(expand=c(0,0), breaks = scales::breaks_pretty(n=10))
fig

##print out this version for all g ranks combined
png(filename = "Output/fig.rankreviewdate.ecosystems.png", width = 6.5, height = 3, units = "in", res=200)
print(fig)
dev.off()

##hist with facets
fig <- ggplot(data = data.plot, aes(Year)) +
  geom_bar() +
  geom_bar(data=subset(data.plot, Year>=as.numeric(format(Sys.Date(), "%Y"))-10), fill="seagreen4") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust=1, color="black"), axis.text.y = element_text(color="black")) +
  scale_x_continuous(breaks = scales::breaks_pretty(n = 10), limits = c(min(data.plot$Year, na.rm=T)-1, max(data.plot$Year, na.rm = T)+1)) +
  xlab("G Rank Review Date") +
  scale_y_continuous(expand=c(0,0), breaks = scales::breaks_pretty(n=5))
fig <- fig + facet_grid(G_RANK~taxa, scales = "free") + theme(strip.background = element_rect(colour = "white", fill = "white"))
fig

##print out this version for split up g ranks
##might want to adjust scale_x_date breaks and bins
png(filename = "Output/fig.rankreviewdate.grank.ecosystems.png", width = 6.5, height = 9, units = "in", res=200)
print(fig)
dev.off()


## Plot g rank review date by informal taxonomy
if ("taxa2" %in% colnames(data.qual.taxa)) {
  standard.plot<-standards[2]
  data.plot <- data.qual.taxa %>% 
    filter(standard==standard.plot) %>%
    filter(!is.na(taxa2)) %>%
    mutate(value = factor(value, levels = c(">10 years", "0-10 years")))
  data.plot <-  data.plot %>%
    dplyr::group_by(taxa2, taxa) %>%
    dplyr::arrange(desc(value)) %>%
    dplyr::mutate(lab.ypos = cumsum(prop) - 0.5*prop) %>%
    data.frame()
  label <- subset(data.plot, value==T | value=="0-10 years", select=c(taxa2, taxa, prop)) %>% dplyr::group_by(taxa2, taxa) %>% data.frame()
  colnames(label)<-c("taxa2", "taxa","label")
  
  data.plot <- dplyr::left_join(data.plot, label) %>% replace_na(list(label = 0))
  
  mycols <- c("lightgrey", "seagreen4", "gold", "#0073C2FF")
  fig.temp <- ggplot(data.plot, aes(x = taxa, y = prop, fill = value)) +
    geom_bar(stat = "identity", color = "white") +
    #geom_text(aes(y = lab.ypos, label = format(n, big.mark=",")), color = "white")+
    #facet_grid(taxa2~., scales = "free") +
    scale_fill_manual(values = mycols, name=gsub(standard.plot, pattern="_", replace=" ")) +
    theme_classic() +
    #theme(text = element_text(size = 12), strip.text = element_text(size=12), legend.position="bottom") +
    ylab("Proportion of taxa") +
    xlab("Taxonomic Group") +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
    theme(strip.background = element_blank()) +
    scale_y_continuous(expand=c(0,0), breaks = scales::breaks_pretty(n=10))
  print(fig.temp)
  
  png(filename = "Output/fig.rankreviewdate.informal.taxa.png", width = 12, height = 5, units = "in", res=200)
  print(fig.temp)
  dev.off()
}
