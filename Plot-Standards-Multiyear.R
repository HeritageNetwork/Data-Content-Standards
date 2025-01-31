## Plot standards multi-year

library(tidyverse)

## Load data for each year
data.qual.taxa <- read.csv("Output/data.qual.taxa.2025-01-30.csv") %>% mutate(Year = 2025) %>%
  bind_rows(read.csv("Output-January2024/data.qual.taxa.2025-01-30.csv") %>% mutate(Year = 2024)) %>%
  bind_rows(read.csv("Output-January2023/data.qual.taxa.2025-01-30.csv") %>% mutate(Year = 2023)) %>%
  bind_rows(read.csv("Output-January2022/data.qual.taxa.2025-01-23.csv") %>% mutate(Year = 2022))

data.qual.grank <- read.csv("Output/data.qual.grank.2025-01-30.csv") %>% mutate(Year = 2025) %>%
  bind_rows(read.csv("Output-January2024/data.qual.grank.2025-01-30.csv") %>% mutate(Year = 2024)) %>%
  bind_rows(read.csv("Output-January2023/data.qual.grank.2025-01-30.csv") %>% mutate(Year = 2023)) %>%
  bind_rows(read.csv("Output-January2022/data.qual.grank.2025-01-23.csv") %>% mutate(Year = 2022))

dat<-read.csv("Output/PrimarySubsetGlobal.csv") %>% mutate(Year = 2025) %>%
  bind_rows(read.csv("Output-January2024/PrimarySubsetGlobal.csv") %>% mutate(Year = 2024)) %>%
  bind_rows(read.csv("Output-January2023/PrimarySubsetGlobal.csv") %>% mutate(Year = 2023)) %>%
  bind_rows(read.csv("Output-January2022/PrimarySubsetGlobal-20221011.csv") %>% mutate(Year = 2022)) %>%
  filter(native) ## 2022-2024 primary global datasets include non-natives so need to filter them out

##create function to make donut charts for taxa
bar.plot.taxa <- function(data.plot, standard.plot) {
  ##first determine whether plotting for all plants/animals or by G Rank
  data.plot <- subset(data.plot, standard==standard.plot) 
  if (standard.plot == "G_Rank_Review_Date") {data.plot <- data.plot %>% mutate(value = factor(value, levels = c(">10 years", "0-10 years")))}
  data.plot <- data.plot %>%
    dplyr::group_by(taxa, Year) %>%
    dplyr::arrange(desc(value)) %>%
    dplyr::mutate(lab.ypos = cumsum(prop) - 0.5*prop) %>%
    data.frame()
  label <- subset(data.plot, value==T | value=="0-10 years", select=c(taxa, Year, prop)) %>% dplyr::group_by(taxa, Year) %>% data.frame()
  colnames(label)<-c("taxa", "Year", "label")
  
  data.plot <- dplyr::left_join(data.plot, label) %>% replace_na(list(label = 0))
  
  mycols <- c("lightgrey", "seagreen4", "gold", "#0073C2FF")
  fig.temp <- ggplot(data.plot, aes(x = Year, y = prop, fill = value)) +
    geom_bar(stat = "identity", color = "white") +
    #coord_polar(theta = "y", start = 0)+
    geom_text(aes(y = lab.ypos, label = format(n, big.mark=",")), color = "white")+
    geom_text(aes(y = 1.1, x = Year, label = paste0(round(label*100,0), "%")), color = c("black"), size = 5) +
    facet_wrap(.~taxa, ncol=1) +
    scale_fill_manual(values = mycols, name=gsub(standard.plot, pattern="_", replace=" ")) +
    theme_classic() +
    ylab("Proportion of elements") +
    theme(text = element_text(size = 12), strip.text = element_text(size=12), legend.position="right", strip.background = element_blank()) +
    theme(panel.spacing = unit(1.5, "lines")) +
    scale_y_continuous(breaks = seq(0,1, 0.1), limits = c(0,1.2), expand=c(0, 0)) +
    scale_x_continuous(expand=c(0,0), breaks = scales::breaks_pretty(n=max(data.plot$Year)-min(data.plot$Year)+1), limits = c(min(data.plot$Year)-0.5, max(data.plot$Year)+0.5))
  print(fig.temp)
}

n.groups <- max(data.qual.taxa$Year, na.rm = T)-min(data.qual.taxa$Year, na.rm = T)+1 # number of columns
standards<-unique(data.qual.taxa$standard)
for (j in 1:length(standards)) {
  png(filename = paste0("Output/fig.", standards[j],".taxa.multiyear.png"), width = 1200*n.groups/4, height = 1200, res=200)
  bar.plot.taxa(data.plot = data.qual.taxa, standard.plot = standards[j])
  dev.off()
}
