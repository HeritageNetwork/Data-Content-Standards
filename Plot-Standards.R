##Plots of data content standards
##doesn't work for non spatial snapshot yet

library(ggplot2)

##convert counts into proportions of cases that are T/F for each standards and plants vs animals vs all
data.qual.prop<-data.qual %>% dplyr::group_by(standard, taxa) %>% dplyr::summarise(prop = n/sum(n)) %>% data.frame()
data.qual$prop<-data.qual.prop$prop

##create function to make donut charts
donut.plot <- function(data.plot, standard.plot) {
  ##get the label positions; need to get different positions for each taxa (code needs updating here)
  data.plot <- subset(data.plot, standard==standard.plot) %>%
    dplyr::group_by(taxa) %>%
    dplyr::arrange(desc(value)) %>%
    dplyr::mutate(lab.ypos = cumsum(prop) - 0.5*prop) %>%
    data.frame()
  data.plot
  
  mycols <- c("gold", "#0073C2FF", "black", "darkgreen", "lightblue", "red", "blue", "darkgrey", "lightgrey", 1:10)
  fig.temp <- ggplot(data.plot, aes(x = 2, y = prop, fill = value)) +
    geom_bar(stat = "identity", color = "white") +
    coord_polar(theta = "y", start = 0)+
    geom_text(aes(y = lab.ypos, label = paste0("n = ", n, ", \n", round(prop*100,0), "%")), color = "white")+
    facet_wrap(.~taxa) +
    scale_fill_manual(values = mycols, name=standard.plot) +
    theme_void() +
    xlim(.9, 2.5)
  print(fig.temp)
}

##for loop that creates a donut chart for each standard and saves it
standards<-unique(data.qual$standard)
for (j in 1:length(standards)) {
  png(filename = paste0("Output/fig.", standards[j],".png"))
  donut.plot(data.plot = data.qual, standard.plot = standards[j])
  dev.off()
}