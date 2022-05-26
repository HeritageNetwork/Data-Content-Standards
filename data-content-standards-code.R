##data content standards
##M Tarjan
##April 11, 2022

##Jan 2022 snapshot
##biotics fields: https://natureserve01-my.sharepoint.com/:x:/g/personal/shara_howie_natureserve_org/EYznjVG_BAJGi40urSIos_kBbKaNQ0gYaaQ0Uyy8AutZ1A?e=J0HS2O&CID=01d1f82e-5dbc-3e50-37b6-635d6815158a
##example of reticulate package: http://zevross.com/blog/2019/11/12/using-reticulate-with-arcpy-on-a-windows-machine/

##use netextender to connect to VPN
##start by installing the package
#install.packages("arcgisbinding", repos="https://r.esri.com", type="win.binary")

## Load libraries
#library(sf)
library(tidyverse)
library(ggplot2)
#library(natserv) ##to connect to api
library(arcgisbinding)
## Check ArcGIS Pro product is installed locally
arc.check_product()

##BIOTICS 5 database
##Instructions for connecting to biotics 5 database: https://help.natureserve.org/biotics/#ConnectingToBiotics5/CONN_Overview_Connecting.htm%3FTocPath%3DConnecting%2520to%2520Biotics%25205%7C_____0
##instructions for connecting to database through r: https://www.r-bloggers.com/2017/05/connecting-r-to-an-oracle-database/

##spatial snapshot is in folder: https://natureserve.atlassian.net/wiki/spaces/BIOTICS5/pages/3093660323/Biotics+Servers

##get data from api
#biotics<-ns_export()
#res<-ns_export_status(biotics) ##check this line until it is complete and you can save the url
#biotics<-tibble::as_tibble(jsonlite::fromJSON(res$data$url))

#biotics.animals<-ns_export(species_taxonomy = list(scientificTaxonomy = "Animalia", level = "kingdom"))
#res<-ns_export_status(biotics.animals) ##check this line until it is complete and you can save the url
#animal<-tibble::as_tibble(jsonlite::fromJSON(res$data$url))

#biotics.plants<-ns_export(species_taxonomy = list(scientificTaxonomy = "Plantae", level = "kingdom"))

##subset biotics data
#usesaCode, cosewicCode, saraCode, gRank

##biotics snapshot
snap<-arc.open("S:/Data/NatureServe/BLD_Occurrences/NS_BLD_GeoDB/Snapshots/Monthly-2022-01/bld-2022-01.gdb/BLD_EO_SPECIES")
##subset arc object before converting it to R object
##run this subset by margaret/cameron
snap.sub<-arc.select(snap, 
                     where_clause = "MAJ_GRP1 = 'Vascular Plants - Flowering Plants'
                     OR MAJ_GRP1 = 'Vascular Plants - Ferns and relatives'
                     OR MAJ_GRP1 = 'Vascular Plants - Conifers and relatives'
                     OR MAJ_GRP1 = 'Vertebrates'
                     OR MAJ_GRP1 = 'Insects - Bees'
                     OR MAJ_GRP2 = 'Butterflies and Skippers'
                     OR MAJ_GRP2 = 'Caddisflies'
                     OR MAJ_GRP2 = 'Crayfishes'
                     OR MAJ_GRP2 = 'Dragonflies and Damselflies'
                     OR MAJ_GRP2 = 'Freshwater Mussels'
                     OR MAJ_GRP3 = 'Freshwater Snails'
                     OR MAJ_GRP3 = 'Terrestrial Snails'
                     OR USESA_CD IS NOT NULL
                     OR SARA IS NOT NULL
                     OR G_INT_ESA IS NOT NULL
                     OR COSEWIC IS NOT NULL"
                     )

#snap.sf<-arc.data2sf(snap.sub) ##convert to sf object
##add whether plant or animal
snap.sub2<-snap.sub
##add whether a plant or animal
snap.sub2$taxa<-ifelse(snap.sub2$MAJ_GRP1 %in% c("Vascular Plants - Flowering Plants", "Vascular Plants - Ferns and relatives", "Vascular Plants - Conifers and relatives"), "Plant", "Animal")
##add whether has Grank or not (T/F)
snap.sub2$has.Grank<-ifelse(is.na(snap.sub2$G_RANK) | snap.sub2$G_RANK %in% c("GNR", "TNR", "GNA", "TNA"), F, T)
##can add additional columns with categorical states (T/F)
##gather the T/F columns into two columns with the name of the standard and whether the standard is met for the record or not (T/F)
snap.sub2<-snap.sub2 %>% gather(key = "standard", value = "value", (ncol(snap.sub)+2):ncol(snap.sub2))

##summarize data to get a count of T/F cases for each standard by plants, animals, and all
##get counts for plants and animals
data.qual <- snap.sub2 %>% group_by(taxa, standard) %>% count(value) %>% data.frame()
##get count for all taxa
data.qual.temp<-snap.sub2 %>% group_by(standard) %>% count(value) %>% data.frame()
data.qual.temp$taxa<-"All"
##combine counts of T/F cases for plants, animals, and all
data.qual<-rbind(data.qual, subset(data.qual.temp, select=names(data.qual)))

##convert counts into proportions of cases that are T/F for each standards and plants vs animals vs all
data.qual.prop<-data.qual %>% group_by(taxa, standard) %>% summarise(prop = n/sum(n)) %>% data.frame()
data.qual$prop<-data.qual.prop$prop

##create function to make donut charts
donut.plot <- function(data.plot, standard.plot) {
  ##get the label positions
  data.plot <- subset(data.plot, standard=standard.plot) %>%
    group_by(taxa) %>%
    arrange(desc(prop)) %>%
    mutate(lab.ypos = cumsum(prop) - 0.5*prop) %>%
    data.frame()
  data.plot
  
  mycols <- c("gold", "#0073C2FF")
  fig.temp <- ggplot(data.plot, aes(x = 2, y = prop, fill = value)) +
    geom_bar(stat = "identity", color = "white") +
    coord_polar(theta = "y", start = 0)+
    geom_text(aes(y = lab.ypos, label = paste0("n = ", n, ", \n", round(prop*100,0), "%")), color = "white")+
    facet_wrap(.~taxa) +
    scale_fill_manual(values = mycols, name=standard.plot) +
    theme_void() +
    xlim(.5, 2.5)
  fig.temp
}

##All element global records should have: 1. A global conservation status assessment (G-rank or T-rank other than GNR or TNR).
##donut chart for all, plants, animals of % with rank and % without rank

##for loop that creates a donut chart for each standard and saves it
standards<-unique(data.qual$standard)
for (j in 1:length(standards)) {
  donut.plot(data.plot = data.qual, standard.plot = standards[j]) ##needs to have n and data.group
  png(filename = str_c("/fig.", standards[j],".png"), units="in", res=200);print(fig.temp); dev.off()
}
