##step 6
##get grank from original subset and add it to data.qual

library(tidyverse)

egt.global<-read.csv("Output/PrimarySubsetGlobal.csv")

##add grank completedness to data.qual
dat <- egt.global
dat$taxa<-NA
dat$taxa[which(dat$NAME_CATEGORY_DESC %in% c("Invertebrate Animal", "Vertebrate Animal"))]<-"Animals"
dat$taxa[which(dat$NAME_CATEGORY_DESC == "Vascular Plant")]<-"Plants"
dat$G_Rank<-T
dat$G_Rank[which(dat$ROUNDED_G_RANK %in% c("GNR", "TNR"))] <- F
dat2 <- table(dat$taxa, dat$G_Rank) %>% data.frame()

colnames(dat2) <- c("group", "value", "n")
dat2$standard <- "G_Rank"
dat2$group.type<-"taxa"
##remove previous values if needed
#data.qual<-subset(data.qual, standard!="G_Rank", select = -prop)
data.qual <- rbind(data.qual, dat2)

##convert counts into proportions of cases that are T/F for each standards and plants vs animals
data.qual<-dplyr::arrange(.data = data.qual, standard, group, group.type, value)
data.qual.prop<-data.qual %>% dplyr::group_by(standard, group, group.type) %>% dplyr::summarise(prop = n/sum(n)) %>% data.frame()
data.qual$prop<-data.qual.prop$prop

##write out dataset
write.csv(data.qual, paste0("Output/data.qual.",Sys.Date(),".csv"), row.names=F)

##add years to dat.rank
dat.rank$Year<-format(dat.rank$G_RANK_REVIEW_DATE2, format = "%Y") %>% as.numeric()
write.csv(dat.rank, "Output/data.rank.csv", row.names=F)

##check the number of observations for each standard
subset(data.qual, group.type == "taxa") %>% group_by(standard) %>% summarise(sum(n))
