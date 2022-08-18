##step 5
##get grank from original subset and add it to data.qual

library(tidyverse)

egt.global<-read.csv("Output/PrimarySubsetGlobal.csv")
dat <- egt.global

##define taxa
dat$taxa<-NA
dat$taxa[which(dat$NAME_CATEGORY_DESC %in% c("Invertebrate Animal", "Vertebrate Animal"))]<-"Animals"
dat$taxa[which(dat$NAME_CATEGORY_DESC == "Vascular Plant")]<-"Plants"
dat$G_Rank<-T
dat$G_Rank[which(dat$ROUNDED_G_RANK %in% c("GNR", "TNR"))] <- F

##Group G and T ranks
dat <- dat %>% dplyr::mutate(G_RANK = dplyr::case_when(
  ROUNDED_G_RANK %in% c("G1", "T1") ~ "G1/T1",
  ROUNDED_G_RANK %in% c("G2", "T2") ~ "G2/T2",
  ROUNDED_G_RANK %in% c("G3", "T3") ~ "G3/T3",
  ROUNDED_G_RANK %in% c("G4", "T4") ~ "G4/T4",
  ROUNDED_G_RANK %in% c("G5", "T5") ~ "G5/T5",
  ROUNDED_G_RANK %in% c("GH", "TH") ~ "GH/TH",
  ROUNDED_G_RANK %in% c("GX", "TX") ~ "GX/TX",
  ROUNDED_G_RANK %in% c("GNA", "TNA") ~ "GNA/TNA",
  ROUNDED_G_RANK %in% c("GNR", "TNR") ~ "GNR/TNR",
  ROUNDED_G_RANK %in% c("GU", "TU") ~ "GU/TU"
))
##translate rank review dates into categories
dat$G_RANK_REVIEW_DATE2<-dat$G_RANK_REVIEW_DATE %>% as.character() %>% as.Date(format= "%Y-%m-%d")
dat <- dat %>% dplyr::mutate(G_Rank_Review_Date = dplyr::case_when(
  (Sys.Date() - dat$G_RANK_REVIEW_DATE2) <= 365*10 ~ "0-10 years",
  #(Sys.Date() - dat$G_RANK_REVIEW_DATE2) >= 365*5 & (Sys.Date() - dat$G_RANK_REVIEW_DATE2) < 365*10 ~ "5-10 years",
  (Sys.Date() - dat$G_RANK_REVIEW_DATE2) > 365*10 | is.na(dat$G_RANK_REVIEW_DATE2) ~ ">10 years"
))

## define rank method reason date by T/F
dat$Rank_Method<-F
dat$Rank_Method[which(!is.na(dat$D_RANK_METHOD_USED_ID))]<-T
dat$Rank_Calculator<-F
dat$Rank_Calculator[which(dat$EXTERNAL_DESC %in% c("Ranked by calculator", "Calculated rank revised by expert"))]<-T
dat$Rank_Reason<-F
dat$Rank_Reason[which(!is.na(dat$G_RANK_REASONS))]<-T
dat$Rank_Change_Reason<-F
dat$Rank_Change_Reason[which(!is.na(dat$D_RANK_CHANGE_REASON_ID))]<-T
dat<-subset(dat, select = -G_RANK_REASONS)
##add years to dat
dat$Year<-format(dat$G_RANK_REVIEW_DATE2, format = "%Y") %>% as.numeric()

write.csv(dat, "Output/PrimarySubsetGlobal.csv", row.names=F)

##wrangle data
standards<-c("Threat_Category", "Rank_Change_Reason", "Habitat_Categories", "G_Rank", "G_Rank_Review_Date", "Rank_Method", "Rank_Calculator", "Rank_Reason")

##data by taxa
data.qual<-dim(0)
dat.temp <- subset(dat, !is.na(taxa) & G_RANK %in% c("G1/T1", "G2/T2", "G3/T3", "GH/TH"), select = c(taxa, Threat_Category)) %>% group_by(taxa) %>% table() %>% data.frame()
names(dat.temp)<- c("group", "value", "n"); dat.temp$standard<-"Threat_Category"; dat.temp$group.type<-"taxa"
data.qual<-rbind(data.qual, dat.temp)

dat.temp <- subset(dat, !is.na(taxa), select = c(taxa, Rank_Change_Reason)) %>% group_by(taxa) %>% table() %>% data.frame()
names(dat.temp)<- c("group", "value", "n"); dat.temp$standard<-"Rank_Change_Reason"; dat.temp$group.type<-"taxa"
data.qual<-rbind(data.qual, dat.temp)

dat.temp <- subset(dat, !is.na(taxa), select = c(taxa, Habitat_Categories)) %>% group_by(taxa) %>% table() %>% data.frame()
names(dat.temp)<- c("group", "value", "n"); dat.temp$standard<-"Habitat_Categories"; dat.temp$group.type<-"taxa"
data.qual<-rbind(data.qual, dat.temp)

dat.temp <- subset(dat, !is.na(taxa), select = c(taxa, G_Rank)) %>% group_by(taxa) %>% table() %>% data.frame()
names(dat.temp)<- c("group", "value", "n"); dat.temp$standard<-"G_Rank"; dat.temp$group.type<-"taxa"
data.qual<-rbind(data.qual, dat.temp)

dat.temp <- subset(dat, !is.na(taxa), select = c(taxa, Rank_Method)) %>% group_by(taxa) %>% table() %>% data.frame()
names(dat.temp)<- c("group", "value", "n"); dat.temp$standard<-"Rank_Method"; dat.temp$group.type<-"taxa"
data.qual<-rbind(data.qual, dat.temp)

dat.temp <- subset(dat, !is.na(taxa), select = c(taxa, Rank_Calculator)) %>% group_by(taxa) %>% table() %>% data.frame()
names(dat.temp)<- c("group", "value", "n"); dat.temp$standard<-"Rank_Calculator"; dat.temp$group.type<-"taxa"
data.qual<-rbind(data.qual, dat.temp)

dat.temp <- subset(dat, !is.na(taxa), select = c(taxa, Rank_Reason)) %>% group_by(taxa) %>% table() %>% data.frame()
names(dat.temp)<- c("group", "value", "n"); dat.temp$standard<-"Rank_Reason"; dat.temp$group.type<-"taxa"
data.qual<-rbind(data.qual, dat.temp)

##by grank
dat.temp <- subset(dat, !is.na(taxa), select = c(G_RANK, Threat_Category)) %>% group_by(G_RANK) %>% table() %>% data.frame()
names(dat.temp)<- c("group", "value", "n"); dat.temp$standard<-"Threat_Category"; dat.temp$group.type<-"G_Rank"
data.qual<-rbind(data.qual, dat.temp)

dat.temp <- subset(dat, !is.na(taxa), select = c(G_RANK, Rank_Change_Reason)) %>% group_by(G_RANK) %>% table() %>% data.frame()
names(dat.temp)<- c("group", "value", "n"); dat.temp$standard<-"Rank_Change_Reason"; dat.temp$group.type<-"G_Rank"
data.qual<-rbind(data.qual, dat.temp)

dat.temp <- subset(dat, !is.na(taxa), select = c(G_RANK, Habitat_Categories)) %>% group_by(G_RANK) %>% table() %>% data.frame()
names(dat.temp)<- c("group", "value", "n"); dat.temp$standard<-"Habitat_Categories"; dat.temp$group.type<-"G_Rank"
data.qual<-rbind(data.qual, dat.temp)

dat.temp <- subset(dat, !is.na(taxa), select = c(G_RANK, G_Rank)) %>% group_by(G_RANK) %>% table() %>% data.frame()
names(dat.temp)<- c("group", "value", "n"); dat.temp$standard<-"G_Rank"; dat.temp$group.type<-"G_Rank"
data.qual<-rbind(data.qual, dat.temp)

dat.temp <- subset(dat, !is.na(taxa), select = c(G_RANK, Rank_Method)) %>% group_by(G_RANK) %>% table() %>% data.frame()
names(dat.temp)<- c("group", "value", "n"); dat.temp$standard<-"Rank_Method"; dat.temp$group.type<-"G_Rank"
data.qual<-rbind(data.qual, dat.temp)

dat.temp <- subset(dat, !is.na(taxa), select = c(G_RANK, Rank_Calculator)) %>% group_by(G_RANK) %>% table() %>% data.frame()
names(dat.temp)<- c("group", "value", "n"); dat.temp$standard<-"Rank_Calculator"; dat.temp$group.type<-"G_Rank"
data.qual<-rbind(data.qual, dat.temp)

dat.temp <- subset(dat, !is.na(taxa), select = c(G_RANK, Rank_Reason)) %>% group_by(G_RANK) %>% table() %>% data.frame()
names(dat.temp)<- c("group", "value", "n"); dat.temp$standard<-"Rank_Reason"; dat.temp$group.type<-"G_Rank"
data.qual<-rbind(data.qual, dat.temp)

##convert counts into proportions of cases that are T/F for each standards and plants vs animals
data.qual<-dplyr::arrange(.data = data.qual, standard, group, group.type, value)
data.qual.prop<-data.qual %>% dplyr::group_by(standard, group, group.type) %>% dplyr::summarise(prop = n/sum(n)) %>% data.frame()
data.qual$prop<-data.qual.prop$prop

##write out dataset
#write.csv(data.qual, paste0("Output/data.qual.",Sys.Date(),".csv"), row.names=F)

##check the number of observations for each standard
subset(data.qual, group.type == "taxa") %>% group_by(standard) %>% summarise(sum(n))
