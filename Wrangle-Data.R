##step 5
##get grank from original subset and add it to data.qual

library(tidyverse)

egt.global<-read.csv("Output/PrimarySubsetGlobal.csv") %>% filter(native)

## add info on models
library(SHMDB)
### SHM database
con <- shmdb_connect()
shms <- dbGetQuery(con, "SELECT * FROM project_outputs")
shms <- shms %>% filter(!is.na(model_name) & !(model_status %in% c("N/A", "model_defunct", "archived", "model_in_development"))) %>% select(element_global_id, scientific_name, common_name, model_status) %>% unique()

dat <- egt.global %>% mutate(Habitat_Model = ifelse(ELEMENT_GLOBAL_ID %in% shms$element_global_id, T, F))

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
  (Sys.Date() - dat$G_RANK_REVIEW_DATE2) > 365*10 | is.na(dat$G_RANK_REVIEW_DATE2) ~ ">10 years"
))

## define rank method reason date by T/F
dat$Rank_Method<-F
dat$Rank_Method[which(!is.na(dat$D_RANK_METHOD_USED_ID))]<-T
dat$Rank_Calculator<-F
dat$Rank_Calculator[which(dat$EXTERNAL_DESC %in% c("Ranked by calculator", "Calculated rank revised by expert"))]<-T
dat$Rank_Reason<-F
dat$Rank_Reason[which(!is.na(dat$G_RANK_REASONS))]<-T
#dat<-subset(dat, select = -G_RANK_REASONS)
##add years to dat
dat$Year<-format(dat$G_RANK_REVIEW_DATE2, format = "%Y") %>% as.numeric()

write.csv(dat, "Output/PrimarySubsetGlobal.csv", row.names=F)

dat<-read.csv("Output/PrimarySubsetGlobal.csv")

##get dataset for taxa
data.qual.taxa <- dat %>%
  filter(!is.na(taxa) & !(G_RANK %in% c("GNR/TNR", "GNA/TNA"))) %>%
  gather(key = "standard", value = "value", c(Habitat_Categories, Rank_Method, Rank_Calculator, Rank_Reason)) %>% 
  group_by(taxa, standard, value) %>% 
  summarise(n=n()) %>% 
  data.frame()
##remove gx/tx for rank review date
data.qual.taxa <- subset(dat, !is.na(taxa) & !(G_RANK %in% c("GNR/TNR", "GNA/TNA", "GX/TX"))) %>% 
  gather(key = "standard", value = "value", G_Rank_Review_Date) %>% 
  group_by(taxa, standard, value) %>% 
  summarise(n=n()) %>% 
  data.frame() %>% 
  rbind(data.qual.taxa)
##remove entries that have never had a rank change when evaluating % with rank change reason
data.qual.taxa <- subset(dat, !is.na(taxa) & !(G_RANK %in% c("GNR/TNR", "GNA/TNA")) & !is.na(Rank_Change_Reason)) %>% 
  gather(key = "standard", value = "value", Rank_Change_Reason) %>% 
  group_by(taxa, standard, value) %>% 
  summarise(n=n()) %>% 
  data.frame() %>% 
  rbind(data.qual.taxa)
data.qual.taxa <- subset(dat, !is.na(taxa) & G_RANK %in% c("G1/T1", "G2/T2", "G3/T3", "GH/TH")) %>% 
  gather(key = "standard", value = "value", Threat_Category) %>% 
  group_by(taxa, standard, value) %>% 
  summarise(n=n()) %>% 
  data.frame() %>% 
  rbind(data.qual.taxa)
data.qual.taxa <- subset(dat, !is.na(taxa)) %>% 
  gather(key = "standard", value = "value", G_Rank) %>% 
  group_by(taxa, standard, value) %>% 
  summarise(n=n()) %>% 
  data.frame() %>% 
  rbind(data.qual.taxa)
if (any(names(dat) %in% "Habitat_Model")) {
  data.qual.taxa <- subset(dat, !is.na(taxa) & G_RANK %in% c("G1/T1", "G2/T2")) %>% 
    gather(key = "standard", value = "value", Habitat_Model) %>% 
    group_by(taxa, standard, value) %>% 
    summarise(n=n()) %>% 
    data.frame() %>% 
    rbind(data.qual.taxa)
}

##convert counts into proportions of cases that are T/F for each standard and plants vs animals
data.qual.taxa<-dplyr::arrange(.data = data.qual.taxa, standard, taxa, value)
data.qual.prop<-data.qual.taxa %>% dplyr::group_by(standard, taxa) %>% dplyr::summarise(prop = n/sum(n)) %>% data.frame()
data.qual.taxa$prop<-data.qual.prop$prop

##get dataset for combo of grank and taxa
data.qual.grank <- subset(dat, !is.na(taxa)& !(G_RANK %in% c("GNR/TNR", "GNA/TNA"))) %>% gather(key = "standard", value = "value", c(Habitat_Categories, Rank_Method, Rank_Calculator, Rank_Reason)) %>% group_by(taxa, G_RANK, standard, value) %>% summarise(n=n()) %>% data.frame()
##remove GX/TX for rank review date
data.qual.grank <- subset(dat, !is.na(taxa)& !(G_RANK %in% c("GNR/TNR", "GNA/TNA", "GX/TX"))) %>% gather(key = "standard", value = "value", G_Rank_Review_Date) %>% group_by(taxa, G_RANK, standard, value) %>% summarise(n=n()) %>% data.frame() %>% rbind(data.qual.grank)
##remove entries that have never had a rank change when evaluating % with rank change reason
data.qual.grank <- subset(dat, !is.na(taxa)& !(G_RANK %in% c("GNR/TNR", "GNA/TNA")) & !is.na(Rank_Change_Reason)) %>% gather(key = "standard", value = "value", Rank_Change_Reason) %>% group_by(taxa, G_RANK, standard, value) %>% summarise(n=n()) %>% data.frame() %>% rbind(data.qual.grank)
##only evaluate G1-3 and H for threat category
data.qual.grank <- subset(dat, !is.na(taxa) & G_RANK %in% c("G1/T1", "G2/T2", "G3/T3", "GH/TH")) %>% gather(key = "standard", value = "value", Threat_Category) %>% group_by(taxa, G_RANK, standard, value) %>% summarise(n=n()) %>% data.frame() %>% rbind(data.qual.grank)

if (any(names(dat) %in% "Habitat_Model")) {
  ##only evaluate G1 G2 for habitat models
  data.qual.grank <- subset(dat, !is.na(taxa) & G_RANK %in% c("G1/T1", "G2/T2")) %>% gather(key = "standard", value = "value", Habitat_Model) %>% group_by(taxa, G_RANK, standard, value) %>% summarise(n=n()) %>% data.frame() %>% rbind(data.qual.grank)
}

##convert counts into proportions of cases that are T/F for each standards and plants vs animals
data.qual.grank<-dplyr::arrange(.data = data.qual.grank, standard, taxa, G_RANK, value)
data.qual.prop<-data.qual.grank %>% dplyr::group_by(standard, taxa, G_RANK) %>% dplyr::summarise(prop = n/sum(n)) %>% data.frame()
data.qual.grank$prop<-data.qual.prop$prop

##write out dataset
write.csv(data.qual.taxa, paste0("Output/data.qual.taxa.",Sys.Date(),".csv"), row.names=F)
write.csv(data.qual.grank, paste0("Output/data.qual.grank.",Sys.Date(),".csv"), row.names=F)

##check the number of observations for each standard
#subset(data.qual, group.type == "taxa") %>% group_by(standard) %>% summarise(sum(n))
