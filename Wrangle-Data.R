##step 5
##get grank from original subset and add it to data.qual

library(tidyverse)

egt.global<-read.csv("Output/PrimarySubsetGlobal.csv")

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
