##select primary subset global ecosystems
##M Tarjan
##Oct 11, 2022
##query modified from K Snow
##STEP 1a - this document includes the query from biotics and data wrangling. steps after this for ecosystems are plot-standards (portion of the code) and knitting the markdown doc.

# Load RODBC package
library(RODBC)
library(tidyverse)

##NEED TO FIRST CONNECT TO VPN
##open connection to database; Jan 2022 snapshot
#con<-odbcConnect("BIOSNAPDB07", uid="biotics_report", pwd=rstudioapi::askForPassword("Password"))
##open connection to database; Jan 2023 snapshot
con<-odbcConnect("BIOSNAPDB08", uid="biotics_report", pwd=rstudioapi::askForPassword("Password"))
#sqlTables(con) ##show tables in the database

#/* BDCS ECOSYSTEM Global Elements 'Primary Subset'
#--CRITERIA (under review):
#  --IVC Groups, Alliances, and Associations 
#--Confident (including GX) in US (not including territories) & Canada (what about GH?)
#--Standard or Provisional, but exclude CEPP (a subset of Provisional)
#--NOT ruderal
#--NOT Archive Candidate
#--NOTE: returns classification level instead of name category like species query does
#*/

qry <- "SELECT egt.element_global_id, gname.scientific_name, egt.g_primary_common_name, gname.d_classification_level_id, dcl.classification_level_name, egt.rounded_g_rank, egr.d_rank_method_used_id, rmu.rank_method_used_desc, rmu.external_desc, egr.g_rank_reasons, EGT.G_RANK_REVIEW_DATE
FROM  element_global egt
INNER JOIN scientific_name gname ON egt.gname_id = gname.scientific_name_id
INNER JOIN d_classification_level dl ON gname.d_classification_level_id = dl.d_classification_level_id
LEFT JOIN element_global_rank egr ON egt.element_global_id = egr.element_global_id
LEFT JOIN d_rank_method_used rmu ON egr.d_rank_method_used_id = rmu.d_rank_method_used_id
LEFT JOIN d_classification_level dcl ON gname.d_classification_level_id = dcl.d_classification_level_id
WHERE not exists(select 1 from element_national where d_classif_confidence_id=-1 and element_global_id = egt.element_global_id) --exclude archive candidates
  and egt.element_global_id in 
      (select en.element_global_id from element_national en join community_national cn on en.element_national_id = cn.element_national_id 
         and cn.d_dist_confidence_id = 1         --confident in present or past (GX)
         and en.nation_id in (38,225))           --Canada and US
  and gname.d_classification_level_id in(47,46,106) -- Association, Alliance, Group
  and egt.elcode_bcd not like 'CEPP%' 
  and egt.g_rank != 'GNA'                        -- not ruderal
  and egt.d_classification_status_id in(1,3)     --standard or provisional
  and egt.d_maintained_by_status_id = 1          --Centrally maintained
  and egt.inactive_ind = 'N'"

eco.global<-sqlQuery(con, qry); head(eco.global) ##import the queried table

# When finished, it's a good idea to close the connection
odbcClose(con)

##WRANGLE ECOSYSTEM DATA
##Use the following categories: Imperiled (rounded G1 and G2), Vulnerable (rounded G3), or Apparently Secure (rounded G4 and G5), GU, G-Other (GX)
##groups, alliances, associations
##Group G ranks
eco.global <- eco.global %>% dplyr::mutate(G_RANK = dplyr::case_when(
  ROUNDED_G_RANK %in% c("G1", "G2") ~ "Imperiled",
  ROUNDED_G_RANK %in% c("G3") ~ "Vulnerable",
  ROUNDED_G_RANK %in% c("G4", "G5") ~ "Apparently\nSecure",
  ROUNDED_G_RANK %in% c("GU") ~ "GU"#,
  #ROUNDED_G_RANK %in% c("GX", "GH") ~ "G-Other"
))
eco.global$G_Rank <- ifelse(eco.global$ROUNDED_G_RANK =="GNR", F, T)
eco.global$Rank_Method <- ifelse(is.na(eco.global$D_RANK_METHOD_USED_ID), F, T)
eco.global$Rank_Calculator <- ifelse(!str_detect(string = eco.global$RANK_METHOD_USED_DESC, pattern = "Rank calculation") | is.na(eco.global$RANK_METHOD_USED_DESC), F, T)
eco.global$Rank_Reason <- ifelse(is.na(eco.global$G_RANK_REASONS), F, T)
eco.global$G_Rank_Review_Date <- ifelse(eco.global$G_RANK_REVIEW_DATE > Sys.Date()-365*10, T, F)
eco.global$Year<- lubridate::year(eco.global$G_RANK_REVIEW_DATE) %>% as.numeric()

write.csv(eco.global, "Output/PrimarySubsetGlobalEcosystems.csv", row.names=F)

##summarize by "taxa" (groups, alliances, associations)
eco.sum.group <- eco.global %>% gather(key = "standard", value = "value", G_Rank:G_Rank_Review_Date) %>% group_by(CLASSIFICATION_LEVEL_NAME, standard, value) %>% summarise(n=n()) %>% rename(taxa = CLASSIFICATION_LEVEL_NAME) %>% dplyr::arrange(standard, taxa, value) %>% subset(!is.na(value)) %>% data.frame()

##convert counts into proportions of cases that are T/F for each standard and plants vs animals
eco.prop<-eco.sum.group %>% dplyr::group_by(standard, taxa) %>% dplyr::summarise(prop = n/sum(n)) %>% data.frame()
eco.sum.group$prop<-eco.prop$prop

##summarize by taxa/g-rank group
eco.sum.grank <- subset(eco.global, !is.na(G_RANK), select=-G_Rank) %>% gather(key = "standard", value = "value", Rank_Method:G_Rank_Review_Date) %>% group_by(CLASSIFICATION_LEVEL_NAME, G_RANK, standard, value) %>% summarise(n=n()) %>% rename(taxa = CLASSIFICATION_LEVEL_NAME) %>% dplyr::arrange(standard, G_RANK, taxa, value) %>% data.frame()

##convert counts into proportions of cases that are T/F for each standard and plants vs animals
eco.prop<-eco.sum.grank %>% dplyr::group_by(standard, G_RANK, taxa) %>% dplyr::summarise(prop = n/sum(n)) %>% data.frame()
eco.sum.grank$prop<-eco.prop$prop

write.csv(eco.sum.group, paste0("Output/data.qual.ecosystems.taxa.",Sys.Date(),".csv"), row.names=F)
write.csv(eco.sum.grank, paste0("Output/data.qual.ecosystems.grank.",Sys.Date(),".csv"), row.names=F)
