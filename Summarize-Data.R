## Summarize data

dat<-read.csv("Output/PrimarySubsetGlobal.csv") %>% filter(!is.na(taxa))

##get dataset for taxa
data.qual.taxa <- dat %>%
  filter(!G_RANK %in% c("GNR/TNR", "GNA/TNA")) %>%
  pivot_longer(cols = c(Habitat_Categories, Rank_Method, Rank_Calculator, Rank_Reason), 
               names_to = "standard", values_to = "value") %>%
  count(taxa, standard, value) %>%
  as.data.frame()

##remove gx/tx for rank review date
data.qual.taxa <- subset(dat, !(G_RANK %in% c("GNR/TNR", "GNA/TNA", "GX/TX"))) %>% 
  gather(key = "standard", value = "value", G_Rank_Review_Date) %>% 
  group_by(taxa, standard, value) %>% 
  summarise(n=n()) %>% 
  data.frame() %>% 
  rbind(data.qual.taxa)
##remove entries that have never had a rank change when evaluating % with rank change reason
data.qual.taxa <- subset(dat, !(G_RANK %in% c("GNR/TNR", "GNA/TNA")) & !is.na(Rank_Change_Reason)) %>% 
  gather(key = "standard", value = "value", Rank_Change_Reason) %>% 
  group_by(taxa, standard, value) %>% 
  summarise(n=n()) %>% 
  data.frame() %>% 
  rbind(data.qual.taxa)
data.qual.taxa <- subset(dat, G_RANK %in% c("G1/T1", "G2/T2", "G3/T3", "GH/TH")) %>% 
  gather(key = "standard", value = "value", Threat_Category) %>% 
  group_by(taxa, standard, value) %>% 
  summarise(n=n()) %>% 
  data.frame() %>% 
  rbind(data.qual.taxa)
data.qual.taxa <- dat %>% 
  gather(key = "standard", value = "value", G_Rank) %>% 
  group_by(taxa, standard, value) %>% 
  summarise(n=n()) %>% 
  data.frame() %>% 
  rbind(data.qual.taxa)
if (any(names(dat) %in% "Habitat_Model")) {
  data.qual.taxa <- subset(dat, G_RANK %in% c("G1/T1", "G2/T2")) %>% 
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
data.qual.grank <- subset(dat, !(G_RANK %in% c("GNR/TNR", "GNA/TNA"))) %>% gather(key = "standard", value = "value", c(Habitat_Categories, Rank_Method, Rank_Calculator, Rank_Reason)) %>% group_by(taxa, G_RANK, standard, value) %>% summarise(n=n()) %>% data.frame()
##remove GX/TX for rank review date
data.qual.grank <- subset(dat, !(G_RANK %in% c("GNR/TNR", "GNA/TNA", "GX/TX"))) %>% gather(key = "standard", value = "value", G_Rank_Review_Date) %>% group_by(taxa, G_RANK, standard, value) %>% summarise(n=n()) %>% data.frame() %>% rbind(data.qual.grank)
##remove entries that have never had a rank change when evaluating % with rank change reason
data.qual.grank <- subset(dat, !(G_RANK %in% c("GNR/TNR", "GNA/TNA")) & !is.na(Rank_Change_Reason)) %>% gather(key = "standard", value = "value", Rank_Change_Reason) %>% group_by(taxa, G_RANK, standard, value) %>% summarise(n=n()) %>% data.frame() %>% rbind(data.qual.grank)
##only evaluate G1-3 and H for threat category
data.qual.grank <- subset(dat, G_RANK %in% c("G1/T1", "G2/T2", "G3/T3", "GH/TH")) %>% gather(key = "standard", value = "value", Threat_Category) %>% group_by(taxa, G_RANK, standard, value) %>% summarise(n=n()) %>% data.frame() %>% rbind(data.qual.grank)

if (any(names(dat) %in% "Habitat_Model")) {
  ##only evaluate G1 G2 for habitat models
  data.qual.grank <- subset(dat, G_RANK %in% c("G1/T1", "G2/T2")) %>% gather(key = "standard", value = "value", Habitat_Model) %>% group_by(taxa, G_RANK, standard, value) %>% summarise(n=n()) %>% data.frame() %>% rbind(data.qual.grank)
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
