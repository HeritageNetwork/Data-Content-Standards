##Step 4
##Select-ThreatCatgory
##Threat categories indicated, even if assessed and found to be “unknown” (GH/G1/G2/G3 and TH/T1/T2/T3 taxa only).

con<-odbcConnect("BIOSNAPDB07", uid="biotics_report", pwd=rstudioapi::askForPassword("Password")) ##open connection to database

##put a loop around query and use rbind to get more than 999 records
id.vector<-egt.global$ELEMENT_GLOBAL_ID
max.length <- 999
x <- 1
y <- min(c(max.length,length(id.vector)))
dat<-dim(0)
for (j in 1:ceiling((length(id.vector)/max.length))) {
  id.temp<-paste0("(", paste0(id.vector[x:y], collapse = ", "), ")")
  #print(x); print(y)
  ##then query the database and write out to vector that gets added to
  
  ##get all element global ids and g ranks
  qry <- paste0("SELECT EGT.element_global_id, sn.NAME_CATEGORY, egt.rounded_g_rank
  
  from element_global egt, scientific_name_dvw sn
    where (egt.element_global_id IN ", id.temp, ")
      and egt.gname_id = sn.scientific_name_id")
  
  dat.temp<-sqlQuery(con, qry)
  
  ##then get element ids with rank methods
  qry <- paste0("SELECT EGT.element_global_id, sn.NAME_CATEGORY, eta.d_iucn_threat_category_id
  
  from element_global egt, scientific_name_dvw sn, el_global_threats_assess eta
    where (egt.element_global_id IN ", id.temp, ")
      and egt.gname_id = sn.scientific_name_id
  and egt.element_global_id = eta.element_global_id")
  
  dat.temp2<-sqlQuery(con, qry) ##import the queried table
  ##join the two tables
  dat.temp3<-dplyr::left_join(dat.temp, dat.temp2)
  dat.temp3$Threat_Category<-F
  dat.temp3$Threat_Category[which(!is.na(dat.temp3$D_IUCN_THREAT_CATEGORY_ID))]<-T
  
  ##summarise in the loop so dataframe doesn't get too big
  dat.temp<- table(subset(dat.temp3, select = c(NAME_CATEGORY, ROUNDED_G_RANK, Threat_Category))) %>% data.frame()
  dat.temp$x<-x
  dat.temp$y<-y
  dat<-rbind(dat, dat.temp)
  x <- y +1
  y <- min(c(x-1+max.length,length(id.vector)))
}

# When finished, it's a good idea to close the connection
odbcClose(con)

##summarize across looped ids
dat2<-dat
dat2 <- subset(dat2, ROUNDED_G_RANK %in% c('G1', 'G2', 'G3', 'GH', 'T1', 'T2', 'T3', 'TH'))
dat2$taxa<-NA
dat2$taxa[which(dat2$NAME_CATEGORY %in% c("Invertebrate Animal", "Vertebrate Animal"))]<-"Animals"
dat2$taxa[which(dat2$NAME_CATEGORY == "Vascular Plant")]<-"Plants"
##Group G and T ranks
dat2 <- dat2 %>% dplyr::mutate(G_RANK = dplyr::case_when(
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
dat3<-subset(dat2, taxa %in% c("Animals", "Plants")) %>% dplyr::group_by(taxa, Threat_Category) %>% dplyr::summarise(n=sum(Freq), group.type="taxa") %>% data.frame()
colnames(dat3) <- c("group", "value", "n", "group.type")
dat3$standard<-"Threat_Category"

dat4<-subset(dat2, taxa %in% c("Animals", "Plants")) %>% dplyr::group_by(G_RANK, Threat_Category) %>% dplyr::summarise(n=sum(Freq), group.type="G_Rank") %>% data.frame()
colnames(dat4) <- c("group", "value", "n", "group.type")
dat4$standard<-"Threat_Category"

##if want to replace what's already in the main data table
data.qual<-subset(data.qual, !standard=="Threat_Category") ##first remove existing values

data.qual<-rbind(data.qual, subset(dat3, select= names(data.qual)))
data.qual<-rbind(data.qual, subset(dat4, select= names(data.qual)))
