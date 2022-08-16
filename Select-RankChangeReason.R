##step 5

##Indication of the reason for the most recent rank change. 
##Percent of elements with rank change reasons indicated 

##need rank change date (to find the most recent change)?? and rank change reason
##ELEMENT_GLOBAL.g_rank_change_date
##element_grank_change.rank_change_entry_date, d_rank_change_reason_id (na or not na) ##this is the table from margaret that has the grank change reason
##start with identical date; might do +/- a timeframe to find matches in the future

con<-odbcConnect("BIOSNAPDB07", uid="biotics_report", pwd=rstudioapi::askForPassword("Password")) ##open connection to database

##put a loop around query and use rbind to get more than 999 records
id.vector<-egt.global$ELEMENT_GLOBAL_ID
max.length <- 999
x <- 1
y <- min(c(max.length,length(id.vector)))
dat<-dim(0)
for (j in 1:ceiling((length(id.vector)/max.length))) {
  id.temp<-paste0("(", paste0(id.vector[x:y], collapse = ", "), ")")
  ##then query the database and write out to vector that gets added to
  
  ##get all element global ids and g ranks
  qry <- paste0("SELECT EGT.element_global_id, sn.NAME_CATEGORY, egt.rounded_g_rank, egt.g_rank_change_date
  from element_global egt, scientific_name_dvw sn
    where (egt.element_global_id IN ", id.temp, ")
      and egt.gname_id = sn.scientific_name_id")
  
  dat.temp<-sqlQuery(con, qry)
  
  ##then get element ids with rank change reasons; ASSUMES THAT DATES MATCH PERFECTLY FOR G RANK CHANGE AND ENTRY DATES
  qry <- paste0("SELECT EGT.element_global_id, sn.NAME_CATEGORY, egt.rounded_g_rank, egt.g_rank_change_date, rc.rank_change_entry_date, rc.d_rank_change_reason_id
  from element_global egt, scientific_name_dvw sn, element_grank_change rc
    where (egt.element_global_id IN ", id.temp, ")
      and egt.gname_id = sn.scientific_name_id
      and rc.rank_change_entry_date = EGT.g_rank_change_date
      and rc.element_global_id = egt.element_global_id")
  
  dat.temp2<-sqlQuery(con, qry) ##import the queried table
  ##join the two tables
  dat.temp3<-dplyr::left_join(dat.temp, dat.temp2)
  dat.temp3$Rank_Change_Reason<-F
  dat.temp3$Rank_Change_Reason[which(!is.na(dat.temp3$D_RANK_CHANGE_REASON_ID))]<-T
  
  ##summarise in the loop so dataframe doesn't get too big
  dat.temp<- table(subset(dat.temp3, select = c(NAME_CATEGORY, ROUNDED_G_RANK, Rank_Change_Reason))) %>% data.frame()
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

dat3<-subset(dat2, taxa %in% c("Animals", "Plants")) %>% dplyr::group_by(taxa, Rank_Change_Reason) %>% dplyr::summarise(n=sum(Freq), group.type="taxa") %>% data.frame()
colnames(dat3) <- c("group", "value", "n", "group.type")
dat3$standard<-"Rank_Change_Reason"

dat4<-subset(dat2, taxa %in% c("Animals", "Plants")) %>% dplyr::group_by(G_RANK, Rank_Change_Reason) %>% dplyr::summarise(n=sum(Freq), group.type="G_Rank") %>% data.frame()
colnames(dat4) <- c("group", "value", "n", "group.type")
dat4$standard<-"Rank_Change_Reason"

##if want to replace what's already in the main data table
#data.qual<-subset(data.qual, !standard=="Rank_Change_Reason", select = -prop) ##first remove existing values

data.qual<-rbind(data.qual, subset(dat3, select= names(data.qual)))
data.qual<-rbind(data.qual, subset(dat4, select= names(data.qual)))
