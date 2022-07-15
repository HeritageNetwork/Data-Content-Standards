##step 2

##gather information about taxa
##nation, subnation, rounded grank, 

##NEED TO FIRST CONNECT TO VPN
##read in subset
#egt.global<-read.csv("Output/PrimarySubsetGlobal.csv")

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
  
  qry <- paste0("select tbl1.name_category, count(1) total
     , count(g1t1) num_g1t1
     , count(g2t2) num_g2t2
     , count(g3t3) num_g3t3
     , count(g4t4) num_g4t4
     , count(g5t5) num_g5t5
     , count(ghth) num_ghth
     , count(gxtx) num_gxtx
     , count(gnatna) num_gnatna
     , count(gnrtnr) num_gnrtnr
     , count(gutu) num_gutu
     from 
     (    select sn.NAME_CATEGORY, egt.rounded_g_rank,
       case 
         when egt.rounded_g_rank in ('G1', 'T1') then 'G1/T1'
         else null
       end g1t1,
       case 
         when egt.rounded_g_rank in ('G2', 'T2') then 'G2/T2'
         else null
       end g2t2,
       case 
         when egt.rounded_g_rank in ('G3', 'T3') then 'G3/T3'
         else null
       end g3t3,
       case 
         when egt.rounded_g_rank in ('G4', 'T4') then 'G4/T4'
         else null
       end g4t4,
       case 
         when egt.rounded_g_rank in ('G5', 'T5') then 'G5/T5'
         else null
       end g5t5,
       case 
         when egt.rounded_g_rank in ('GH', 'TH') then 'GH/TH'
         else null
       end ghth,
       case 
         when egt.rounded_g_rank in ('GX', 'TX') then 'GX/TX'
         else null
       end gxtx,
       case 
         when egt.rounded_g_rank in ('GNA', 'TNA') then 'GNA/TNA' 
         else null
       end gnatna,
       case 
         when egt.rounded_g_rank in ('GNR', 'TNR') then 'GNR/TNR'
         else null
       end gnrtnr,
       case 
         when egt.rounded_g_rank in ('GU', 'TU') then 'GU/TU'
         else null
       end gutu
    from element_global egt, scientific_name_dvw sn
    where (egt.element_global_id IN ", id.temp, ")
      and egt.gname_id = sn.scientific_name_id
    ) tbl1
    group by tbl1.name_category
    order by tbl1.name_category
    ")
  dat.temp<-sqlQuery(con, qry); head(dat.temp) ##import the queried table
  dat.temp$x<-x
  dat.temp$y<-y
  dat<-rbind(dat, dat.temp)
  x <- y +1
  y <- min(c(x-1+max.length,length(id.vector)))
}

#qry <- paste0("SELECT Decode(ent.nation_id, 225, 'US', 38, 'CA') as nation, subn.subnation_code, EGT.element_global_id, egt.rounded_g_rank, sn.NAME_CATEGORY

#FROM element_global egt, Element_national ent, 
#Element_subnational est, subnation subn, scientific_name_dvw sn

#WHERE egt.element_global_id IN ('", id.temp,
#"') and egt.element_global_id = ent.element_global_id
#and ent.element_national_id = est.element_national_id
#and est.subnation_id = subn.subnation_id
#and egt.gname_id = sn.scientific_name_id
#AND ent.nation_id in (38,225)")

#qry <- paste0("select sn.NAME_CATEGORY, egt.rounded_g_rank, COUNT(egt.rounded_g_rank)
#    from element_global egt, scientific_name_dvw sn
#    where (egt.element_global_id in ", id.temp,") 
#      and egt.gname_id = sn.scientific_name_id
#    GROUP BY sn.NAME_CATEGORY, egt.rounded_g_rank
#")

# When finished, it's a good idea to close the connection
odbcClose(con)

library(tidyr)

dat2 <- dat
dat2$taxa<-NA
dat2$taxa[which(dat2$NAME_CATEGORY %in% c("Invertebrate Animal", "Vertebrate Animal"))]<-"Animals"
dat2$taxa[which(dat2$NAME_CATEGORY == "Vascular Plant")]<-"Plants"
dat2 <- subset(dat2, taxa %in% c("Animals", "Plants"), select = -TOTAL) %>% tidyr::gather("Grank", "n", NUM_G1T1:NUM_GUTU) %>% dplyr::group_by(taxa, Grank) %>% dplyr::summarise(n=sum(n)) %>% data.frame()
##rename G ranks to T/F
dat2$Grank[which(dat2$Grank =="NUM_GNRTNR")] <- F
dat2$Grank[which(dat2$Grank!=F)]<-T
dat2 <- dat2 %>% dplyr::group_by(taxa, Grank) %>% dplyr::summarise(n=sum(n)) %>% data.frame()

colnames(dat2) <- c("group", "value", "n")
data.qual <- dat2
data.qual$standard <- "G_Rank"
data.qual$group.type<-"taxa"
