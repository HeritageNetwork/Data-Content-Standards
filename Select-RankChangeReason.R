##step 5
################~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!!!!!!!!!!!!!!
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
###!!!!!!!!!!!!!!!!!!!!
####NOT RUNNING YET. still figuring out the sql query
##Select-RankChangeReason

##Indication of the reason for the most recent rank change. 
##Percent of elements with rank change reasons indicated 

##need rank change date (to find the most recent change)?? and rank change reason
##TAXON_GBL_RANK_CHANGES.g_rank_change_com; ELEMENT_GLOBAL.g_rank_change_date

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
  qry <- paste0("SELECT EGT.element_global_id, sn.NAME_CATEGORY, egt.rounded_g_rank
  from element_global egt, scientific_name_dvw sn
    where (egt.element_global_id IN ", id.temp, ")
      and egt.gname_id = sn.scientific_name_id")
  
  dat.temp<-sqlQuery(con, qry)
  
  ##then get element ids with rank change reasons
  ##still working on PULLING OUT RANK CHANGE REASONS
  qry <- paste0("SELECT EGT.element_global_id, sn.NAME_CATEGORY, egt.rounded_g_rank, rc.g_rank_change_com, EGT.g_rank_change_date, egt.g_rank_rev_date, rc.??reviewdate
  from element_global egt, scientific_name_dvw sn, TAXON_GBL_RANK_CHANGES rc
    where (egt.element_global_id IN ", id.temp, ")
      and egt.gname_id = sn.scientific_name_id
      and rc.??date = EGT.g_rank_change_date
      and rc.??id = egt.element_global_id")
  
  dat.temp2<-sqlQuery(con, qry) ##import the queried table
  ##join the two tables
  dat.temp3<-dplyr::left_join(dat.temp, dat.temp2)
  dat.temp3$Rank_Change_Reason<-F
  dat.temp3$Rank_Change_Reason[which(!is.na(dat.temp3$G_RANK_CHANGE_COM))]<-T
  
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