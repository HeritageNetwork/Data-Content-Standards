##step 3

##Indication of the reason for the most recent rank change. 
##Percent of elements with rank change reasons indicated 

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
  
  ##then get element ids with rank change reasons; ASSUMES THAT DATES MATCH PERFECTLY FOR G RANK CHANGE AND ENTRY DATES
  qry <- paste0("SELECT DISTINCT EGT.element_global_id, egt.rounded_g_rank, egt.g_rank_change_date, rc.rank_change_entry_date, rc.d_rank_change_reason_id
  from element_global egt
  LEFT JOIN element_grank_change rc
    ON egt.element_global_id = rc.element_global_id
  WHERE (egt.element_global_id IN ", id.temp, ")
    and rc.rank_change_entry_date = EGT.g_rank_change_date
    AND rc.rank_change_entry_date IS NOT NULL")
  
  dat.temp<-sqlQuery(con, qry) ##import the queried table
  
  ##combine to have 1 row per species
  dat.temp2 <- dat.temp %>% group_by(ELEMENT_GLOBAL_ID) %>% summarise(Rank_Change_Reason = !all(is.na(D_RANK_CHANGE_REASON_ID))) %>% data.frame()
  ##join the two tables
  dat.temp<-dplyr::left_join(subset(egt.global[x:y,], select = ELEMENT_GLOBAL_ID), dat.temp2)
  dat<-rbind(dat, dat.temp)
  x <- y +1
  y <- min(c(x-1+max.length,length(id.vector)))
}

# When finished, it's a good idea to close the connection
odbcClose(con)

egt.global <- left_join(egt.global, dat)
write.csv(egt.global, "Output/PrimarySubsetGlobal.csv", row.names=F)
