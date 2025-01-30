##step 3

##Indication of the reason for the most recent rank change. 
##Percent of elements with rank change reasons indicated 

##start with identical date; might do +/- a timeframe to find matches in the future

##open connection to database; Jan 2022 snapshot
#con<-odbcConnect("BIOSNAPDB07", uid="biotics_report", pwd=rstudioapi::askForPassword("Password"))
##open connection to database; Jan 2024 snapshot
con<-odbcConnect("BIOSNAPDB01", uid="biotics_report", pwd=rstudioapi::askForPassword("Password"))

##put a loop around query and use rbind to get more than 999 records
id.vector<-egt.global$ELEMENT_GLOBAL_ID
max.length <- 999
x <- 1
y <- min(c(max.length,length(id.vector)))
dat<-dim(0)
for (j in 1:ceiling((length(id.vector)/max.length))) {
  id.temp<-paste0("(", paste0(id.vector[x:y], collapse = ", "), ")")
  
  ##then get element ids with rank change reasons
  qry <- paste0("SELECT DISTINCT EGT.element_global_id, egt.rounded_g_rank, egt.g_rank_change_date, rc.rank_change_entry_date, rc.d_rank_change_reason_id
  from element_global egt
  LEFT JOIN element_grank_change rc
    ON egt.element_global_id = rc.element_global_id
  WHERE (egt.element_global_id IN ", id.temp, ")
    AND rc.rank_change_entry_date IS NOT NULL")
  
  dat.temp<-sqlQuery(con, qry) ##import the queried table
  
  ##combine to have 1 row per species
  ##ASSUMES THAT DATES MATCH PERFECTLY FOR G RANK CHANGE AND ENTRY DATES
  dat.temp2 <- subset(dat.temp, RANK_CHANGE_ENTRY_DATE == G_RANK_CHANGE_DATE) %>% group_by(ELEMENT_GLOBAL_ID) %>% summarise(Rank_Change_Reason = !all(is.na(D_RANK_CHANGE_REASON_ID))) %>% data.frame()
  ##join the two tables
  #dat.temp3<-dplyr::left_join(subset(egt.global[x:y,], select = c(ELEMENT_GLOBAL_ID, G_RANK_CHANGE_DATE)), dat.temp2) ##giving an error because g_rank_change_date is not in egt.global. removing this for testing
  dat.temp3<-dplyr::left_join(subset(egt.global[x:y,], select = c(ELEMENT_GLOBAL_ID)), dat.temp2)
  ##need to set those with rank change date to F if not T and others to NA (if there was never a rank change)
  dat.temp3$Rank_Change_Reason[which(is.na(dat.temp3$Rank_Change_Reason) & dat.temp3$ELEMENT_GLOBAL_ID %in% dat.temp$ELEMENT_GLOBAL_ID)] <- F
  dat<-rbind(dat, dat.temp3)
  x <- y +1
  y <- min(c(x-1+max.length,length(id.vector)))
}

# When finished, it's a good idea to close the connection
odbcClose(con)

egt.global <- left_join(egt.global, dat)
write.csv(egt.global, "Output/PrimarySubsetGlobal.csv", row.names=F)
