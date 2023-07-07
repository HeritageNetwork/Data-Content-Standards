##Step 2
##Select-ThreatCatgory
##Threat categories indicated, even if assessed and found to be “unknown” (GH/G1/G2/G3 and TH/T1/T2/T3 taxa only).

##open connection to database; Jan 2022 snapshot
#con<-odbcConnect("BIOSNAPDB07", uid="biotics_report", pwd=rstudioapi::askForPassword("Password"))
##open connection to database; production biotics
con<-odbcConnect("centralbiotics", uid="biotics_report", pwd=rstudioapi::askForPassword("Password"))

##put a loop around query and use rbind to get more than 999 records
id.vector<-egt.global$ELEMENT_GLOBAL_ID
max.length <- 999
x <- 1
y <- min(c(max.length,length(id.vector)))
dat<-dim(0)
for (j in 1:ceiling((length(id.vector)/max.length))) {
  id.temp<-paste0("(", paste0(id.vector[x:y], collapse = ", "), ")")
  ##get element ids with rank methods
  qry <- paste0("SELECT EGT.element_global_id, eta.d_iucn_threat_category_id
  from element_global egt
  LEFT JOIN el_global_threats_assess eta
    ON egt.element_global_id = eta.element_global_id
  where (egt.element_global_id IN ", id.temp, ")")
  
  dat.temp<-sqlQuery(con, qry) ##import the queried table
  ##combine to have 1 row per species
  dat.temp2 <- dat.temp %>% group_by(ELEMENT_GLOBAL_ID) %>% summarise(Threat_Category = !all(is.na(D_IUCN_THREAT_CATEGORY_ID))) %>% data.frame()
  
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
