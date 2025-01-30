## select national rank to find non-natives
## nonnatives are when Database Table: ELEMENT_NATIONAL$n_rank == "NNA"

filename.temp <- "Output/PrimarySubsetGlobal.csv"
egt.global <- read.csv(filename.temp)

##open connection to database; Jan 2024 snapshot
con<-odbcConnect("BIOSNAPDB01", uid="biotics_report", pwd=rstudioapi::askForPassword("Password"))

#put a loop around query and use rbind to get more than 999 records
id.vector<-egt.global$ELEMENT_GLOBAL_ID
max.length <- 999
x <- 1
y <- min(c(max.length,length(id.vector)))
dat<-dim(0)
for (j in 1:ceiling((length(id.vector)/max.length))) {
  id.temp<-paste0("(", paste0(id.vector[x:y], collapse = ", "), ")")
  
  ##use national rank to determine origin (deprecated)
  # qry <- paste0("SELECT DISTINCT EGT.element_global_id, ELEMENT_NATIONAL.nation_id, ELEMENT_NATIONAL.n_rank
  # from element_global egt
  # LEFT JOIN ELEMENT_NATIONAL
  #   ON egt.element_global_id = ELEMENT_NATIONAL.element_global_id
  # WHERE (egt.element_global_id IN ", id.temp, ") AND ELEMENT_NATIONAL.nation_id in (38,225)")
  # 
  # dat.temp<-sqlQuery(con, qry) ##import the queried table
  # 
  # ##combine to have 1 row per species
  # dat.temp2 <- dat.temp %>%
  #   group_by(ELEMENT_GLOBAL_ID) %>%
  #   mutate(native = ifelse(all(N_RANK == "NNA"), FALSE, TRUE)) %>%
  #   ungroup() %>%
  #   select(ELEMENT_GLOBAL_ID, native) %>%
  #   distinct()
  
  qry <- paste0("SELECT DISTINCT EGT.element_global_id, ELEMENT_NATIONAL.nation_id, taxon_natl_dist.d_origin_id, taxon_natl_dist.d_regularity_id, taxon_natl_dist.d_dist_confidence_id, d_origin.origin_desc
  from element_global egt
  LEFT JOIN ELEMENT_NATIONAL
    ON egt.element_global_id = ELEMENT_NATIONAL.element_global_id
  LEFT JOIN TAXON_NATL_DIST
    ON ELEMENT_NATIONAL.element_national_id = TAXON_NATL_DIST.element_national_id
  LEFT JOIN d_origin
    ON TAXON_NATL_DIST.d_origin_id = d_origin.d_origin_id
  WHERE (egt.element_global_id IN ", id.temp, ") AND ELEMENT_NATIONAL.nation_id in (38,225)")
  
  dat.temp<-sqlQuery(con, qry) ##import the queried table
  
  ##combine to have 1 row per species
  dat.temp2 <- dat.temp %>%
    group_by(ELEMENT_GLOBAL_ID) %>%
    ## allows origin to be native or unknown (1=native, 3=unknown), must be confident (d_dist_confidence_id == 1)
    mutate(native = ifelse(any(D_ORIGIN_ID %in% c(1,3) & D_DIST_CONFIDENCE_ID == 1), T, F)) %>%
    ungroup() %>%
    select(ELEMENT_GLOBAL_ID, native) %>%
    distinct()

  ##join the two tables
  dat.temp3<-dplyr::left_join(subset(egt.global[x:y,], select = c(ELEMENT_GLOBAL_ID)), dat.temp2)
  dat<-rbind(dat, dat.temp3)
  x <- y +1
  y <- min(c(x-1+max.length,length(id.vector)))
}

# When finished, it's a good idea to close the connection
odbcClose(con)

egt.global <- left_join(egt.global, dat)
write.csv(egt.global, filename.temp, row.names=F)
