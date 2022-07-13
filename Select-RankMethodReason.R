##Select Rank method and rank reason

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
  qry <- paste0("SELECT EGT.element_global_id, egt.rounded_g_rank, sn.NAME_CATEGORY
  
  from element_global egt, scientific_name_dvw sn
    where (egt.element_global_id IN ", id.temp, ")
      and egt.gname_id = sn.scientific_name_id")
  
  dat.temp<-sqlQuery(con, qry)
  
  ##then get element ids with rank methods
  qry <- paste0("SELECT EGT.element_global_id, egt.rounded_g_rank, sn.NAME_CATEGORY, egr.d_rank_method_used_id, rmu.rank_method_used_desc
  
  from element_global egt, scientific_name_dvw sn, element_global_rank egr, d_rank_method_used rmu
    where (egt.element_global_id IN ", id.temp, ")
      and egt.gname_id = sn.scientific_name_id
  and egt.gname_id = egr.element_global_id
  and egr.d_rank_method_used_id = rmu.d_rank_method_used_id")
  
  dat.temp2<-sqlQuery(con, qry) ##import the queried table
  
  ##get element ids with rank reasons statement
  qry <- paste0("SELECT EGT.element_global_id, egt.rounded_g_rank, sn.NAME_CATEGORY, egr.g_rank_reasons
  
  from element_global egt, scientific_name_dvw sn, element_global_rank egr
    where (egt.element_global_id IN ", id.temp, ")
      and egt.gname_id = sn.scientific_name_id
  and egt.gname_id = egr.element_global_id")
  
  dat.temp3<-sqlQuery(con, qry) ##import the queried table
  
  ##join the two tables
  dat.temp<-dplyr::left_join(dat.temp, dat.temp2)
  dat.temp<-dplyr::left_join(dat.temp, dat.temp3)
  dat.temp$Rank_Method<-F
  dat.temp$Rank_Method[which(!is.na(dat.temp$D_RANK_METHOD_USED_ID))]<-T
  dat.temp$Rank_Reason<-F
  dat.temp$Rank_Reason[which(!is.na(dat.temp$G_RANK_REASONS))]<-T
  
  ##summarise in the loop so dataframe doesn't get too big
  dat.temp<- table(subset(dat.temp, select = c(NAME_CATEGORY, ROUNDED_G_RANK, Rank_Method, Rank_Reason))) %>% data.frame()
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
dat3<-subset(dat2, taxa %in% c("Animals", "Plants")) %>% dplyr::group_by(taxa, Rank_Method) %>% dplyr::summarise(n=sum(Freq), standard="Rank_Method") %>% data.frame()
dat4<-subset(dat2, taxa %in% c("Animals", "Plants")) %>% dplyr::group_by(taxa, Rank_Reason) %>% dplyr::summarise(n=sum(Freq), standard="Rank_Reason") %>% data.frame()

colnames(dat3) <- c("taxa", "value", "n", "standard"); colnames(dat4) <- c("taxa", "value", "n", "standard")
data.qual<-rbind(data.qual, dat3, dat4)
