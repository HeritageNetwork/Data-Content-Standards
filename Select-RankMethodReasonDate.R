##Select Rank method and rank reason
##step 3

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
  qry <- paste0("SELECT EGT.element_global_id, egt.g_primary_common_name , egt.rounded_g_rank, sn.NAME_CATEGORY
  
  from element_global egt, scientific_name_dvw sn
    where (egt.element_global_id IN ", id.temp, ")
      and egt.gname_id = sn.scientific_name_id")
  
  dat.temp<-sqlQuery(con, qry)
  
  ##then get element ids with rank methods
  qry <- paste0("SELECT EGT.element_global_id, egt.rounded_g_rank, sn.NAME_CATEGORY, egr.d_rank_method_used_id, rmu.rank_method_used_desc, rmu.external_desc
  
  from element_global egt, scientific_name_dvw sn, element_global_rank egr, d_rank_method_used rmu
    where (egt.element_global_id IN ", id.temp, ")
      and egt.gname_id = sn.scientific_name_id
  and egt.element_global_id = egr.element_global_id
  and egr.d_rank_method_used_id = rmu.d_rank_method_used_id")
  
  dat.temp2<-sqlQuery(con, qry) ##import the queried table
  
  ##get element ids with rank reasons statement
  qry <- paste0("SELECT EGT.element_global_id, egt.rounded_g_rank, sn.NAME_CATEGORY, egr.g_rank_reasons
  
  from element_global egt, scientific_name_dvw sn, element_global_rank egr
    where (egt.element_global_id IN ", id.temp, ")
      and egt.gname_id = sn.scientific_name_id
  and egt.element_global_id = egr.element_global_id")
  
  dat.temp3<-sqlQuery(con, qry) ##import the queried table
  
  qry <- paste0("SELECT EGT.element_global_id, egt.rounded_g_rank, sn.NAME_CATEGORY, EGT.G_RANK_REVIEW_DATE
  from element_global egt, scientific_name_dvw sn
    where (egt.element_global_id IN ", id.temp, ")
      and egt.gname_id = sn.scientific_name_id")
  
  dat.temp4<-sqlQuery(con, qry) ##import the queried table
  
  ##join the two tables
  dat.temp<-dplyr::left_join(dat.temp, dat.temp2)
  dat.temp<-dplyr::left_join(dat.temp, dat.temp3)
  dat.temp<-dplyr::left_join(dat.temp, dat.temp4)
  dat.temp$Rank_Method<-F
  dat.temp$Rank_Method[which(!is.na(dat.temp$D_RANK_METHOD_USED_ID))]<-T
  dat.temp$Rank_Calculator<-F
  dat.temp$Rank_Calculator[which(dat.temp$EXTERNAL_DESC %in% c("Ranked by calculator", "Calculated rank revised by expert"))]<-T
  dat.temp$Rank_Reason<-F
  dat.temp$Rank_Reason[which(!is.na(dat.temp$G_RANK_REASONS))]<-T
  
  ##summarise in the loop so dataframe doesn't get too big
  #dat.temp<- table(subset(dat.temp, select = c(NAME_CATEGORY, ROUNDED_G_RANK, Rank_Method, Rank_Reason, Rank_Calculator, G_RANK_REVIEW_DATE))) %>% data.frame() ##given huge number of combinations this now gets huge
  dat.temp<- subset(dat.temp, select = c(ELEMENT_GLOBAL_ID, G_PRIMARY_COMMON_NAME, NAME_CATEGORY, ROUNDED_G_RANK, Rank_Method, Rank_Reason, Rank_Calculator, G_RANK_REVIEW_DATE, D_RANK_METHOD_USED_ID))
  dat.temp$x<-x
  dat.temp$y<-y
  dat<-rbind(dat, dat.temp)
  x <- y +1
  y <- min(c(x-1+max.length,length(id.vector)))
}

# When finished, close the connection
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
##translate rank review dates into categories
dat2$G_RANK_REVIEW_DATE2<-dat2$G_RANK_REVIEW_DATE %>% as.character() %>% as.Date(format= "%Y-%m-%d")
dat2 <- dat2 %>% dplyr::mutate(G_Rank_Review_Date = dplyr::case_when(
  (Sys.Date() - dat2$G_RANK_REVIEW_DATE2) <= 365*10 ~ "0-10 years",
  #(Sys.Date() - dat2$G_RANK_REVIEW_DATE2) >= 365*5 & (Sys.Date() - dat2$G_RANK_REVIEW_DATE2) < 365*10 ~ "5-10 years",
  (Sys.Date() - dat2$G_RANK_REVIEW_DATE2) > 365*10 | is.na(dat2$G_RANK_REVIEW_DATE2) ~ ">10 years"
))

dat3<-subset(dat2, taxa %in% c("Animals", "Plants")) %>% dplyr::group_by(taxa, Rank_Method) %>% dplyr::summarise(n=dplyr::n(), standard="Rank_Method", group.type="taxa") %>% data.frame()
dat4<-subset(dat2, taxa %in% c("Animals", "Plants")) %>% dplyr::group_by(taxa, Rank_Reason) %>% dplyr::summarise(n=dplyr::n(), standard="Rank_Reason", group.type="taxa") %>% data.frame()
dat5<-subset(dat2, taxa %in% c("Animals", "Plants")) %>% dplyr::group_by(taxa, Rank_Calculator) %>% dplyr::summarise(n=dplyr::n(), standard="Rank_Calculator", group.type="taxa") %>% data.frame()
dat6<-subset(dat2, taxa %in% c("Animals", "Plants") & !(G_RANK %in% c("GX/TX"))) %>% dplyr::group_by(taxa, G_Rank_Review_Date) %>% dplyr::summarise(n=dplyr::n(), standard="G_Rank_Review_Date", group.type="taxa") %>% data.frame()

colnames(dat3) <- c("group", "value", "n", "standard", "group.type"); colnames(dat4) <- c("group", "value", "n", "standard", "group.type"); colnames(dat5) <- c("group", "value", "n", "standard", "group.type"); colnames(dat6) <- c("group", "value", "n", "standard", "group.type")

##break counts up by Grank
standards.temp<-c("Rank_Method", "Rank_Reason", "Rank_Calculator", "G_Rank_Review_Date")
dat7<-dim(0)
for(j in 1:length(standards.temp)) {
  dat.temp<-subset(dat2, taxa %in% c("Animals", "Plants")) %>% dplyr::group_by(get(standards.temp[j]), G_RANK) %>% dplyr::summarise(n=dplyr::n()) %>% data.frame()
  names(dat.temp)[names(dat.temp) == 'get.standards.temp.j..'] <- 'value'
  dat.temp$standard<-standards.temp[j]
  dat7<-rbind(dat7, dat.temp)
}
dat7$group.type<-"G_Rank"
dat7<-subset(dat7, !(standard=="G_Rank_Review_Date" & G_RANK=="GX/TX"))
names(dat7)[names(dat7) == 'G_RANK'] <- 'group'

##if need to remove existing data from master dataframe first
#data.qual<-subset(data.qual, subset = !(standard %in% standards.temp), select = -prop)

data.qual<-rbind(data.qual, dat3, dat4, dat5, dat6)
data.qual<-rbind(data.qual, subset(dat7, select= names(data.qual)))

##save dat2 for histogram of review years
dat.rank<-dat2
