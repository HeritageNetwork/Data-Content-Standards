##step 5
##Habitat categories indicated.

##Percent of elements with at least habitat category marked

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
  
  ##get element ids with habitat categories indicated
  ##find out if has g habitat matrix
  qry <- paste0("SELECT
 egt.element_global_id, egt.ROUNDED_G_RANK, GETNAMECATDESC(egt.gname_id) Name_Category,
(case when egr.g_rank_reasons is not null then 'Y' else 'N' end) has_g_rank_reasons,
--(case when egr.g_threat_com is not null then 'Y' else 'N' end) has_g_threat_com,
--(case when egr.d_threat_impact_assigned_id is not null then 'Y' else 'N' end) has_g_overall_threat_impact,
(case when exists (select 'X' from egr_threats_vw
where egt.element_global_id = egr_threats_vw.element_global_id and egr_threats_vw.iucn_threat is not null) then 'Y' else 'N' end) has_g_threat_grid,
--(case when plant_cag.g_habitat_com is not null or animal_cag.g_habitat_com is not null then 'Y' else 'N' end) has_g_habitat_com,
(case when
(egt.element_global_id in
        (select distinct egt.ELEMENT_GLOBAL_ID
         from ELEMENT_GLOBAL egt,
    animal_cag_estuarine_hab acag_est,
    animal_cag_lacus_hab acag_lacus,
    animal_cag_marine_hab acag_marine,
    animal_cag_palus_hab acag_pal,
    animal_cag_riverine_hab acag_riv,
    animal_cag_subterr_hab acag_st,
    animal_cag_terr_hab acag_terr
         where
     egt.element_global_id = acag_est.element_global_id (+)
    and egt.element_global_id = acag_lacus.element_global_id (+)
    and egt.element_global_id = acag_marine.element_global_id (+)
    and egt.element_global_id = acag_pal.element_global_id (+)
    and egt.element_global_id = acag_riv.element_global_id (+)
    and egt.element_global_id = acag_st.element_global_id (+)
    and egt.element_global_id = acag_terr.element_global_id (+)   
         and (acag_est.D_ESTUARINE_HABITAT_ID is not null
         or acag_lacus.D_LACUSTRINE_HABITAT_ID is not null
         or acag_marine.D_MARINE_HABITAT_ID is not null
         or acag_pal.D_PALUSTRINE_HABITAT_ID is not null
         or acag_riv.D_RIVERINE_HABITAT_ID is not null
         or acag_st.D_SUBTERRANEAN_HABITAT_ID is not null       
         or acag_terr.D_TERRESTRIAL_HABITAT_ID is not null)  )   
or egt.ELEMENT_GLOBAL_ID in
        (select distinct egt.ELEMENT_GLOBAL_ID
         from ELEMENT_GLOBAL egt,
    plant_cag_estuarine_hab pcag_est,
    plant_cag_lacus_hab pcag_lacus,
    plant_cag_marine_hab pcag_marine,
    plant_cag_palus_hab pcag_pal,
    plant_cag_riverine_hab pcag_riv,
        plant_cag_terr_hab pcag_terr
         where
     egt.element_global_id = pcag_est.element_global_id (+)
    and egt.element_global_id = pcag_lacus.element_global_id (+)
    and egt.element_global_id = pcag_marine.element_global_id (+)
    and egt.element_global_id = pcag_pal.element_global_id (+)
    and egt.element_global_id = pcag_riv.element_global_id (+)
        and egt.element_global_id = pcag_terr.element_global_id (+)       
         and (pcag_est.D_ESTUARINE_HABITAT_ID is not null
         or pcag_lacus.D_LACUSTRINE_HABITAT_ID is not null
         or pcag_marine.D_MARINE_HABITAT_ID is not null
         or pcag_pal.D_PALUSTRINE_HABITAT_ID is not null
         or pcag_riv.D_RIVERINE_HABITAT_ID is not null
         or pcag_terr.D_TERRESTRIAL_HABITAT_ID is not null)  ) ) then 'Y' else 'N' end) has_g_habitat_matrix
FROM
  ELEMENT_GLOBAL egt
  , scientific_name sn
  , element_global_rank egr
  , PLANT_CAG
  , ANIMAL_CAG
WHERE
egt.GNAME_ID =  sn.SCIENTIFIC_NAME_ID
and egt.element_global_id = egr.element_global_id (+)
and egt.element_global_id = PLANT_CAG.element_global_id (+)
and egt.element_global_id = ANIMAL_CAG.element_global_id (+)
and (egt.element_global_id IN ", id.temp, ")")
  
  dat.temp<-sqlQuery(con, qry); head(dat.temp) ##import the queried table
  dat.temp$Habitat_Categories<-F
  dat.temp$Habitat_Categories[which(dat.temp$HAS_G_HABITAT_MATRIX=="Y")]<-T
  
  ##summarise in the loop so dataframe doesn't get too big
  dat.temp<- table(subset(dat.temp, select = c(NAME_CATEGORY, ROUNDED_G_RANK, Habitat_Categories))) %>% data.frame()
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

dat3<-subset(dat2, taxa %in% c("Animals", "Plants")) %>% dplyr::group_by(taxa, Habitat_Categories) %>% dplyr::summarise(n=sum(Freq), group.type="taxa") %>% data.frame()
colnames(dat3) <- c("group", "value", "n", "group.type")
dat3$standard<-"Habitat_Categories"

dat4<-subset(dat2, taxa %in% c("Animals", "Plants")) %>% dplyr::group_by(G_RANK, Habitat_Categories) %>% dplyr::summarise(n=sum(Freq), group.type="G_Rank") %>% data.frame()
colnames(dat4) <- c("group", "value", "n", "group.type")
dat4$standard<-"Habitat_Categories"

##if want to replace what's already in the main data table
#data.qual<-subset(data.qual, !standard=="Habitat_Categories", -prop) ##first remove existing values

data.qual<-rbind(data.qual, subset(dat3, select= names(data.qual)))
data.qual<-rbind(data.qual, subset(dat4, select= names(data.qual)))
