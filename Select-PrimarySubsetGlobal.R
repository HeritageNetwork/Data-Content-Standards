##create the primary element dataset for national records
##connect to database
##execute SQL query
##query from Q:\Conservation Services\ConsServ_Programs\IMPS_Department\Benchmark Data Standards\BDCS_Annual_Report\2018 report\SQL_WorkingLists/sql_EGT_subset2_201806
##step 1

# Load RODBC package
library(RODBC)
library(tidyverse)

##NEED TO FIRST CONNECT TO VPN
##open connection to database; Jan 2022 snapshot
#con<-odbcConnect("BIOSNAPDB07", uid="biotics_report", pwd=rstudioapi::askForPassword("Password"))
##open connection to database; production biotics
#con<-odbcConnect("bioticscentral", uid="biotics_report", pwd=rstudioapi::askForPassword("Password"))
##open connection to database; Jan 2023 snapshot
#con<-odbcConnect("BIOSNAPDB08", uid="biotics_report", pwd=rstudioapi::askForPassword("Password"))
##open connection to database; Jan 2024 snapshot
con<-odbcConnect("BIOSNAPDB01", uid="biotics_report", pwd=rstudioapi::askForPassword("Password"))
#sqlTables(con) ##show tables in the database


qry <- "SELECT DISTINCT egt.element_global_id, gname.scientific_name, egt.g_primary_common_name, nc.name_category_desc, egt.rounded_g_rank, egr.d_rank_method_used_id, rmu.rank_method_used_desc, rmu.external_desc, egr.g_rank_reasons, EGT.G_RANK_REVIEW_DATE
FROM  element_global egt
LEFT JOIN scientific_name gname
  ON egt.gname_id = gname.scientific_name_id
LEFT JOIN d_name_category nc
  ON gname.d_name_category_id = nc.d_name_category_id
LEFT JOIN element_global_rank egr
    ON egt.element_global_id = egr.element_global_id
LEFT JOIN d_rank_method_used rmu
    ON egr.d_rank_method_used_id = rmu.d_rank_method_used_id
WHERE
/* criteria that applies to all records - active, regular and confident in US or Canada */ 
  egt.inactive_ind = 'N' 
  and egt.element_global_id in ( 
    (SELECT ent.element_global_id 
      FROM element_national ent 
      where ent.nation_id in (38,225) 
       and ent.element_national_id in  
       (select tnd.element_national_id from taxon_natl_dist tnd 
        where tnd.d_regularity_id = 1 /* Regularly occurring */ and tnd.d_dist_confidence_id = 1 /* confident */))) 
  and  
  ( 
 -- animal criteria - full species, standard classification, standard taxonomic groups with complete distribution, exclude pops and hybrids 
  egt.element_global_id in  
   (select egta.element_global_id 
     from element_global egta, scientific_name sna,taxon_global tga 
     where egta.gname_id = sna.scientific_name_id 
     and egta.element_global_id = tga.element_global_id 
     and sna.d_classification_level_id = 7  /* full animal species only */ 
     and egta.d_classification_status_id = 1 /*standard */ 
     and sna.scientific_name not like '% pop. %' 
     and tga.g_hybrid_ind = 'N' 
     and standard_taxonomic_groups(egta.element_global_id) is not null 
     and standard_taxonomic_groups(egta.element_global_id) not in ('Notodontid Moths (G1G3)','Giant Silkworm and Royal Moths (G1G3)','Tiger Moths (G1G3)') )  
 -- plant criteria - vascular plants, standard classification, exclude pops and hybrids 
  or  
  egt.element_global_id in  
   (select egtp.element_global_id 
     from element_global egtp, scientific_name snp, taxon_global tgp 
     where egtp.gname_id = snp.scientific_name_id  
     and egtp.element_global_id = tgp.element_global_id 
     and snp.d_name_category_id = 4 /*Vascular Plant */ 
     and egtp.d_classification_status_id = 1 /*standard */ 
     and snp.scientific_name not like '% pop. %' 
     and tgp.g_hybrid_ind = 'N') 
 -- USESA criteria - include all except Delisted 
  or  
  egt.element_global_id in 
    (select esa_tg.element_global_id 
     from taxon_global esa_tg 
     where esa_tg.d_usesa_id is not null 
     and esa_tg.d_usesa_id != 39)  /** exclude Delisted only  **/ 
  or 
 -- COSEWIC status (actual, not interpreted, specific values) 
  egt.element_global_id in 
     (select cosewic_ent.element_global_id 
     from element_national cosewic_ent, 
     taxon_national cosewic_tn 
     where cosewic_ent.element_national_id = cosewic_tn.element_national_id 
     and cosewic_ent.nation_id = 38 
     and cosewic_tn.d_cosewic_id in (1, 2, 3, 4, 5)) 
  or 
 -- SARA status 
  egt.element_global_id in 
     (select sara_ent.element_global_id 
     from element_national sara_ent, 
      el_natl_agency_status sara_nas 
     where sara_ent.element_national_id = sara_nas.element_national_id 
     and sara_ent.nation_id = 38 
     and sara_nas.agency_name like 'SARA%') 
  ) 
"

egt.global<-sqlQuery(con, qry); head(egt.global) ##import the queried table

# When finished, it's a good idea to close the connection
odbcClose(con)

write.csv(egt.global, "Output/PrimarySubsetGlobal.csv", row.names=F)
