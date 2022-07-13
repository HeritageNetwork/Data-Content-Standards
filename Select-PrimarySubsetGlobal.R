##create the primary element dataset for national records
##connect to database
##execute SQL query
##query from Q:\Conservation Services\ConsServ_Programs\IMPS_Department\Benchmark Data Standards\BDCS_Annual_Report\2018 report\SQL_WorkingLists/sql_EGT_subset2_201806

# Load RODBC package
library(RODBC)

##NEED TO FIRST CONNECT TO VPN
con<-odbcConnect("BIOSNAPDB07", uid="biotics_report", pwd=rstudioapi::askForPassword("Password")) ##open connection to database
#sqlTables(con) ##show tables in the database

qry <- "SELECT DISTINCT egt.element_global_id
FROM  scientific_name gname,
  element_global egt, element_global_tax_vw egtv
WHERE 
  egt.inactive_ind = 'N'
  and egt.gname_id = gname.scientific_name_id
  and egt.element_global_id = egtv.element_global_id
  and gname.d_name_category_id in (1,2) /* vert and invert animals */
  AND gname.d_classification_level_id = 7  /* full species only */
  and (standard_taxonomic_groups(egt.element_global_id) is not null OR egtv.genus in ('Megachile', 'Osmia'))
/* standard tax groups */
  and standard_taxonomic_groups(egt.element_global_id) not in ('Notodontid Moths','Giant Silkworm and Royal Moths','Tiger Moths')  
and 
  (egt.element_global_id in
    (SELECT ca_ent.element_global_id
    FROM element_national ca_ent,
        taxon_natl_dist ca_tn_dist,
        element_global ca_egt
    WHERE ca_ent.nation_id = 38  /* Canada */
        and ca_ent.element_national_id = ca_tn_dist.element_national_id (+)
        and ca_ent.element_global_id = ca_egt.element_global_id
        and (((ca_tn_dist.d_dist_confidence_id = 1 /* confident */ and ca_tn_dist.d_regularity_id = 1  /* Regularly occurring */ ) 
                 or (ca_ent.n_rank = 'NU'))
            and ca_egt.d_classification_status_id = 1  /* standard */))

  or

  egt.element_global_id in
    (SELECT us_ent.element_global_id
      FROM element_national us_ent,
        taxon_natl_dist us_tn_dist,
        element_global us_egt
      WHERE us_ent.nation_id = 225  /*  US */
        and us_ent.element_national_id = us_tn_dist.element_national_id (+)
        and us_ent.element_global_id = us_egt.element_global_id (+)
        and (((us_tn_dist.d_dist_confidence_id = 1 /* confident */  and us_tn_dist.d_regularity_id = 1 /* Regularly occurring */  )
                      or (us_ent.n_rank = 'NU'))
            and us_egt.d_classification_status_id = 1 /* standard */ ))
)

UNION

/*  Any taxa that have federal status (US: USESA; Canada: SARA or COSEWIC) */

SELECT DISTINCT egt.element_global_id
FROM  element_global egt
WHERE 
  egt.inactive_ind = 'N'
  and 
  (egt.element_global_id in
    (SELECT ca_ent.element_global_id
      FROM element_national ca_ent,
        taxon_natl_dist ca_tn_dist,
        taxon_national ca_tn,
        element_global ca_egt,
        el_natl_agency_status nas,
        other_ent_id nid
     WHERE ca_ent.nation_id = 38  /* Canada */
        and ca_ent.element_national_id = ca_tn.element_national_id (+)
        and ca_ent.element_national_id = ca_tn_dist.element_national_id (+)
        and ca_ent.element_global_id = ca_egt.element_global_id
        and ca_ent.element_national_id = nas.element_national_id (+) 
        and ca_ent.element_national_id = nid.element_national_id (+)
        and (((ca_tn_dist.d_dist_confidence_id = 1 /* confident */ and ca_tn_dist.d_regularity_id = 1  /* Regularly occurring */ ) 
                 or (ca_ent.n_rank = 'NU'))
            and (nas.agency_name like 'SARA%'
                  or ((nid.other_id_organization = 'COSEWIC' 
                         or ca_tn.interpreted_cosewic is not null 
                         or ca_tn.d_cosewic_id is not null) and                           ca_tn.d_cosewic_id in (1, 2, 3, 4, 5))
                  )
                  ))

  or

  egt.element_global_id in
    (SELECT us_ent.element_global_id
      FROM element_national us_ent,
        taxon_natl_dist us_tn_dist,
        element_global us_egt,
        taxon_global us_tg
      WHERE us_ent.nation_id = 225  /* US */
        and us_ent.element_national_id = us_tn_dist.element_national_id (+)
        and us_ent.element_global_id = us_egt.element_global_id (+)
        and us_egt.element_global_id = us_tg.element_global_id
        and ( ((us_tn_dist.d_dist_confidence_id = 1 /* confident */  and us_tn_dist.d_regularity_id = 1 /* Regularly occurring */)
                 or (us_ent.n_rank = 'NU'))
            and (us_tg.d_usesa_id is not null
                  OR us_tg.interpreted_usesa is not null)))
)

UNION

/* modified from the NSX sql: All botanical elements which regularly and confidently occur in the US or CA which are: standard, or are provisional with a published gname (usu. these are newly described taxa) 
Excludes inactive elements and plant populations. */

select distinct egt.element_global_id
from
  element_global egt,
  scientific_name gname,
  d_name_category name_cat
where 
  egt.inactive_ind = 'N'
  and egt.gname_id = gname.scientific_name_id
  and gname.d_name_category_id = name_cat.d_name_category_id
  and name_cat.name_category_desc like 'Vascular Plant'
  and gname.scientific_name not like '% pop. %'
  and 
(egt.element_global_id in
    (SELECT ca_ent.element_global_id
      FROM element_national ca_ent,
        taxon_natl_dist ca_tn_dist,
        element_global ca_egt,
        scientific_name ca_gname
     WHERE ca_ent.nation_id = 38  /* Canada */
        and ca_ent.element_national_id = ca_tn_dist.element_national_id (+)
        and ca_ent.element_global_id = ca_egt.element_global_id
        and ca_egt.gname_id = ca_gname.scientific_name_id
        and (((ca_tn_dist.d_dist_confidence_id = 1 /* confident */ and ca_tn_dist.d_regularity_id = 1 /* regularly occurring */) 
                     or (ca_ent.n_rank = 'NU'))
            and (ca_egt.d_classification_status_id = 1 /* standard */
                  or (ca_egt.d_classification_status_id = 3 /* provisional */ and ca_gname.placeholder_name_ind = 'N'))))

  or

  egt.element_global_id in
    (SELECT us_ent.element_global_id
      FROM element_national us_ent,
        taxon_natl_dist us_tn_dist,
        element_global us_egt,
        scientific_name us_gname
      WHERE us_ent.nation_id = 225 /* US */
        and us_ent.element_national_id = us_tn_dist.element_national_id (+)
        and us_ent.element_global_id = us_egt.element_global_id (+)
        and us_egt.gname_id = us_gname.scientific_name_id
        and ( ((us_tn_dist.d_dist_confidence_id = 1 /* confident */ and us_tn_dist.d_regularity_id = 1 /* regularly occurring */)
                      or (us_ent.n_rank = 'NU'))
            and ( us_egt.d_classification_status_id = 1 /* standard */
                  or (us_egt.d_classification_status_id = 3 /* provisional */ and us_gname.placeholder_name_ind = 'N'))))
)
"

egt.global<-sqlQuery(con, qry); head(egt.global) ##import the queried table

# When finished, it's a good idea to close the connection
odbcClose(con)

#write.csv(egt.global, "Output/PrimarySubsetGlobal.csv", row.names=F)
