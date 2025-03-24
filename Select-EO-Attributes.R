# Load RODBC package
library(RODBC)
library(tidyverse)

con<-odbcConnect("BIOSNAPDB01", uid="biotics_report", pwd=Sys.getenv("BIOSNAPDB01_PW"))
#con<-odbcConnect("bioticscentral", uid="biotics_report", pwd=Sys.getenv("BIOTICSCENTRAL_PW"))

## Step 1 - select subnational elements
## combo of primary element dataset and where clauses from subnational query
## subnational query

##check for available values
#qry <- "SELECT DISTINCT ROUNDED_G_RANK FROM ELEMENT_GLOBAL"
#dat <- sqlQuery(con, qry) ##import the queried table


## Select subnational elements that are within primary element dataset and G1, G2, ESA, COSEWIC status
qry1 <- "
SELECT 
    element_global.element_global_id, 
    nc.name_category_desc, 
    nsx_informal_tax(element_global.element_global_id) nsx_tax, 
    informal_tax(element_global.element_global_id) info_tax, 
    element_global.rounded_g_rank, 
    egt_usesa(element_global.element_global_id) USESA, 
    d_cosewic.cosewic_desc, 
    element_global.parent_id, 
    element_subnational.element_subnational_id,
    subnation.subnation_code, 
    element_subnational.s_rank
FROM element_subnational
LEFT JOIN element_national ON element_subnational.element_national_id = element_national.element_national_id
LEFT JOIN element_global ON element_national.element_global_id = element_global.element_global_id
LEFT JOIN scientific_name gname ON element_global.gname_id = gname.scientific_name_id
LEFT JOIN d_name_category nc ON gname.d_name_category_id = nc.d_name_category_id
LEFT JOIN subnation ON element_subnational.subnation_id = subnation.subnation_id
LEFT JOIN taxon_subnatl_dist ON element_subnational.element_subnational_id = taxon_subnatl_dist.element_subnational_id
LEFT JOIN taxon_national ON element_national.element_national_id = taxon_national.element_national_id
LEFT JOIN d_cosewic ON taxon_national.d_cosewic_id = d_cosewic.d_cosewic_id
WHERE taxon_subnatl_dist.d_origin_id in (1,3)  /* native or unknown */
AND taxon_subnatl_dist.d_regularity_id = 1  /* regularly occurring */
AND taxon_subnatl_dist.d_dist_confidence_id = 1  /* confident */
AND element_global.inactive_ind = 'N' 
AND element_global.g_rank != 'GNA' 
AND element_national.nation_id IN (38,225)
AND (
    /* Animal criteria - full species, standard classification, standard taxonomic groups with complete distribution, exclude pops and hybrids */
    element_global.element_global_id IN ( 
        SELECT egta.element_global_id 
        FROM element_global egta, scientific_name sna, taxon_global tga 
        WHERE egta.gname_id = sna.scientific_name_id 
        AND egta.element_global_id = tga.element_global_id 
        AND sna.d_classification_level_id = 7  /* full animal species only */
        AND egta.d_classification_status_id = 1  /* standard */
        AND sna.scientific_name NOT LIKE '% pop. %' 
        AND tga.g_hybrid_ind = 'N' 
        AND standard_taxonomic_groups(egta.element_global_id) IS NOT NULL 
        AND standard_taxonomic_groups(egta.element_global_id) NOT IN ('Giant Silkworm and Royal Moths (G1G3)')
        AND egta.rounded_g_rank IN ('G1', 'G2', 'T1', 'T2', 'GH', 'TH')
    )  
    /* Plant criteria - vascular plants, standard classification, exclude pops and hybrids */
    OR element_global.element_global_id IN ( 
        SELECT egtp.element_global_id 
        FROM element_global egtp, scientific_name snp, taxon_global tgp 
        WHERE egtp.gname_id = snp.scientific_name_id  
        AND egtp.element_global_id = tgp.element_global_id 
        AND snp.d_name_category_id = 4  /* Vascular Plant */
        AND egtp.d_classification_status_id = 1  /* standard */
        AND snp.scientific_name NOT LIKE '% pop. %' 
        AND tgp.g_hybrid_ind = 'N'
        AND egtp.rounded_g_rank IN ('G1', 'G2', 'T1', 'T2', 'GH', 'TH')
    ) 
    /* USESA criteria - include all except Delisted */
    OR element_global.element_global_id IN ( 
        SELECT esa_tg.element_global_id 
        FROM taxon_global esa_tg 
        WHERE esa_tg.d_usesa_id IS NOT NULL 
        AND esa_tg.d_usesa_id != 39  /* exclude Delisted */
    )  
    /* COSEWIC status - threatened and endangered only */
    OR element_global.element_global_id IN ( 
        SELECT cosewic_ent.element_global_id 
        FROM element_national cosewic_ent, taxon_national cosewic_tn 
        WHERE cosewic_ent.element_national_id = cosewic_tn.element_national_id 
        AND cosewic_ent.nation_id = 38 
        AND cosewic_tn.d_cosewic_id IS NOT NULL
        AND cosewic_tn.d_cosewic_id IN (3, 4)  /* Threatened and Endangered only */
    )
)
;
"

dat.spp <- sqlQuery(con, qry1) ##import the queried table

## Step 2 - select eo data
qry2 <- ("select case when dnc.name_category_desc like 'Other (%' then dnc.name_type_cd || ' (Other)'
         else dnc.name_type_cd 
       end name_cat, dnc.name_category_desc name_cat_desc
     , egt.elcode_bcd
     , informal_tax(egt.element_global_id) info_tax
     , nsx_informal_tax(egt.element_global_id) info_tax2
     , gname.scientific_name gname, egt.element_global_id, egt.g_primary_common_name
     , egt.rounded_g_rank
     , egt.gt1gt2, egt.gt3, egt.gt4gt5
     , egt_usesa(egt.element_global_id) USESA
     , est.est_app_usesa
     , est.esa_te, est.esa_cp, est.esa_dl, est.esa_oth, est.esa_any
     , case
         when (egt.gt1gt2 = 'Y' and est.esa_te = 'Y') then 'Y'
       end gt1gt2_te
     , case
         when ((egt.gt1gt2 = 'Y' or egt.gt3 = 'Y') and est.esa_te = 'Y') then 'Y'
       end gt1gt3_te
     , case
         when ((egt.gt1gt2 = 'Y' or egt.gt3 = 'Y' or egt.gt4gt5 = 'Y') and est.esa_te = 'Y') then 'Y'
       end gt1gt5_te
     , case
         when (egt.gt1gt2 = 'Y' and est.esa_any = 'Y') then 'Y'
       end gt1gt2_anyesa
     , case
         when ((egt.gt1gt2 = 'Y' or egt.gt3 = 'Y') and est.esa_any = 'Y') then 'Y'
       end gt1gt3_anyesa
     , case
         when ((egt.gt1gt2 = 'Y' or egt.gt3 = 'Y' or egt.gt4gt5 = 'Y') and est.esa_any = 'Y') then 'Y'
       end gt1gt5_anyesa
     , est.usesa_cat
     , est.element_subnational_id, est.s_primary_common_name, sbn.subnation_code, dlookup('NATION', sbn.nation_id) as nation
     , est.rounded_s_rank
     , case
         when taxon_subnational.hybrid_ind = 'Y' then 'Yes'
         else 'No'
       end hybrid
     , dlookup('D_DATA_SENSITIVE', est.d_data_sensitive_id) as est_data_sens
     , count(eo.eo_id) as num_eos
     , count(case
               when eo.principal_eo_shape_id is not null 
               then 1
             end) as num_sub_eos
     , count(case 
               when eo.has_shp is null
               then 1
             end) as num_eos_wo_shp
     , count(eo.xh) as num_xh_EOs
     , count(eo.id_unconfirmed) as num_id_unconfirmed_EOs
     , count(lobs_over_30_yr) as EOs_lobs_gt_30yr
     , count(lobs_over_40_yr) as EOs_lobs_gt_40yr
     , count(case
               when eo.principal_eo_shape_id is not null then null
               when eo.has_shp is null then null
               when eo.id_unconfirmed is not null then null
               when eo.xh is not null then null
               when eo.lobs_over_30_yr = 'Y' then null
               else 1
             end) as num_EOs_std_count_30yr
     , count(case
               when eo.principal_eo_shape_id is not null then null
               when eo.has_shp is null then null
               when eo.id_unconfirmed is not null then null
               when eo.xh is not null then null
               when eo.lobs_over_40_yr = 'Y' then null
               else 1
             end) as num_EOs_std_count_40yr
     , (select count(1) from source_feature sf 
        where sf.element_subnational_id = est.element_subnational_id 
          and est.subnation_id in (32,51) 
          and sf.independent_source_feature_ind = 'Y') num_TXMS_ind_SFs
     , (select count(1) from source_feature sf 
        where sf.element_subnational_id = est.element_subnational_id 
          and est.subnation_id = 29 
          and getnametypecd(est.sname_id) = 'A'
          and sf.independent_source_feature_ind = 'Y') num_MNanimal_ind_SFs          
     , count(eo.data_sens) as num_data_sens_EOs
     , count(case
               when eo.principal_eo_shape_id is not null then null
               when eo.has_shp is null then null
               when eo.id_unconfirmed is not null then null
               when eo.xh is not null then null
               when eo.lobs_over_30_yr = 'Y' then null
               when eo.data_sens is not null then 1
               else null               
             end) as num_data_sens_EOs_curr_30_yr
     , count(case
               when eo.principal_eo_shape_id is not null then null
               when eo.has_shp is null then null
               when eo.id_unconfirmed is not null then null
               when eo.xh is not null then null
               when eo.lobs_over_40_yr = 'Y' then null
               when eo.data_sens is not null then 1
               else null               
             end) as num_data_sens_EOs_curr_40_yr
     , (select count(1) from eo eo2, element_subnational est2, taxon_subnational ts2
        where est2.subnation_id = est.subnation_id 
          and eo2.element_subnational_id = est2.element_subnational_id
          and est2.element_subnational_id = ts2.element_subnational_id
          --and (eo2.d_id_confirmed_id is null or eo2.d_id_confirmed_id <> 1)
          and eo2.principal_eo_shape_id is null
          and ts2.hybrid_ind <> 'Y'
          ) num_eos_all_elem_in_state
     , (select count(1) from source_feature sf2, element_subnational est2 
        where est2.subnation_id = est.subnation_id 
          and sf2.element_subnational_id = est2.element_subnational_id
          and (est.subnation_id in (32,51)
               or
               (est.subnation_id = 29 and getnametypecd(est.sname_id) = 'A')
               )
        ) num_ind_SFs_all_elem_in_state
FROM 
  (select egt2.*
    , case
       when egt2.rounded_g_rank in ('G1','G2','T1','T2') then 'Y'
      end gt1gt2
    , case
       when egt2.rounded_g_rank in ('G3','T3') then 'Y'
      end gt3
    , case
       when egt2.rounded_g_rank in ('G4','T4','G5','T5') then 'Y'
      end gt4gt5
   from element_global egt2) egt, 
  scientific_name gname,
  element_national ent,
  (select sbtbl2.*
        , case
           when sbtbl2.usesa_cat in ('TE','TBD') then 'Y' /*there shouldn't be any TBD, but if there are be conservative and assume TE*/
          end esa_te
        , case
           when sbtbl2.usesa_cat = 'CP' then 'Y'
          end esa_cp
        , case
           when sbtbl2.usesa_cat = 'DL' then 'Y'
          end esa_dl
        , case
           when sbtbl2.usesa_cat = 'OTH' then 'Y'
          end esa_oth
        , case
           when (sbtbl2.usesa_cat is not null and sbtbl2.usesa_cat <> 'DL') then 'Y'
          end esa_any
      from
        (select sbtbl1.*,
             case 
               when trim(est_app_usesa) is null then null
               when upper(est_app_usesa) = 'NO STATUS' then null
               when est_app_usesa = 'DL' then 'DL'
               when est_app_usesa in ('UR','SAE','SAT','SC','PS:SC','PS:SAT','PS:SAE') then 'OTH'
               when upper(est_app_usesa) in ('E','LE','T','LT','ENDANGERED','THREATENED','PS','PS:E','PS:T','XE','XN') then 'TE'
               when upper(est_app_usesa) in ('PE','PT','PE,PT','C','PS:C','PSAE','PSAT') then 'CP'
               when est_app_usesa like 'PS:P%' then 'CP'
               when est_app_usesa like 'E,%' then 'TE'
               when est_app_usesa like 'LE,%' then 'TE'
               when est_app_usesa like 'T,%' then 'TE'
               when est_app_usesa like 'LT,%' then 'TE'
               when (est_app_usesa like 'PS:%' and (substr(est_app_usesa,4) like 'E%' 
                                            or substr(est_app_usesa,4) like 'LE%' 
                                            or substr(est_app_usesa,4) like 'T%' 
                                            or substr(est_app_usesa,4) like 'LT%')) 
                     then 'TE'
               else 'TBD'
             end usesa_cat
             from (select est2.*, ns_est_usesa(est2.element_subnational_id) est_app_usesa 
                   from element_subnational est2) sbtbl1
        ) sbtbl2
  ) est, 
  subnation sbn, taxon_subnational,
  d_name_category dnc,
  (select eo.*
        , case 
            when d_basic_eo_rank_id in (16, 17) then 'Y' 
            else null
          end xh
        , case
            when eo.d_id_confirmed_id = 1 then 'Y'
          end id_unconfirmed
        , case 
            when d_basic_eo_rank_id not in (16, 17) then 'Y' 
            when d_basic_eo_rank_id is null then 'Y' 
            else null
          end not_xh
        , case
            when nvl((select dloy.last_obs_single_year 
                      from d_last_obs_year dloy 
                      where dloy.last_obs_date = eo.last_obs_date),99999) < (EXTRACT(YEAR FROM sysdate)-30) then 'Y'
            when nvl((select dloy.last_obs_max_year from d_last_obs_year dloy where dloy.last_obs_date = eo.last_obs_date),99999) < (EXTRACT(YEAR FROM sysdate)-30) then 'Y'
            when (select dloy.ws_display_value from d_last_obs_year dloy where dloy.last_obs_date = eo.last_obs_date) is null then null
            else null
          end lobs_over_30_yr
        , case
            when nvl((select dloy.last_obs_single_year 
                      from d_last_obs_year dloy 
                      where dloy.last_obs_date = eo.last_obs_date),99999) < (EXTRACT(YEAR FROM sysdate)-40) then 'Y'
            when nvl((select dloy.last_obs_max_year from d_last_obs_year dloy where dloy.last_obs_date = eo.last_obs_date),99999) < (EXTRACT(YEAR FROM sysdate)-40) then 'Y'
            else null
          end lobs_over_40_yr
        , case
            when d_basic_eo_rank_id in (16, 17) then null 
            when nvl((select dloy.last_obs_single_year 
                      from d_last_obs_year dloy 
                      where dloy.last_obs_date = eo.last_obs_date),99999) >= (EXTRACT(YEAR FROM sysdate)-30) then 'Y'
            when nvl((select dloy.last_obs_max_year from d_last_obs_year dloy where dloy.last_obs_date = eo.last_obs_date),99999) >= (EXTRACT(YEAR FROM sysdate)-30) then 'Y'
            else null
          end curr_30_yr
        , case
            when d_basic_eo_rank_id in (16, 17) then null 
            when nvl((select dloy.last_obs_single_year 
                      from d_last_obs_year dloy 
                      where dloy.last_obs_date = eo.last_obs_date),99999) >= (EXTRACT(YEAR FROM sysdate)-40) then 'Y'
            when nvl((select dloy.last_obs_max_year from d_last_obs_year dloy where dloy.last_obs_date = eo.last_obs_date),99999) >= (EXTRACT(YEAR FROM sysdate)-40) then 'Y'
            when (select dloy.ws_display_value from d_last_obs_year dloy where dloy.last_obs_date = eo.last_obs_date) is null then 'Y'
            else null
          end curr_40_yr
        , case
            when d_data_sensitive_id <> 2 then 'Y'
            when d_data_sensitive_id is null then 'Y'
            when exists (select 1 from element_subnational est where eo.element_subnational_id = est.element_subnational_id and est.d_data_sensitive_id <> 2) then 'Y'
            else null
          end data_sens
        , case 
            when (d_basic_eo_rank_id not in (16, 17) or d_basic_eo_rank_id is null) and (d_data_sensitive_id <> 2 or d_data_sensitive_id is null or exists (select 1 from element_subnational est where eo.element_subnational_id = est.element_subnational_id and est.d_data_sensitive_id <> 2)) then 'Y' 
            else null
          end data_sens_not_xh
        , delimlist('select luc.location_use_class_desc 
                     from eo_source_feature esf, source_feature sf, d_location_use_class luc 
                     where esf.source_feature_id = sf.source_feature_id 
                       and sf.d_location_use_class_id = luc.d_location_use_class_id 
                       and luc.d_location_use_class_id in (7) and esf.eo_id = ' || eo.eo_id || ' 
                       group by luc.location_use_class_desc 
                       order by luc.location_use_class_desc',', ') hibernaculum 
        , case
            when exists(select 1 from eo_shape eoshp where eoshp.eo_id = eo_id and eoshp.shape is not null) then 'Y'
            when exists (select 1 from element_subnational est, subnation sbn 
                  where est.subnation_id=sbn.subnation_id 
                    and sbn.nation_id in (225,38) 
                    and sbn.subnation_code in ('AZ','PA','MA')) then 'U'
            else null
          end has_shp
        from eo
        ) eo
WHERE egt.gname_id = gname.scientific_name_id
  and gname.d_name_category_id = dnc.d_name_category_id (+)
  and egt.element_global_id = ent.element_global_id
  and ent.element_national_id = est.element_national_id
  and est.subnation_id = sbn.subnation_id
  and est.element_subnational_id = eo.element_subnational_id (+)
  and est.element_subnational_id = taxon_subnational.element_subnational_id (+)
  and (eo.eo_id is not null 
        or exists (select 1 from source_feature sf 
                   where sf.element_subnational_id = est.element_subnational_id 
                     and sf.independent_source_feature_ind = 'Y' 
                     and est.subnation_id in (29,32,51)))
group by gname.scientific_name, gname.d_name_category_id, dnc.name_type_cd, dnc.name_category_desc
      , egt.elcode_bcd, egt.element_global_id, egt.g_primary_common_name, est.element_subnational_id, est.sname_id
      , egt.rounded_g_rank, egt.gt1gt2, egt.gt3, egt.gt4gt5
      , est.s_primary_common_name, sbn.subnation_code, est.subnation_id, sbn.nation_id, est.d_data_sensitive_id, est.rounded_s_rank
      , taxon_subnational.hybrid_ind, est.est_app_usesa, est.usesa_cat
      , est.esa_te, est.esa_cp, est.esa_dl, est.esa_oth, est.esa_any 
order by gname.scientific_name, egt.element_global_id, sbn.NATION_ID desc, sbn.subnation_code")

dat.eos <- sqlQuery(con, qry2) ##import the queried table

##temporary way to get data without running query
#dat.eos <- readxl::read_xlsx("C:/Users/MaxTarjanPhD/Downloads/Biotics_EO_Summary.xlsx", sheet = "EO_Summary_v2_202401")

#dat.eos %>% filter(ROUNDED_G_RANK %in% c("G1", "T1", "G2", "T2", "GH", "TH") | (!is.na(USESA) & USESA != "DL")) %>% dim()

## combine subnational elements with EO data
dat <- dat.spp %>%
  left_join(dat.eos %>%
              select(ELEMENT_GLOBAL_ID, NAME_CAT, ELEMENT_SUBNATIONAL_ID, SUBNATION_CODE, NATION, NUM_EOS, NUM_SUB_EOS, NUM_EOS_WO_SHP, NUM_XH_EOS, EOS_LOBS_GT_30YR, EOS_LOBS_GT_40YR, NUM_EOS_STD_COUNT_30YR, NUM_EOS_STD_COUNT_40YR, NUM_TXMS_IND_SFS, NUM_MNANIMAL_IND_SFS, NUM_EOS_ALL_ELEM_IN_STATE, NUM_IND_SFS_ALL_ELEM_IN_STATE))

write.csv(dat, paste0("Output/ImperiledSubsetGlobal-EOs-", Sys.Date(), ".csv"), row.names = F)
