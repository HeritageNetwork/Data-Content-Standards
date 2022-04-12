##data content standards
##M Tarjan
##April 11, 2022

##Jan 2022 snapshot
##biotics fields: https://natureserve01-my.sharepoint.com/:x:/g/personal/shara_howie_natureserve_org/EYznjVG_BAJGi40urSIos_kBbKaNQ0gYaaQ0Uyy8AutZ1A?e=J0HS2O&CID=01d1f82e-5dbc-3e50-37b6-635d6815158a
##example of reticulate package: http://zevross.com/blog/2019/11/12/using-reticulate-with-arcpy-on-a-windows-machine/

##use netextender to connect to VPN
##start by installing the package
#install.packages("arcgisbinding", repos="https://r.esri.com", type="win.binary")

# Reading in MRT tables from AGOL into R
## Load libraries
library(sf)
library(tidyverse)
library(arcgisbinding)
## Check ArcGIS Pro product is installed locally
arc.check_product()

##biotics snapshot
snap<-arc.open("S:/Data/NatureServe/BLD_Occurrences/NS_BLD_GeoDB/Snapshots/Monthly-2022-01/bld-2022-01.gdb/BLD_EO_SPECIES")
##subset arc object before converting it to R object
snap.sub<-arc.select(snap, 
                     where_clause = "MAJ_GRP1 = 'Vascular Plants - Flowering Plants'
                     OR MAJ_GRP1 = 'Vascular Plants - Ferns and relatives'
                     OR MAJ_GRP1 = 'Vascular Plants - Conifers and relatives'
                     OR MAJ_GRP1 = 'Vertebrates'
                     OR USESA_CD IS NOT NULL
                     OR SARA IS NOT NULL"
                     )
##SQL copied from arcpro - not complete
##USESA_CD IS NOT NULL Or SARA IS NOT NULL Or G_INT_ESA IS NOT NULL Or MAJ_GRP1 = 'Vascular Plants - Conifers and relatives' Or MAJ_GRP1 = 'Vascular Plants - Ferns and relatives' Or MAJ_GRP1 = 'Vascular Plants - Flowering Plants' Or MAJ_GRP1 = 'Vertebrates' Or COSEWIC IS NOT NULL Or MAJ_GRP1 = 'Insects - Bees' Or MAJ_GRP2 = 'Butterflies and Skippers' Or MAJ_GRP2 = 'Caddisflies' Or MAJ_GRP2 = 'Crayfishes' Or MAJ_GRP2 = 'Dragonflies and Damselflies' Or MAJ_GRP2 = 'Freshwater Mussels' Or MAJ_GRP3 = 'Freshwater Snails' Or MAJ_GRP3 = 'Terrestrial Snails'


#snap.sf<-arc.data2sf(snap.sub) ##convert to sf objecrt
