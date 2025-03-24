## Select EO data from Biotics

library(tidyverse)

#dat <- readxl::read_xlsx("C:/Users/MaxTarjanPhD/Downloads/Biotics_EO_Summary.xlsx", sheet = "EO_Summary_v2_202401")
dat <- read.csv(paste0("Output/ImperiledSubsetGlobal-EOs-", Sys.Date(), ".csv"))

## wrangle data to create summaries of data completeness

## add last obs year, and whether an EO is current
dat <- dat %>%
  mutate(
    G_RANK = dplyr::case_when(
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
    ),
    NUM_EOS_STD_COUNT_40YR = ifelse(is.na(NUM_EOS_STD_COUNT_40YR), 0, NUM_EOS_STD_COUNT_40YR),
    TAX1 = dplyr::case_when(
      NAME_CATEGORY_DESC %in% c("Vertebrate Animal", "Invertebrate Animal") ~ "Animals",
      NAME_CATEGORY_DESC %in% c("Nonvascular Plant", "Vascular Plant") ~ "Plants"
    )
  )


### Total number of EOs per taxonomic group. EOs must be current (last observation date within the last 40 years, not extirpated, and with confirmed species identity).
data.plot <- dat %>%
  mutate(taxa = NSX_TAX) %>%
  group_by(taxa) %>%
  summarise(n_EOs = sum(NUM_EOS_STD_COUNT_40YR, na.rm = T))

fig.temp <- ggplot(data.plot, aes(x = reorder(taxa, n_EOs), y = n_EOs)) +
  geom_col(fill = "steelblue") +
  coord_flip() +  # Flips the bars for readability
  labs(
    x = "",
    y = "Number of EOs"
  ) +
  theme_minimal()
fig.temp

## Write out plot
png(filename = paste0("Output/fig.n_EOs.taxa.png"), width = 1200, height = 1200, res=200)
fig.temp
dev.off()

## Number of current EOs per subnation
data.plot <- dat %>% 
  group_by(SUBNATION_CODE) %>%
  summarise(Number_of_EOs = sum(NUM_EOS_STD_COUNT_40YR, na.rm = T))
data.plot <- dat %>%
  group_by(SUBNATION_CODE, ELEMENT_GLOBAL_ID) %>%
  summarise(has_eos = ifelse(sum(NUM_EOS_STD_COUNT_40YR)>0, 1, 0)) %>%
  group_by(SUBNATION_CODE) %>%
  summarise(N_Species_with_EOs = sum(has_eos), 
            N_Species = n(), 
            Proportion_Species_with_EOs = sum(has_eos)/n()) %>%
  left_join(data.plot)
  
write.csv(data.plot, paste0("Output/Subnational_EO_Summary_", Sys.Date(), ".csv"), row.names = F)

library(sf)
library(rnaturalearth)
library(scales)
world <- rnaturalearth::ne_states(country = c("United States of America", "Canada"), returnclass = "sf")
world <- st_transform(world, "+proj=lcc +lat_1=33 +lat_2=45 +lon_0=-96")

# Merge map with EO data
map_data <- world %>%
  mutate(SUBNATION_CODE = gsub("^(US-|CA-)", "", iso_3166_2)) %>%
  left_join(data.plot, by = "SUBNATION_CODE") %>%
  mutate(Number_of_EOs = ifelse(is.na(Number_of_EOs), 0, Number_of_EOs))

# Get bounding box for cropping
bbox <- st_bbox(map_data %>% filter(!is.na(Number_of_EOs) & Number_of_EOs > 0))

# Plot map
#colors.plot <- c("white", "yellow", "orange", "red", "darkred")
colors.plot <- c("white", "#c7e9c0", "#7fbc41", "#2c7d32", "#00441b")
subnation.map.function <- function(data.plot, standard.plot) {
  fig.temp <- ggplot(map_data) +
    geom_sf(aes(fill = !!sym(standard.plot)), color = "black") +
    scale_fill_gradientn(colors = colors.plot,
                         values = scales::rescale(c(0, 1, max(map_data[[standard.plot]], na.rm = TRUE)/3, max(map_data[[standard.plot]], na.rm = TRUE)/3*2, max(map_data[[standard.plot]], na.rm = TRUE))),
                         na.value = "white",
                         name = gsub("_", " ", standard.plot), labels = comma) +
    coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]), ylim = c(bbox["ymin"], bbox["ymax"]), expand = FALSE) +
    theme_minimal() +
    #labs(title = "Number of Element Occurrences (EOs) by Subnation",
    #     subtitle = "US States and Canadian Provinces",
    #     fill = "EOs Count") +
    theme(legend.position = "bottom",
          legend.key.width = unit(1, "cm"), # Make the legend key width smaller
          legend.title = element_text(size = 12),  # Increase title font size
          legend.text = element_text(size = 10))   # Increase text font size
  print(fig.temp)
}

subnation.map.function(data.plot = data.plot, standard.plot = "Number_of_EOs")
map.standards <- data.plot %>% select(-SUBNATION_CODE) %>% names()
#map.standards <- c("Proportion_Species_with_EOs", "Number_of_EOs")
for (j in 1:length(map.standards)) {
  png(filename = paste0("Output/map.", map.standards[j] ,".subnation.png"), width = 1200, height = 1200*.7, res=200)
  subnation.map.function(data.plot = data.plot, standard.plot = map.standards[j])
  dev.off()
}

# Get data for plot functions
## Proportion of elements with EOs for all subnations in which they occur
data.qual.eos <- dat %>%
  mutate(taxa = TAX1) %>%
  filter(!is.na(taxa)) %>%
  group_by(ELEMENT_GLOBAL_ID, SUBNATION_CODE, taxa) %>%
  summarize(
    n_eo = sum(NUM_EOS_STD_COUNT_40YR, na.rm=T),
    .groups = "drop"
  ) %>%
  mutate(has_eo = ifelse(n_eo > 0, 1, 0)) %>%
  group_by(ELEMENT_GLOBAL_ID, taxa) %>%
  summarize(
    total_subnations = n(),
    total_with_eos = sum(has_eo),
    total_without_eos = total_subnations - total_with_eos,
    .groups = "drop"
  ) %>%
  mutate(prop_subn_w_eo = total_with_eos/total_subnations,
         eos_all_subn = ifelse(prop_subn_w_eo == 1, T, F)) %>%
  group_by(taxa, eos_all_subn) %>%
  summarise(n = n(), .groups = "drop") %>%
  spread(key = eos_all_subn, value = n, fill = 0) %>%
  gather(key = "eos_all_subn", value = "n", `FALSE`, `TRUE`) %>%
  group_by(taxa) %>%
  mutate(total_count = sum(n)) %>% # Total count per INFO_TAX group
  mutate(prop = round(n / total_count, 2)) %>% # Proportion of TRUE/FALSE within each group
  select(taxa, eos_all_subn, n, prop) %>% # Keep relevant columns
  arrange(taxa) %>%
  rename(value = eos_all_subn) %>%
  mutate(standard = "EOs_for_all_Subnations") %>%
  select(taxa, standard, value, n, prop)

## Proportion of elements with at least one current EO.
data.qual.eos <- dat %>%
  mutate(taxa = TAX1) %>%
  filter(!is.na(taxa)) %>%
  group_by(ELEMENT_GLOBAL_ID, taxa) %>%
  summarize(
    n_eo = sum(NUM_EOS_STD_COUNT_40YR, na.rm=T),
    .groups = "drop"
  ) %>%
  mutate(Element_with_EOs = ifelse(n_eo > 0, T, F)) %>%
  group_by(taxa, Element_with_EOs) %>%
  summarise(n = n(), .groups = "drop") %>%
  spread(key = Element_with_EOs, value = n, fill = 0) %>%
  gather(key = "Element_with_EOs", value = "n", `FALSE`, `TRUE`) %>%
  group_by(taxa) %>%
  mutate(total_count = sum(n)) %>% # Total count per INFO_TAX group
  mutate(prop = round(n / total_count, 2)) %>% # Proportion of TRUE/FALSE within each group
  select(taxa, Element_with_EOs, n, prop) %>% # Keep relevant columns
  arrange(taxa) %>%
  rename(value = Element_with_EOs) %>%
  mutate(standard = "Elements_with_EOs") %>%
  select(taxa, standard, value, n, prop) %>%
  rbind(data.qual.eos)

#donut.plot.taxa(data.plot = data.qual.eos, standard.plot = "Elements_with_EOs")
#donut.plot.taxa(data.plot = data.qual.eos, standard.plot = "EOs_for_all_Subnations")

## Bind to data.qual.taxa if want to run plots with non-spatial standards
data.qual.taxa <- read.csv("Output/data.qual.taxa.2025-01-30.csv") %>% rbind(data.qual.eos)
write.csv(data.qual.taxa, paste0("Output/data.qual.taxa.", Sys.Date(), ".csv"), row.names = F)
