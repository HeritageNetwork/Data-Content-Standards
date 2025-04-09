## Select EO data from Biotics

library(tidyverse)

#dat <- readxl::read_xlsx("C:/Users/MaxTarjanPhD/Downloads/Biotics_EO_Summary.xlsx", sheet = "EO_Summary_v2_202401")
dat <- read.csv(paste0("Output/ImperiledSubsetGlobal-EOs-", Sys.Date(), ".csv"))
#dat <- read.csv("Output/ImperiledSubsetGlobal-EOs-2025-04-01.csv")

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
    NUM_EOS = ifelse(is.na(NUM_EOS), 0, NUM_EOS),
    NUM_EOS_STD_COUNT = ifelse(is.na(NUM_EOS_STD_COUNT), 0, NUM_EOS_STD_COUNT),
    NUM_EOS_STD_COUNT_LOBS = ifelse(is.na(NUM_EOS_STD_COUNT_LOBS), 0, NUM_EOS_STD_COUNT_LOBS),
    NUM_EOS_STD_COUNT_RANKED = ifelse(is.na(NUM_EOS_STD_COUNT_RANKED), 0, NUM_EOS_STD_COUNT_RANKED),
    NUM_EOS_STD_COUNT_ACCURACY = ifelse(is.na(NUM_EOS_STD_COUNT_ACCURACY), 0, NUM_EOS_STD_COUNT_ACCURACY),
    NUM_EOS_STD_COUNT_40YR = ifelse(is.na(NUM_EOS_STD_COUNT_40YR), 0, NUM_EOS_STD_COUNT_40YR),
    NUM_EOS_STD_COUNT_10YR = ifelse(is.na(NUM_EOS_STD_COUNT_10YR), 0, NUM_EOS_STD_COUNT_10YR),
    TAX1 = dplyr::case_when(
      NAME_CATEGORY_DESC %in% c("Vertebrate Animal", "Invertebrate Animal") ~ "Animals",
      NAME_CATEGORY_DESC %in% c("Nonvascular Plant", "Vascular Plant") ~ "Plants"
    )
  ) %>%
  filter(G_RANK %in% c("G1/T1", "G2/T2", "GH/TH") | COSEWIC_DESC %in% c("Endangered", "Threatened") | grepl(x = USESA, pattern = "(^|,)(E|T)"))

### Total number of EOs per taxonomic group. EOs must be current (last observation date within the last 40 years, not extirpated, and with confirmed species identity).
data.plot <- dat %>%
  mutate(taxa = INFO_TAX_CLASS) %>%
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

## Number and proportion of species with EOs
data.plot <- dat %>%
  group_by(SUBNATION_CODE, ELEMENT_GLOBAL_ID) %>%
  summarise(has_eos = ifelse(sum(NUM_EOS_STD_COUNT_40YR)>0, 1, 0)) %>%
  group_by(SUBNATION_CODE) %>%
  summarise(N_Species_with_EOs = sum(has_eos), 
            N_Species = n(), 
            Proportion_Species_with_EOs = sum(has_eos)/n()) %>%
  left_join(data.plot)

## Proportion of EOs visited in the last 10 years
data.plot <- dat %>%
  group_by(SUBNATION_CODE) %>%
  summarise(Proportion_EOs_10_Years_Current = sum(NUM_EOS_STD_COUNT_10YR, na.rm = T)/sum(NUM_EOS_STD_COUNT, na.rm = T)) %>% 
  mutate(Proportion_EOs_10_Years_Current = ifelse(is.na(Proportion_EOs_10_Years_Current), 0, Proportion_EOs_10_Years_Current)) %>%
  left_join(data.plot)

## Proportion of EOs with valid last obs dates
data.plot <- dat %>%
  group_by(SUBNATION_CODE) %>%
  summarise(Proportion_EOs_with_Dates = sum(NUM_EOS_STD_COUNT_LOBS, na.rm = T)/sum(NUM_EOS_STD_COUNT, na.rm = T)) %>% 
  mutate(Proportion_EOs_with_Dates = ifelse(is.na(Proportion_EOs_with_Dates), 0, Proportion_EOs_with_Dates)) %>%
  left_join(data.plot)

## Proportion of EOs Ranked
data.plot <- dat %>%
  group_by(SUBNATION_CODE) %>%
  summarise(Proportion_EOs_Ranked = sum(NUM_EOS_STD_COUNT_RANKED, na.rm = T)/sum(NUM_EOS_STD_COUNT, na.rm = T)) %>% 
  mutate(Proportion_EOs_Ranked = ifelse(is.na(Proportion_EOs_Ranked), 0, Proportion_EOs_Ranked)) %>%
  left_join(data.plot)

## Proportion of EOs with representational accuracy
data.plot <- dat %>%
  group_by(SUBNATION_CODE) %>%
  summarise(Proportion_EOs_with_Accuracy = sum(NUM_EOS_STD_COUNT_ACCURACY, na.rm = T)/sum(NUM_EOS_STD_COUNT, na.rm = T)) %>%
  mutate(Proportion_EOs_with_Accuracy = ifelse(is.na(Proportion_EOs_with_Accuracy), 0, Proportion_EOs_with_Accuracy)) %>%
  left_join(data.plot)

write.csv(data.plot, paste0("Output/Subnational_EO_Summary_", Sys.Date(), ".csv"), row.names = F)

# Get data for plot functions
## Proportion of elements with EOs for all subnations in which they occur
data.qual.eos <- dat %>%
  mutate(taxa = INFO_TAX_CLASS) %>%
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
  mutate(taxa = INFO_TAX_CLASS) %>%
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

## Proportion of EOs with valid observation dates
## use EOs_lobs_valid = count of EOs with valid observation dates
data.qual.eos <- dat %>%
  mutate(taxa = INFO_TAX_CLASS,
         standard = "EOs_with_Dates") %>%
  group_by(taxa, standard) %>%
  filter(!is.na(taxa)) %>%
  summarise(`TRUE` = sum(NUM_EOS_STD_COUNT_LOBS, na.rm=T),
            `FALSE` = sum(NUM_EOS_STD_COUNT, na.rm=T)-sum(NUM_EOS_STD_COUNT_LOBS, na.rm=T)) %>%
  gather(key = "value", value = "n", 3:4) %>%
  group_by(taxa, standard) %>%
  mutate(prop = n/sum(n)) %>%
  ungroup() %>%
  arrange(taxa) %>%
  rbind(data.qual.eos)

## Proportion of EOs with valid ranks
data.qual.eos <- dat %>%
  mutate(taxa = INFO_TAX_CLASS,
         standard = "EOs_with_Rank") %>%
  group_by(taxa, standard) %>%
  filter(!is.na(taxa)) %>%
  summarise(`TRUE` = sum(NUM_EOS_STD_COUNT_RANKED, na.rm=T),
            `FALSE` = sum(NUM_EOS_STD_COUNT, na.rm=T)-sum(NUM_EOS_STD_COUNT_RANKED, na.rm=T)) %>%
  gather(key = "value", value = "n", 3:4) %>%
  group_by(taxa, standard) %>%
  mutate(prop = n/sum(n)) %>%
  ungroup() %>%
  arrange(taxa) %>%
  rbind(data.qual.eos)

## Proportion of EOs with representational accuracy
data.qual.eos <- dat %>%
  mutate(taxa = INFO_TAX_CLASS,
         standard = "EOs_with_Accuracy") %>%
  group_by(taxa, standard) %>%
  filter(!is.na(taxa)) %>%
  summarise(`TRUE` = sum(NUM_EOS_STD_COUNT_ACCURACY, na.rm=T),
            `FALSE` = sum(NUM_EOS_STD_COUNT, na.rm=T)-sum(NUM_EOS_STD_COUNT_ACCURACY, na.rm=T)) %>%
  gather(key = "value", value = "n", 3:4) %>%
  group_by(taxa, standard) %>%
  mutate(prop = n/sum(n)) %>%
  ungroup() %>%
  arrange(taxa) %>%
  rbind(data.qual.eos)

## Proportion of EOs visited in last 10 years
data.qual.eos <- dat %>%
  mutate(taxa = INFO_TAX_CLASS,
         standard = "EOs_10_Years_Current") %>%
  group_by(taxa, standard) %>%
  filter(!is.na(taxa)) %>%
  summarise(`TRUE` = sum(NUM_EOS_STD_COUNT_10YR, na.rm=T),
            `FALSE` = sum(NUM_EOS_STD_COUNT, na.rm=T)-sum(NUM_EOS_STD_COUNT_10YR, na.rm=T)) %>%
  gather(key = "value", value = "n", 3:4) %>%
  group_by(taxa, standard) %>%
  mutate(prop = n/sum(n)) %>%
  ungroup() %>%
  arrange(taxa) %>%
  rbind(data.qual.eos)
  
## Histogram of EO last observation date
data.hist <- dat %>%
  summarise(across(c(NUM_EOS_STD_COUNT_5YR, NUM_EOS_STD_COUNT_10YR, 
                     NUM_EOS_STD_COUNT_20YR, NUM_EOS_STD_COUNT_30YR, 
                     NUM_EOS_STD_COUNT_40YR), 
                   \(x) sum(x, na.rm = TRUE))) %>%
  pivot_longer(cols = everything(), 
               names_to = "Years", 
               values_to = "Sum_Value") %>%
  mutate(Years = str_extract(Years, "\\d+") %>% as.numeric()) # Extracts numeric values

png(filename = "Output/fig.EOs.Lobs.hist.png", width = 6.5, height = 5, units = "in", res=200)
ggplot(data.hist, aes(x = factor(Years), y = Sum_Value)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(x = "Years", y = "Number of EOs visited within the Last x Years") +
  theme_minimal()
dev.off()

#donut.plot.taxa(data.plot = data.qual.eos, standard.plot = "Elements_with_EOs")
#donut.plot.taxa(data.plot = data.qual.eos, standard.plot = "EOs_for_all_Subnations")

## Bind to data.qual.taxa if want to run plots with non-spatial standards
data.qual.taxa <- read.csv("Output/data.qual.taxa.2025-01-30.csv") %>% rbind(data.qual.eos)
write.csv(data.qual.taxa, paste0("Output/data.qual.taxa.", Sys.Date(), ".csv"), row.names = F)
