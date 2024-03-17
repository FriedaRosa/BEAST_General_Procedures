
## Taxonomic Homogenization ##

# Paths
source_atlas <- c("c:/Users/wolke/OneDrive - CZU v Praze/Datasets/Processed/Atlases/Replicated/")
source_predictors <- c("c:/Users/wolke/OneDrive - CZU v Praze/Dokumenty/PhD_Projects/StaticPredictors/Data/")
source_Git <- c("c:/Users/wolke/OneDrive - CZU v Praze/Dokumenty/GitHub/BEAST_General_Procedures/Project_Frieda/StaticPredictors/")
out_path <- c(paste0(source_Git, "out/"))
tree_path <- paste0(source_predictors, "Weeks_et_at_2022/singe_bird_phylo.tre")
AVONET_path_raw <- paste0(source_predictors, "AVONET/ELEData/TraitData/AVONET_Raw_Data.csv")
avonet_birdlife <- paste0(source_predictors, "AVONET/ELEData/TraitData/AVONET1_Birdlife.csv")
avonet_birdtree <- paste0(source_predictors, "AVONET/ELEData/TraitData/AVONET3_BirdTree.xlsx")

# Data
avonet_raw <- read.csv(AVONET_path_raw) %>% distinct(Species1_BirdLife, Species2_eBird, eBird.species.group, Species3_BirdTree)
avonet_bt <- read_excel(avonet_birdtree, sheet = 2) %>% distinct(Species3, Family3, Order3)
avonet_bl <- read.csv(avonet_birdlife) %>% distinct(Species1, Family1, Order1)
pres_dat_final <- readRDS(paste0(out_path, "rds/presence_data_final.rds"))
tree <- ladderize(read.tree(tree_path))

# Variables
sp_names_atlas <- unique(pres_dat_final$verbatim_name)
sp_names_BirdTree <- gsub("_", " ", tree$tip.label)

#### Non Matches:

# 9 species in data not in taxonomy (Snyonyms)
## 1. "Delichon urbica"  == "Delichon urbicum"
## 2. "Regulus ignicapillus" == "Regulus ignicapilla"
## 3. "Saxicola torquata" == "Saxicola torquatus"
## 4. "Hydropogne caspia" == "Hydroprogne caspia"
## 5. "Egretta intermedia" == "Ardea intermedia"
## 6. "Luscinia akahige" == "Larvivora akahige"
## 7. "Luscinia komadori" == "Larvivora komadori"
## 8. "Poecile varius" == "Sittiparus varius"
## 9. "Sapheopipo noguchii" == "Dendrocopos noguchii"

## Create taxon Lookup frame:
tax_frame <- as.data.frame(sp_names_atlas)
tax_frame2 <- tax_frame %>%
  mutate(
    sp_names_atlas_adapted = case_when(
      sp_names_atlas == "Delichon urbica" ~ "Delichon urbicum",
      sp_names_atlas == "Regulus ignicapillus" ~ "Regulus ignicapilla",
      sp_names_atlas == "Saxicola torquata" ~ "Saxicola torquatus",
      sp_names_atlas == "Hydropogne caspia" ~ "Hydroprogne caspia",
      sp_names_atlas == "Egretta intermedia" ~ "Ardea intermedia",
      sp_names_atlas == "Luscinia akahige" ~ "Larvivora akahige",
      sp_names_atlas == "Luscinia komadori" ~ "Larvivora komadori",
      sp_names_atlas == "Poecile varius" ~ "Sittiparus varius",
      sp_names_atlas == "Sapheopipo noguchii" ~ "Dendrocopos noguchii",
      .default = sp_names_atlas
    )
  ) %>%
  rename("verbatim_name" = "sp_names_atlas")

not_inBL <- setdiff(tax_frame2$sp_names_atlas_adapted, avo_raw$Species1_BirdLife)
not_inBL_and_BT <- setdiff(not_inBL, avo_raw$Species3_BirdTree) 

tax_frame2$sp_names_atlas_adapted_keep <- tax_frame2$sp_names_atlas_adapted

tax_frame3 <- merge(tax_frame2, avo_raw, by.x=c("sp_names_atlas_adapted"), by.y=c("Species1_BirdLife"), all.x = T)
tax_frame4 <- merge(tax_frame3, avo_raw, by.x=c("sp_names_atlas_adapted"), by.y=c("Species3_BirdTree"), all.x = T)
tax_frame5 <- tax_frame4 %>% mutate(
  Species2_eBird = coalesce(Species2_eBird.x, Species2_eBird.y),
  eBird.species.group = coalesce(eBird.species.group.x, eBird.species.group.y)
) %>% select(-eBird.species.group.x, -eBird.species.group.y, -Species2_eBird.x, -Species2_eBird.y) %>%
  rename("sp_BirdTree" = "Species3_BirdTree", 
         "sp_eBird" = "Species2_eBird", 
         "sp_group_eBird" = "eBird.species.group",
         "sp_BirdLife" = "Species1_BirdLife",
         "sp_atlas_adapted" = "sp_names_atlas_adapted") %>% distinct(.) #select(sp_atlas, Genus.name, Family.name, Order.name) %>% unique()

write.csv(tax_frame5, paste0(out_path, "csv/Tax_lookup_BirdLife_eBird_BirdTree_Atlas.csv"))

rm(sp_names_atlas, avo_bl, tree, avo_raw, avo_bt, tax_frame, tax_frame2, tax_frame3, tax_frame4)

