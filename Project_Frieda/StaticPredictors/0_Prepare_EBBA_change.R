## This script is based on work of Dr. Gabriel Ortega Solis (copy and pasted from script "6_change_data.qmd)
change <- readxl::read_excel("Checked_data/change_data.xlsx") %>%
  rename(cellID = cell50x50_change, verbatim_name = birdlife_scientific_name) %>%
  separate_longer_delim(., cellID, "_") %>%
  filter(!Change == "G") %>%
  mutate(prob = ifelse(str_detect(Change, "B|D|F") == T, sqrt(0.5), 1)) %>%
  left_join(., wide_grid, by = "cellID") %>%
  pivot_longer(
    cols = matches("^\\d"),
    names_to = "cell_grouping",
    values_to = "cell_label"
  ) %>%
  unique() %>%
  mutate(
    cell_grouping = as.numeric(cell_grouping),
    cell_label = as.numeric(cell_label)
  ) %>%
  select(-cellID, -birdlife_code, -Change) %>%
  unique()
saveRDS(change, "~/GitHub/BEAST_General_Procedures/Project_Frieda/StaticPredictors/out/rds/EBBA_change.rds")


## My turn: Prepare EBBA change data
rm(list=ls())
library(dplyr); library(sf)

EBBA_change <- readRDS("out/rds/EBBA_change.rds") %>% 
  tidyr::pivot_longer(cols=c("T1", "T2"), names_to = "tp") %>% 
  mutate(tp = case_when(
  tp == "T1" ~ "1",
  tp == "T2" ~ "2")) %>% filter(value == 1) %>% 
  mutate(start_year = case_when(
    tp == "1" ~ "1972",
    tp == "2" ~ "2013"))



source_atlas <- c("c:/Users/wolke/OneDrive - CZU v Praze/Datasets/Processed/Atlases/Replicated/")
# Folder paths to atlas data
source_paths <- c(paste0(source_atlas, "Birds_Atlas_Czechia/"), 
                  paste0(source_atlas, "Birds_Atlas_New_York/"), 
                  paste0(source_atlas, "Birds_atlas_Japan/"), 
                  paste0(source_atlas, "Birds_atlas_EBBA/"))
# Paths to data & grids
data_paths <- c(paste0(source_paths[1],"Birds_Atlas_Czechia_beast_data.rds"), 
                paste0(source_paths[2], "Birds_Atlas_New_York_beast_data.rds"), 
                paste0(source_paths[3], "Birds_atlas_Japan_beast_data.rds"))

grid_paths <- c(paste0(source_paths[1],"Birds_Atlas_Czechia_grid.gpkg"), 
                paste0(source_paths[2], "Birds_Atlas_New_York_grid.gpkg"), 
                paste0(source_paths[3], "Birds_atlas_Japan_grid.gpkg"),
                paste0(source_paths[4], "Birds_atlas_EBBA_grid.gpkg"))

# Vectors for loops:
atlas_names <- c("Birds_Atlas_Czechia", "Birds_Atlas_New_York","Birds_atlas_Japan", "Birds_atlas_EBBA")
time_periods <- c(1,2)

# Define the desired order of factor levels
desired_levels <- factor(c("1", "2","4", "8", "16", "32", "64", "128"), ordered = T,  
                         levels = c("1", "2","4", "8", "16", "32", "64", "128")) 







# Species data  =====================
presence_data <- list()
for (i in seq_along(data_paths)){
  pres_dat <- readRDS(data_paths[i])
  sy <- sort(unique(pres_dat$start_year))
  
  ## Add time-period column 
  pres_dat2 <- pres_dat %>% 
    mutate(tp = case_when(start_year == sy[1] ~ 1,
                          start_year == sy[2] ~ 2)) %>% 
    filter(tp %in% c(1,2)) %>%
    reorder_levels(cell_grouping, order=desired_levels)
  presence_data[[i]] <- pres_dat2
}

# Merge list together
presence_data2 <- plyr::rbind.fill(presence_data, fill=T) %>% select(-repeated)


# Grid data  =====================
# make list with names of layers so we can read them in below
layers_list <- list()
for (i in seq_along(grid_paths)){
  layers <- st_layers(grid_paths[i])$name
  layers_list[[i]] <- layers
}
names(layers_list) <- atlas_names

# read grids to list
grid_list2 <- list()
for (a in seq_along(grid_paths)) {
  grid_list <- sapply(layers_list[[a]], function(i) {
    st_read(grid_paths[[a]], paste(i), quiet = TRUE) %>% 
      st_transform(crs = 4326) %>% reorder_levels( cell_grouping, order=desired_levels)
  }, simplify = FALSE)
  grid_list2[[a]] <- grid_list
}


### Merge new EBBA data to grid info

scales_g <- names(grid_list2[[4]])
scales_d <- unique(EBBA_change$cell_grouping)
scales_list <- list()
for (i in seq_along(scales_g)){
  cur_scale_grid <- grid_list2[[4]][[i]]%>% st_drop_geometry()
  cur_scale_dat <- EBBA_change %>% filter(cell_grouping == scales_d[[i]]) %>% mutate(cell_grouping = as.factor(as.character(cell_grouping)))
  cur_scale_dat$dataset <- "Birds_atlas_EBBA"
  merged <- left_join(cur_scale_dat, cur_scale_grid) 
  scales_list[[i]] <- merged
  
}

EBBA_new <- plyr::rbind.fill(scales_list, fill=T) %>% unique()
saveRDS(EBBA_new, "out/rds/EBBA_change_new.rds")
