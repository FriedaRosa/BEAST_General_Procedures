# Variables
# Folders
source_atlas <- c("c:/Users/wolke/OneDrive - CZU v Praze/Datasets/Processed/Atlases/Replicated/")
source_predictors <- c("c:/Users/wolke/OneDrive - CZU v Praze/Dokumenty/PhD_Projects/StaticPredictors/Data/")
source_Git <- c("c:/Users/wolke/OneDrive - CZU v Praze/Dokumenty/GitHub/BEAST_General_Procedures/Project_Frieda/StaticPredictors/")

# Folder paths to atlas data
source_paths <- c(paste0(source_atlas, "Birds_Atlas_Czechia/"), 
                  paste0(source_atlas, "Birds_Atlas_New_York/"), 
                  paste0(source_atlas, "Birds_atlas_Japan/"), 
                  paste0(source_atlas, "Birds_atlas_EBBA/"))

# Folder path to output folder
out_path <- c(paste0(source_Git, "out/"))

# Paths to data & grids
data_paths <- c(paste0(source_paths[1],"Birds_Atlas_Czechia_beast_data.rds"), 
                paste0(source_paths[2], "Birds_Atlas_New_York_beast_data.rds"), 
                paste0(source_paths[3], "Birds_atlas_Japan_beast_data.rds"),
                paste0(source_paths[4], "2Birds_atlas_EBBA_beast_data.rds"))

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


















## Merge data + grid and reduce to cells that are well sampled
grid_list <- list()
df_filtered_list <- list()
presence_sf_list <- list()

for (i in seq_along(data_paths)){
  pres_dat <- readRDS(data_paths[i])
  sy <- sort(unique(pres_dat$start_year))
  
  pres_dat2 <- pres_dat %>%     
    ungroup() %>%
    # Add time-period column
    mutate(tp = case_when(start_year == sy[1] ~ 1, 
                          start_year == sy[2] ~ 2)) %>% 
    filter(tp %in% c(1,2)) %>%
    # Reorder spatial scales from small to large
    reorder_levels(cell_grouping, order=desired_levels) %>% 
    select(dataset, tp, cell_grouping, 
           cell_label, cell_lat, cell_long, area, 
           verbatim_name) 
  # presence_data[[i]] <- pres_dat2
  
  
  ## Data Checks:
  # Summary sp and site numbers: Before reduction
  pres_dat2 %>% 
    filter(cell_grouping == 1) %>% 
    group_by(tp) %>% 
    summarise(n_sp = n_distinct(verbatim_name),
              n_sites = n_distinct(cell_label))
  pres_dat2 %>% 
    ungroup() %>% 
    summarise(n_sp = n_distinct(verbatim_name)) # 989 sp in total (before reduction)
  ## End
  
  ## Filter level 1: Cells sampled twice 
  common_cells <- pres_dat2 %>% 
    ungroup() %>% 
    distinct(dataset, cell_grouping, cell_label, tp) %>% 
    group_by(cell_grouping, cell_label) %>%
    # How often each cell was sampled:
    mutate(num_periods_cells = n_distinct(tp)) %>% 
    select(cell_grouping, cell_label, num_periods_cells, dataset) %>% 
    distinct()
  presence_data_rep <- full_join(pres_dat2, common_cells)
  
  ## Data Checks: 
  common_cells %>% 
    filter(num_periods_cells == 2 & cell_grouping == 1) %>%
    group_by(dataset, cell_grouping) %>% 
    summarise(n = n_distinct(cell_label))
  
  excluded_cells <- common_cells %>% 
    filter(num_periods_cells %in% c(0, 1)) %>% 
    distinct(dataset, cell_grouping, cell_label)
  
  ## End
  
  ## Filter level 2: Species sampled twice in the remaining cells
  common_sp <- presence_data_rep %>% 
    filter(num_periods_cells == 2 & cell_grouping == 1) %>% 
    group_by(dataset, verbatim_name) %>%
    summarise(num_periods_sp = n_distinct(tp))
  
  ## Data Checks:
  excluded_sp <- common_sp %>% 
    filter(num_periods_sp == 1) %>% 
    distinct(verbatim_name)
  ## End
  
  presence_data_filt <- full_join(presence_data_rep, common_sp)  %>%  
    filter(num_periods_cells == 2 & num_periods_sp == 2)
  
  df_filtered_list[[i]] <- presence_data_filt
  
  ## Grids
  grids <- sapply("cell1grid", function(a){
    st_read(grid_paths[[i]], paste(a), quiet = TRUE) %>% 
      reorder_levels(cell_grouping, order=desired_levels) %>%
      select(cell_grouping, cell_label)
  }, simplify = FALSE)
  grid_list[[i]] <- grids$cell1grid
  
  
  presence_sf <- full_join(grids$cell1grid, presence_data_filt) %>%
    mutate(exclude_cell = case_when(is.na(dataset) | is.na(verbatim_name) ~ 1,
                                    .default = 0))
  
  presence_sf_list[[i]] <- presence_sf
    
  }
  

df_filtered_full <- plyr::rbind.fill(df_filtered_list)

## Data Checks:
df_filtered_full %>% distinct(verbatim_name) %>% nrow()
colSums(is.na(df_filtered_full))



## Plot:
library(ggplot2)
sp <- unique(presence_sf$verbatim_name, na.rm=T)
presence_sf %>% filter(verbatim_name %in% c(sp[3], NA)) %>%
ggplot()+
  geom_sf(aes(fill = verbatim_name))

ggplot(presence_sf_list[[1]] %>% distinct(cell_label, num_periods_cells))+
  geom_sf(aes(fill = as.factor(num_periods_cells)))

