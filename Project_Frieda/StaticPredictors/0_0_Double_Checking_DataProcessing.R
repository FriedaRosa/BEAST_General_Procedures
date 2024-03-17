



### RUN 0_1_Atlas_prep.qmd until (including) chunk: "Read & Process Data"

### Summary before reducing the data =============================================================================================== #

presence_data2 %>% filter(cell_grouping == 1) %>% group_by(dataset, tp) %>% 
  summarize(n_sp = n_distinct(verbatim_name),
            mean_effort = mean(effort, na.rm=T),
            sum_effort = sum(effort, na.rm=T),
            samp_effort_type = unique(samp_effort_type))

## Total Number cells sampled in both time periods
presence_data2 %>% ungroup() %>% filter(cell_grouping == 1) %>% group_by(dataset) %>% summarise(n_cells = n_distinct(cell_label))

## Cells sampled twice
common_cells <- presence_data2 %>% ungroup() %>% filter(cell_grouping == 1) %>%
  group_by(dataset, cell_label) %>%
  mutate(num_periods_cells = n_distinct(tp)) %>%
  mutate(repeated = ifelse(num_periods_cells == 2, 1, 0)) %>% 
  ungroup() %>% group_by(dataset) %>% 
  select(dataset, cell_label, num_periods_cells) %>% distinct(.)

pres_dat_red_cells <- full_join(presence_data2, common_cells) %>% filter(repeated == 1)

## Species sampled twice
presence_data2 %>%
  group_by(dataset, verbatim_name) %>%
  summarise(num_periods_sp = n_distinct(tp)) %>%ungroup() %>% group_by(dataset, num_periods_sp) %>% summarise(n_sp = n_distinct(verbatim_name))

### Species sampled twice in cells that were sampled twice:
pres_dat_red_cells %>%
  group_by(dataset, verbatim_name) %>%
  summarise(num_periods_sp = n_distinct(tp)) %>%ungroup() %>% group_by(dataset, num_periods_sp) %>% summarise(n_sp = n_distinct(verbatim_name))

# Final Species Numbers (with change data:)
common_sp <- pres_dat_red_cells %>%
  group_by(dataset, verbatim_name) %>%
  summarise(num_periods_sp = n_distinct(tp)) %>%ungroup() %>% group_by(dataset, num_periods_sp)

pres_dat_red_sp <- full_join(pres_dat_red_cells, common_sp) %>% filter(num_periods_sp == 2)
pres_dat_red_sp %>% filter(num_periods_sp == 2 & cell_grouping == 1) %>% group_by(dataset) %>% summarise(n_sp = length(unique(verbatim_name)))

### =============================================================================================== #





pres_dat2 %>% pull(end_year) %>% unique()
pres_dat2 %>% pull(start_year) %>% unique()

# N species total
pres_dat2 %>% filter(cell_grouping == 1) %>% distinct(verbatim_name, tp) %>% group_by(tp) %>% summarize(n = n_distinct(verbatim_name))

# Total cells sampled overall
pres_dat2 %>% filter(cell_grouping == 1) %>% ungroup() %>% select(tp, cell_label, repeated) %>% distinct(tp, cell_label, .keep_all = T) %>% group_by(tp) %>%
  summarize(n = n_distinct(cell_label))

# in tp 1
pres_dat2 %>% filter(cell_grouping == 1) %>% ungroup() %>% select(tp, cell_label, repeated) %>% distinct(tp, cell_label, .keep_all = T) %>% filter(tp==1) %>%
  summarize(n = n_distinct(cell_label))

# in tp 2
pres_dat2 %>% filter(cell_grouping == 1) %>% ungroup() %>% select(tp, cell_label, repeated) %>% distinct(tp, cell_label, .keep_all = T) %>% filter(tp==2) %>%
  summarize(n = n_distinct(cell_label))


# N cells total in Atlas
grid_list2[[i]]$cell1grid %>% summarize(n_cells = n_distinct(cell_label))

# Sampled in both periods
pres_dat2 %>% filter(cell_grouping == 1) %>% ungroup() %>% select(tp, cell_label, repeated) %>% distinct(tp, cell_label, .keep_all = T) %>% filter(tp == 1) %>%
  summarize(n = sum(repeated))

pres_dat2 %>% filter(cell_grouping == 1) %>% ungroup() %>% select(tp, cell_label, repeated) %>% distinct(tp, cell_label, .keep_all = T) %>% filter(tp == 2) %>%
  summarize(n = sum(repeated))


setdiff(unique( pres_dat2 %>% filter(cell_grouping == 1) %>% ungroup() %>% select(tp, cell_label, repeated) %>% distinct(tp, cell_label, .keep_all = T) %>% filter(tp == 2) %>% pull(cell_label)),unique(pres_dat2 %>% filter(cell_grouping == 1) %>% ungroup() %>% select(tp, cell_label, repeated) %>% distinct(tp, cell_label, .keep_all = T) %>% filter(tp == 1) %>% pull(cell_label))) %>% length()
setdiff(unique( pres_dat2 %>% filter(cell_grouping == 1) %>% ungroup() %>% select(tp, cell_label, repeated) %>% distinct(tp, cell_label, .keep_all = T) %>% filter(tp == 1) %>% pull(cell_label)),unique(pres_dat2 %>% filter(cell_grouping == 1) %>% ungroup() %>% select(tp, cell_label, repeated) %>% distinct(tp, cell_label, .keep_all = T) %>% filter(tp == 2) %>% pull(cell_label))) %>% length()
intersect(unique( pres_dat2 %>% filter(cell_grouping == 1) %>% ungroup() %>% select(tp, cell_label, repeated) %>% distinct(tp, cell_label, .keep_all = T) %>% filter(tp == 1) %>% pull(cell_label)),unique(pres_dat2 %>% filter(cell_grouping == 1) %>% ungroup() %>% select(tp, cell_label, repeated) %>% distinct(tp, cell_label, .keep_all = T) %>% filter(tp == 2) %>% pull(cell_label))) %>% length()

# N entries
pres_dat2 %>% filter(cell_grouping == 1) %>% ungroup() %>% select(tp, cell_label, verbatim_name) %>% distinct(tp, cell_label, verbatim_name, .keep_all = T) %>% 
  filter(tp==1) %>% summarize(n = n())

pres_dat2 %>% filter(cell_grouping == 1) %>% ungroup() %>% select(tp, cell_label, verbatim_name) %>% distinct(tp, cell_label, verbatim_name, .keep_all = T) %>% 
  filter(tp==2) %>% summarize(n = n())


# Species sampled twice
pres_dat2 %>% filter(cell_grouping == 1) %>%
  group_by(verbatim_name) %>% select(verbatim_name, tp) %>% filter(tp %in% c(1,2)) %>%
  mutate(num_periods = n_distinct(tp)) %>% group_by(tp) %>%
  filter(num_periods == 2) %>% summarize(n = n_distinct(verbatim_name))

intersect(unique(pres_dat2 %>% filter(cell_grouping == 1 & tp == 1) %>% pull(verbatim_name)),unique(pres_dat2 %>% filter(cell_grouping == 1 & tp == 2) %>% pull(verbatim_name))) %>% length()



# Species sampled once
pres_dat2 %>% filter(cell_grouping == 1) %>%
  group_by(verbatim_name) %>% select(verbatim_name, tp) %>%filter(tp %in% c(1,2)) %>%
  mutate(num_periods = n_distinct(tp)) %>% group_by(tp) %>%
  filter(num_periods == 1) %>% summarize(n = n_distinct(verbatim_name))


## Samp effort
pres_dat2 %>% filter_at(vars(effort, samp_effort_type), any_vars(is.na(.))) %>% ungroup() %>% select(verbatim_name, tp, samp_effort_type, effort) %>% distinct(.) %>% group_by(tp) %>% summarise(n_sp = n_distinct(verbatim_name))

pres_dat2 %>% filter(cell_grouping == 1) %>% ungroup() %>% filter(tp %in% c(1,2)) %>%
  distinct(tp, samp_effort_type)

# avg effort
pres_dat2 %>% filter(cell_grouping == 1) %>% ungroup() %>% filter(tp %in% c(1,2)) %>% group_by(tp) %>% na.omit() %>% summarise(avg_effort = mean(effort))
# sum effort
pres_dat2 %>% filter(cell_grouping == 1) %>% ungroup() %>% filter(tp %in% c(1,2)) %>% group_by(tp) %>% na.omit() %>% summarise(avg_effort = sum(effort))




### AOO
species_data %>% filter(cell_grouping == 1) %>% group_by(dataset, tp) %>% summarise(n=n_distinct(verbatim_name))
species_data %>% filter(cell_grouping == 1) %>% group_by(dataset, tp) %>% summarize(meanAOO = mean(AOO))
species_data %>% filter(cell_grouping == 1) %>% group_by(dataset, tp) %>% summarize(medianAOO = median(AOO))


species_data %>% filter(cell_grouping == 1) %>% group_by(dataset, tp) %>% summarize(meanrelOcc = mean(relative_occupancy_Ncells))
species_data %>% filter(cell_grouping == 1) %>% group_by(dataset, tp) %>% summarize(medianrelOcc = median(relative_occupancy_Ncells))
species_data %>% filter(cell_grouping == 1) %>% group_by(dataset, tp) %>% filter_all(any_vars(is.na(.)))


setdiff(species_data %>% filter(dataset == "Birds_atlas_EBBA" & tp == 1) %>% distinct(verbatim_name, dataset, tp) %>% pull(verbatim_name), species_data %>% filter(dataset == "Birds_atlas_EBBA" & tp == 2) %>% distinct(verbatim_name, dataset, tp) %>% pull(verbatim_name))



### OAR

# Saturated Species Checks
x <- species_data %>% 
  filter(relative_occupancy_Ncells == 1) %>% 
  group_by(dataset, cell_grouping, tp) %>%
  summarise(N_sp = n_distinct(verbatim_name),
            mean_Ncells = mean(occupancy_Ncells),
            total_SR_atlas = total_SR_atlas) %>%
  mutate(proportion_perc = round(((N_sp/total_SR_atlas)*100),0)) %>% distinct() # check saturated occupancies

x




species_data_new %>% filter(is.na(m_AOO_a)) %>% pull(verbatim_name) %>% unique() # 28 species
species_data_new %>% filter(is.na(m_AOO_a)) %>% pull(dataset) %>% unique() # from CZ data
species_data_new %>% group_by(dataset, tp, verbatim_name) %>% filter(cell_grouping == 1)

# Weird species with negative D:
## They have very low occupancy of 1-3 cells
species_data_new %>% group_by(dataset, tp) %>% filter(D_AOO_a < 0) %>% select(dataset, tp, verbatim_name, mean_area, AOO, occupancy_Ncells, relative_occupancy_Ncells, D_AOO_a) %>%
  ggplot(aes(x = log(mean_area), y = log(AOO), col= verbatim_name))+
  geom_point(show.legend = F)+
  geom_line(show.legend = F, aes(group =verbatim_name))+
  facet_wrap(tp~.)+
  theme_bw()





### Telfer

species_data_new2 %>% filter(cell_grouping == 1) %>% filter_all(any_vars(is.na(.))) ## 11 species without OAR from New York
species_data_new2 %>% filter(cell_grouping == 1) %>% filter_all(any_vars(!is.na(.)))  %>% select(dataset, verbatim_name, tp) %>% distinct(.)  %>% group_by(dataset, tp) %>% summarise(n_sp = n_distinct(verbatim_name))


species_data_new2 %>% filter(cell_grouping == 1) %>% filter_all(any_vars(is.na(.))) # NAs for the 45 CZ species that are saturated
