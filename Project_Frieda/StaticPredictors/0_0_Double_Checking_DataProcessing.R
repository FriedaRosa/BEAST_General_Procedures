



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

common_cells %>% group_by(dataset, cell_grouping, repeated, num_periods_cells) %>% summarise(n = n())

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


presence_data3 %>% nrow() - presence_data3 %>% filter(repeated == 1 & num_periods_sp == 2) %>% nrow()
# 166640 rows removed




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

### AOO CALC CHECKS ===
table(species_data %>% filter(cell_grouping == 1) %>% select(tp, dataset))
# Output:
# tp  Birds_Atlas_Czechia Birds_atlas_EBBA Birds_atlas_Japan Birds_Atlas_New_York
#   1                 200              503               208                  237
#   2                 200              503               208                  237

species_data %>% filter(AOO == 0) #none!
#################

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


range(species_data_new2$D_AOO_a, na.rm=T)

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



##
table(atlas_df$dataset)
atlas_df %>% filter(exclude == 1) %>% group_by(dataset, tp) %>% summarise(n = sum(exclude))# 45 rows have to be excluded because of saturated scales
atlas_df %>% filter(available_scales== 2)
###


# Weird species with negative D:
## They have very low occupancy of 1-3 cells
species_data_new %>% group_by(dataset, tp) %>% filter(D_AOO_a < 0) %>% select(dataset, tp, verbatim_name, mean_area, AOO, occupancy_Ncells, relative_occupancy_Ncells, D_AOO_a) %>%
  ggplot(aes(x = log(mean_area), y = log(AOO), col= verbatim_name))+
  geom_point(show.legend = F)+
  geom_line(show.legend = F, aes(group =verbatim_name))+
  facet_wrap(tp~.)+
  theme_bw()



x <- species_data_bigtable %>% filter(D_AOO_a < 0 & relative_occupancy_area < 1 & cell_grouping == 1)
x %>% group_by(dataset, tp, verbatim_name) #very low occupancy of 1-3 cells
table(x$dataset, x$tp)

x %>% group_by(dataset, tp) %>% rstatix::get_summary_stats(type="common")


# Comparison plot
species_data_bigtable %>% filter(D_AOO_a > 0 & relative_occupancy_area < 1) %>%
  ggplot(aes(x = log(mean_area), y=log(AOO), col = tp, alpha = 0.5))+
  geom_point()+
  geom_line(aes(group = factor(verbatim_name)))+
  theme_classic()+
  facet_wrap(dataset~.)+
  labs(title = "D > 0")+
  viridis::scale_color_viridis(discrete = F)+
  theme(legend.position = "none")

species_data_bigtable %>% filter(D_AOO_a < 0 & relative_occupancy_area < 1) %>%
  ggplot(aes(x = log(mean_area), y=log(AOO), col = tp, alpha = 0.5))+
  geom_point()+
  geom_line(aes(group = factor(verbatim_name)))+
  theme_classic()+
  facet_wrap(dataset~.)+
  labs(title = "D < 0")+
  viridis::scale_color_viridis(discrete = F)+
  theme(legend.position = "none")


sp <- unique(x$verbatim_name)

x_list <- list()
for (i in seq_along(sp)) {
  model_df <- x %>% filter(verbatim_name == sp[i]) %>% unique()
  m_lin <- lm(log(AOO) ~ log(scale), data = model_df)
  dd <- data.frame(
    verbatim_name = sp[i],
    m_AOO = m_lin$coefficients[2],
    b_AOO = m_lin$coefficients[1],
    r2_AOO = summary(m_lin)$r.squared)
  dd$D_AOO <- -2*dd$m_AOO+2
  dd 
  x_list[[i]] <- dd
}

negD <- plyr::rbind.fill(x_list) # 22 species






### Telfer

species_data_new2 %>% filter(cell_grouping == 1) %>% filter_all(any_vars(is.na(.))) ## 11 species without OAR from New York
species_data_new2 %>% filter(cell_grouping == 1) %>% filter_all(any_vars(!is.na(.)))  %>% select(dataset, verbatim_name, tp) %>% distinct(.)  %>% group_by(dataset, tp) %>% summarise(n_sp = n_distinct(verbatim_name))


species_data_new2 %>% filter(cell_grouping == 1) %>% filter_all(any_vars(is.na(.))) # NAs for the 45 CZ species that are saturated


### Plots:



library(ggplot2)
## Plot
ggplot(data = telfer_res_df) +
  geom_histogram(aes(x = Telfer_1_2), bins = 50, 
                 bg = "lightgrey", col = "darkgrey")+
  theme_classic() +
  facet_wrap(dataset~.)


ggplot(data = telfer_res_df %>% filter(dataset == "Birds_atlas_EBBA")) +
  geom_histogram(aes(x = Telfer_1_2), bins = 100, 
                 bg = "lightgrey", col = "darkgrey")+
  theme_classic()

p1 <- ggplot(data = telfer_res_df)+
  geom_boxplot(aes(y = Telfer_1_2, x = dataset), outlier.colour = "red")+
  theme_classic()+
  labs(title = "Telfer index of relative Change")


# Welch One way ANOVA test
res.aov2 <- telfer_res_df %>% welch_anova_test(Telfer_1_2 ~ dataset)
# Pairwise comparisons (Games-Howell)
pwc2 <- telfer_res_df %>% games_howell_test(Telfer_1_2 ~ dataset)
# Visualization: box plots with p-values
pwc2 <- pwc2 %>% add_xy_position(x = "dataset", step.increase = 1)
plot1 <- ggviolin(telfer_res_df, x = "dataset", y = "Telfer_1_2") +
  stat_pvalue_manual(pwc2, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(res.aov2, detailed = TRUE),
    caption = get_pwc_label(pwc2)
  )













p2 <- ggplot(data = Change_Data)+
  geom_boxplot(aes(y = log_R2_1, x = dataset), outlier.colour = "red")+
  theme_classic()+
  labs(title = "log Ratio")
gridExtra::grid.arrange(p1, p2)


# Welch One way ANOVA test
res.aov2 <- Change_Data %>% welch_anova_test(log_R2_1 ~ dataset)
# Pairwise comparisons (Games-Howell)
pwc2 <- Change_Data %>% games_howell_test(log_R2_1 ~ dataset)
# Visualization: box plots with p-values
pwc2 <- pwc2 %>% add_xy_position(x = "dataset", step.increase = 1)
plot2 <- ggviolin(Change_Data, x = "dataset", y = "log_R2_1") +
  stat_pvalue_manual(pwc2, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(res.aov2, detailed = TRUE),
    caption = get_pwc_label(pwc2)
  )

gridExtra::grid.arrange(plot1, plot2)








##### Correlation Matrix 
library(ggcorrplot)
cor_df <- species_data_bigtable %>% 
  select("tp", "scale", 
         "AOO", "relative_occupancy_Ncells", 
         "D_AOO_a", "m_AOO_a", "mean_area",
         "Telfer_1_2", "log_R2_1", "dataset")

p.mat <-  model.matrix(~0+., data=cor_df) %>% ggcorrplot::cor_pmat()

model.matrix(~0+., data=cor_df) %>% 
  cor(use="pairwise.complete.obs") %>% 
  ggcorrplot( lab=TRUE, lab_size=2, hc.order=T, insig ="blank",p.mat = p.mat)




#### Temporal Change ~ D 
m1 <- lm(log_R2_1 ~ D_AOO_a, data = species_data_bigtable, na.action = na.omit)
summary(m1)
m2 <- lm(Telfer_1_2 ~ D_AOO_a, data = species_data_bigtable, na.action = na.omit)
summary(m2)


ggplot(data = species_data_bigtable %>% filter(tp == 1), aes(x = D_AOO_a,y = log_R2_1))+
  geom_jitter()+
  geom_smooth()+
  geom_hline(yintercept = 0, col = "red")+
  facet_wrap(dataset~.)

ggplot(data = species_data_bigtable %>% filter(tp == 2), aes(x = D_AOO_a,y = log_R2_1))+
  geom_jitter()+
  geom_smooth()+
  geom_hline(yintercept = 0, col = "red")+
  facet_wrap(dataset~.)


# Significant:No relationship between log change & AOO
summary(lm(log_R2_1 ~ AOO, data = species_data_bigtable))

ggplot(data = species_data_bigtable %>% filter(tp == 1), aes(x = AOO ,y = log_R2_1))+
  geom_jitter()+
  geom_smooth()+
  facet_wrap(dataset~.,scales = "free")
# Plots =================================================================== #
## 1. Change between tp1 and 2 ~ D from tp1
p1 <- species_data_bigtable %>% filter(tp == 1) %>%
  ggplot(aes(y = log_R2_1, x = D_AOO_a))+
  geom_point()+
  ylab(expression("log"~ frac("tp2_AOO_cell1grid", "tp1_AOO_cell1grid")))+
  xlab("tp1_D")+
  #geom_smooth(method = "lm", formula = y ~ x, se = T, alpha = 0.5)+
  geom_smooth(alpha = 0.5)+
  theme_classic()+
  labs(title = "Prediction of future change",
       subtitle = "Log Ratio between AOO from time period 1 and 2 ~ D from time period 1 ")+
  geom_hline(yintercept = 0, col = "red")+
  facet_wrap(dataset~.)




p2 <- species_data_bigtable %>% filter(tp == 2) %>%
  ggplot(aes(y = log_R2_1, x = D_AOO_a))+
  geom_point()+
  ylab(expression("log"~ frac("tp2_AOO_cell1grid", "tp1_AOO_cell1grid")))+
  xlab("tp2_D")+
  geom_smooth(method = "lm", formula = y ~ x, se = T, alpha = 0.5)+
  theme_classic()+
  labs(title = "Prediction of past change",
       subtitle = "Log Ratio between AOO from time period 1 and 2 ~ D from time period 2 ")+
  geom_hline(yintercept = 0, col = "red")+
  facet_wrap(dataset~.)


gridExtra::grid.arrange(p1,p2, nrow = 1, ncol = 2)


############# Change Data Checks


species_data_bigtable %>% filter_all(any_vars(is.na(.))) %>% 
  select(dataset, tp, verbatim_name) %>% unique() ### There is a problem here !


species_data_bigtable %>% 
  group_by(dataset, tp, cell_grouping) %>% 
  select(verbatim_name, dataset, tp, cell_grouping, 
         log_R2_1, Telfer_1_2,
         AOO, relative_occupancy_Ncells, 
         D_AOO_a, m_AOO_a, mean_area) %>%
  rstatix::get_summary_stats(type = "mean_sd")




Change_Data %>% 
  group_by(dataset) %>% 
  summarise(N_sp = n(),
            meanChange = mean(log_R2_1),
            sdChange = sd(log_R2_1),
            medianChange = median(log_R2_1))


species_data_bigtable %>% 
  filter(!is.na(Telfer_1_2) & cell_grouping == 1) %>% 
  select(verbatim_name, dataset, Telfer_1_2) %>% 
  distinct() %>% 
  na.omit() %>% 
  group_by(dataset) %>% 
  summarise(N_sp = n(),
            meanChange = mean(Telfer_1_2),
            sdChange = sd(Telfer_1_2),
            medianChange = median(Telfer_1_2))




####### 0_2_AtlasPredictors.qmd:

## Co-occurrence
# Some summaries -------------------------------------------------------- #

# summary(co_occurrence_tp1)
# prob.table(co_occurrence_tp1)
# plot(co_occurrence_tp1, cex.axis = 0.5)
# pair.profile(co_occurrence_tp1)
# obs.v.exp(co_occurrence_tp1)

## Geometries

### PCA on Geometries

## Summary Stats ========================================

Geometries %>% group_by(dataset, tp) %>% 
  rstatix::get_summary_stats(type="common")

Geometries %>% mutate_at(c('dataset', 'tp', 'verbatim_name'), as.factor)%>% group_by(dataset, tp) %>% 
  rstatix::get_summary_stats(type="common")


##
str(Atlas_geom)
str(sp_pca_points)
atlas_pca_points <- Atlas_geom %>% ungroup() %>% select(-dataset) %>% as.data.frame()
sp_pca_points <- Species_geom_attributes %>% ungroup() %>% select(-dataset, -verbatim_name, -tp)
pca <- prcomp(atlas_pca_points,  scale.=T)
pca2 <- prcomp(sp_pca_points, scale.=T)

autoplot(prcomp(Atlas_geom %>% ungroup() %>% select(-dataset) %>% as.data.frame()))
autoplot(prcomp(Geometries %>% select(-verbatim_name, -tp, -dataset),  scale.=T, center=T))
autoplot(pca2)
autoplot(pca, loadings = TRUE, loadings.label = TRUE)

autoplot(pca2, loadings = TRUE, loadings.label = TRUE,
         data = Species_geom_attributes, colour = 'dataset')



#####

pca_points <- 
  # first convert the pca results to a tibble
  as_tibble(pca2$x) %>% 
  # now we'll add the penguins data
  bind_cols(Species_geom_attributes)

basic_plot <- 
  ggplot(pca_points, aes(x = PC1, y = PC2)) +
  geom_point(aes(colour = dataset)) +
  theme_light()

# first create a dataframe to extract the convex hull points
pca_hull <- 
  pca_points %>% 
  group_by(dataset) %>% 
  slice(chull(PC1, PC2))

# now, we'll just continue to build on our ggplot object
chull_plot <- 
  basic_plot +
  geom_polygon(data = pca_hull,
               aes(fill = dataset,
                   colour = dataset),
               alpha = 0.3,
               show.legend = FALSE)

chull_plot






