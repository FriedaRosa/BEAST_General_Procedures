## SAC Comparison: JC - NCF

SAC_c <- read.csv("~/PhD_Projects/StaticPredictors/Carmen/all_sac_df.csv")
SAC_f <- readRDS("out/predictors/SAC_DataDf.rds")

SAC_f2 <- SAC_f %>% select(verbatim_name, dataset, tp, moran)
SAC_c2 <- SAC_c %>% select(verbatim_name, dataset_id, period, morans_I, joincount_zscore_pres, joincount_zscore_abs)

SAC_c2 %>% distinct(dataset_id)
SAC_c2 %>% group_by(dataset_id) %>% distinct(period)
SAC_f2 %>% distinct(dataset)
SAC_c3 <- SAC_c2 %>% mutate(dataset = case_when(dataset_id == "birds_czechia" ~ "Birds_Atlas_Czechia",
                                      dataset_id == "birds_japan" ~ "Birds_atlas_Japan"),
                            tp = case_when(period == "1985-1989" ~ 1,
                                           period == "2001-2003" ~ 2,
                                           period == "1997-2002" ~ 1,
                                           period == "2016-2021" ~ 2,     .default = NA)) %>% 
  select(-dataset_id, -period)

SAC_joined <- left_join(SAC_f2, SAC_c3 %>% filter(verbatim_name %in% SAC_f2$verbatim_name)) %>% distinct(verbatim_name, dataset, tp, .keep_all = T)

library(ggplot2)
SAC_joined %>% 
  ggplot() +
  geom_point(aes(morans_I, moran, col=dataset))


SAC_joined %>% 
  ggplot(aes(joincount_zscore_pres, moran, col=dataset)) +
  geom_point(aes(joincount_zscore_pres, moran, col=dataset))+ facet_wrap(tp~.)+
  geom_smooth()

SAC_joined %>% 
  ggplot(aes(joincount_zscore_abs, moran, col=dataset)) +
  geom_point(aes(joincount_zscore_abs, moran, col=dataset))+ facet_wrap(tp~.)+
  geom_smooth()

SAC_joined %>% 
  ggplot(aes(joincount_zscore_pres, morans_I, col=dataset)) +
  geom_point(aes(joincount_zscore_pres, morans_I, col=dataset))+ facet_wrap(tp~.)+
  geom_smooth()
