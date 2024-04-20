library(dplyr)

rm(list = ls())


morans_list_CZ <- readRDS("out/rds/morans_list_CZ.rds")
morans_list_NY <- readRDS("out/rds/morans_list_NY.rds")
morans_list_JP <- readRDS("out/rds/morans_list_JP.rds")
morans_list_EU1 <- readRDS("out/rds/morans_list_EU.rds")
morans_list_EU2 <- readRDS("out/rds/morans_list_EU2.rds")


data_list_CZ <- readRDS("out/rds/SAC_df_CZ.rds")
data_list_NY <- readRDS("out/rds/SAC_df_NY.rds")
data_list_JP <- readRDS("out/rds/SAC_df_JP.rds")

data_list_EU1 <- readRDS("out/rds/SAC_df_EU.rds")
data_list_EU2 <- readRDS("out/rds/SAC_df_EU2.rds")

# data_list_EU1_1 <- readRDS("out/rds/SAC_df_EU1_1_127.rds")
# data_list_EU1_2 <- readRDS("out/rds/SAC_df_EU1_128_254.rds")
# data_list_EU1_3 <- readRDS("out/rds/SAC_df_EU1_255_382.rds")
# data_list_EU1_4 <- readRDS("out/rds/SAC_df_EU1_383_511.rds")


# data_EU1 <- rbind(data_list_EU1_1, data_list_EU1_2, data_list_EU1_3, data_list_EU1_4)


morans_list_all <- list(morans_list_CZ, morans_list_NY,morans_list_JP,morans_list_EU1,morans_list_EU2)

data_CZ <- plyr::rbind.fill(data_list_CZ)
data_NY <- plyr::rbind.fill(data_list_NY)
data_JP <- plyr::rbind.fill(data_list_JP)
data_EU1 <- plyr::rbind.fill(data_list_EU1)
data_EU2 <- plyr::rbind.fill(data_list_EU2)

data_EU1 %>% group_by(dataset, tp) %>% summarise(n = n_distinct(verbatim_name))
data_EU2 %>% group_by(dataset, tp) %>% summarise(n = n_distinct(verbatim_name))

SAC_DF <- rbind(data_CZ, data_NY, data_JP, data_EU1, data_EU2) %>% 
  distinct(dataset, tp, verbatim_name, .keep_all = T)

table(SAC_DF$dataset)

saveRDS(SAC_DF, "out/rds/SAC_final.rds")
colSums(is.na(SAC_DF))
