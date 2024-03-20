library(dplyr)



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


morans_list_all <- list(morans_list_CZ, morans_list_NY,morans_list_JP,morans_list_EU1,morans_list_EU2)

data_CZ <- plyr::rbind.fill(data_list_CZ)
data_NY <- plyr::rbind.fill(data_list_NY)
data_JP <- plyr::rbind.fill(data_list_JP)
data_EU1 <- plyr::rbind.fill(data_list_EU1)
data_EU2 <- plyr::rbind.fill(data_list_EU2)

SAC_DF <- rbind(data_CZ, data_NY, data_JP, data_EU1, data_EU2) %>% 
  distinct(dataset, tp, verbatim_name, .keep_all = T)

table(SAC_DF$dataset)

saveRDS(SAC_DF, "out/rds/SAC_final.rds")
