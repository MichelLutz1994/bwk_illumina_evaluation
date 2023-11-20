library(pacman)
pacman::p_load(tidyverse, readxl, openxlsx, zeallot)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# load evaluation functions
source("inter_evaluation.R")
source("intra_evaluation.R")

path23 <- 'summary/23_20230913_Inter-Intraassay-LRM_Illumina.xlsx'
path24 <- 'summary/24_20230914_Inter-Intraassay-LRM_Illumina.xlsx'
path25 <- 'summary/25_20230915_Inter-Intraassay-LRM_Illumina.xlsx'
path26 <- 'summary/26_20230918_Inter-Intraassay_LRM_Illumina.xlsx'
path27 <- 'summary/27_20230926_Inter-Intraassay_LRM_Illumina.xlsx'
path28 <- 'summary/28_20230927_Inter-Intraassay-LRM_Illumina.xlsx'

# load all inter samples
df_inter_sample_1 <- get_inter_df(path23, path24)
df_inter_sample_2 <- get_inter_df(path25, path26)
c(df_intra, df_intra_mean, df_intra_std) %<-% intra_evaluation(path27, path28)
df_inter_sample_3 <- df_intra_mean[,-1]
features <- df_intra_mean[,1]
prob_names <- colnames(df_inter_sample_1)

df_inter <- data.frame(feature=features)
df_inter_mean <- data.frame(feature=features)
df_inter_std <- data.frame(feature=features)
colnames(df_inter) <- "feature"
for (i in 1:ncol(df_inter_sample_1)) {
  #mean
  df_probe <- cbind(df_inter_sample_1[,i] ,df_inter_sample_2[,i], df_inter_sample_3[,i])
  df_probe_mean <- round_df(data.frame(x=rowMeans(df_probe,na.rm=TRUE)), digits=1)
  colnames(df_probe_mean) <- paste(prob_names[i], "mean", sep=" \n")
  df_inter_mean <- cbind(df_inter_mean, df_probe_mean)
  df_inter <- cbind(df_inter, df_probe_mean)
  
  #std
  df_probe_std <- round_df(data.frame(apply(df_probe, 1, sd, na.rm=TRUE)), digits=1)
  colnames(df_probe_std) <- paste(prob_names[i], "sd", sep=" \n")
  df_inter_std <- cbind(df_inter_std, df_probe_std)
  df_inter <- cbind(df_inter, df_probe_std)
}

write.xlsx(df_inter, file = "inter_result.xlsx")
