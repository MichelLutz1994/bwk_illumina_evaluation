library(pacman)
pacman::p_load(tidyverse, readxl, openxlsx, zeallot, stringr)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

evaluate_coverage <- function(path1, path2, path3, path4, path5, path6){
  # load evaluation functions
  source("coverage_inter_evaluation.R")
  source("coverage_intra_evaluation.R")
  
  df_inter_sample_1 <- load_inter_coverage(path1, path2)
  df_inter_sample_2 <- load_inter_coverage(path3, path4)
  c(df_intra, df_intra_mean, df_intra_std) %<-% intra_evaluation(path5, path6)
  prob_names <- colnames(df_inter_sample_1)
  
  df_inter <- data.frame(x=1:nrow(df_inter_sample_1))
  df_inter_mean <- data.frame(x=1:nrow(df_inter_sample_1))
  df_inter_std <- data.frame(x=1:nrow(df_inter_sample_1))
  colnames(df_inter) <- "feature"
  for (i in 1:ncol(df_inter_sample_1)) {
    #mean
    df_probe <- cbind(df_inter_sample_1[,i] ,df_inter_sample_2[,i], df_intra_mean[,i])
    df_probe_mean <- round_df(data.frame(x=rowMeans(df_probe,na.rm=TRUE)), digits=1)
    colnames(df_probe_mean) <- paste(prob_names[i], "mean", sep="\n")
    df_inter_mean <- cbind(df_inter_mean, df_probe_mean)
    df_inter <- cbind(df_inter, df_probe_mean)
    
    #std
    df_probe_std <- round_df(data.frame(apply(df_probe, 1, sd, na.rm=TRUE)), digits=1)
    colnames(df_probe_std) <- paste(prob_names[i], "sd", sep="\n")
    df_inter_std <- cbind(df_inter_std, df_probe_std)
    df_inter <- cbind(df_inter, df_probe_std)
  }
  df_inter <- df_inter[,-1]
  df_inter_mean <- df_inter_mean[,-1]
  df_inter_std <- df_inter_std[,-1]
  
  return(list(df_inter, df_inter_mean, df_inter_std, df_intra, df_intra_mean, df_intra_std))
}

path23 <- "coverage/coverage_data/23"
path24 <- "coverage/coverage_data/24"
path25 <- "coverage/coverage_data/25"
path26 <- "coverage/coverage_data/26"
path27 <- "coverage/coverage_data/27"
path28 <- "coverage/coverage_data/28"

#c(df_inter, df_inter_mean, df_inter_std, df_intra, df_intra_mean, df_intra_std) %<-% evaluate_coverage(path23, 
#                                                                path24, 
#                                                                path25, 
#                                                                path26, 
#                                                                path27,
#                                                                path28)
#write.xlsx(df_inter, file = "coverage_inter_result.xlsx")
#write.xlsx(df_intra, file = "coverage_intra_result.xlsx")
