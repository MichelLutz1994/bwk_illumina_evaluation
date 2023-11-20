library(pacman)
pacman::p_load(tidyverse, readxl, openxlsx, zeallot, stringr)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

intra_evaluation <- function(path1, path2){
  source("coverage_inter_evaluation.R")
  source("tools.R")
  
  df_raw <- load_inter_coverage(path1, path2)
  
  col_names_intra <- lapply(colnames(df_raw),
                            FUN=function(file) (substr(file, 1, str_locate(file, "_a_")[1]-1)))
  
  df_intra <- data.frame(x=1:nrow(df_raw))
  df_intra_mean <- data.frame(x=1:nrow(df_raw))
  df_intra_std <- data.frame(x=1:nrow(df_raw))
  for (i in seq(1, ncol(df_raw), by=3)) {
    #mean
    df_probe_mean <- round_df(data.frame(x=rowMeans(df_raw[,i:(i+2)],na.rm=TRUE)), digits=1)
    colnames(df_probe_mean) <- paste(col_names_intra[i], "mean", sep="\n")
    df_intra_mean <- cbind(df_intra_mean, df_probe_mean)
    df_intra <- cbind(df_intra, df_probe_mean)
    
    #std
    df_probe_std <- round_df(data.frame(apply(df_raw[,i:(i+2)], 1, sd, na.rm=TRUE)), digits=1)
    colnames(df_probe_std) <- paste(col_names_intra[i], "sd", sep="\n")
    df_intra_std <- cbind(df_intra_std, df_probe_std)
    df_intra <- cbind(df_intra, df_probe_std)
  }
  df_intra <- df_intra[,-1]
  df_intra_mean <- df_intra_mean[,-1]
  df_intra_std <- df_intra_std[,-1]
  return(list(df_intra, df_intra_mean, df_intra_std))
}

#path27 <- "coverage/coverage_data/27"
#path28 <- "coverage/coverage_data/28"
#c(df_intra, df_intra_mean, df_intra_std) %<-% intra_evaluation(path27, path28)

#write.xlsx(df_intra, file = "coverage_intra_result.xlsx")



