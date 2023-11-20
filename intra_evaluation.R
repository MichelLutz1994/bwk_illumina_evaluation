library(pacman)
pacman::p_load(tidyverse, readxl, openxlsx, zeallot)
################################################################################
# file for evaluation the "intra" variance experiment
# Author: Michel Lutz
# Date: 09/11/2023
# for BWK
################################################################################


intra_evaluation <- function(path27, path28){
  source("tools.R")
  
  df_27 <-read_excel(path27, sheet=1)[-1,]
  df_28 <-read_excel(path28, sheet=1)[-1,]
  prob_names_27 <- df_27[1,][c(-1,-2)]
  prob_names_28 <- df_28[1,][c(-1,-2)]
  features <- df_27[,1]
  
  # get pure data
  df_27 <- df_27[-(1:7),c(-1,-2)]
  df_28 <- df_28[-(1:7),c(-1,-2)]
  df_27 <- data.frame(apply(df_27, 2, function(x) as.numeric(as.character(x))))
  df_28 <- data.frame(apply(df_28, 2, function(x) as.numeric(as.character(x))))
  
  colnames(df_27) <- prob_names_27
  colnames(df_28) <- prob_names_28
  
  df_intra <- data.frame(feature=features[8:nrow(features),])
  df_intra_mean <- data.frame(feature=features[8:nrow(features),])
  df_intra_std <- data.frame(feature=features[8:nrow(features),])
  colnames(df_intra) <- "feature"
  for (i in seq(1, ncol(df_27), by=3)) {
    #mean
    df_probe_mean <- round_df(data.frame(x=rowMeans(df_27[,i:(i+2)],na.rm=TRUE)), digits=1)
    colnames(df_probe_mean) <- paste(substr(prob_names_27[[i]], 0 , nchar(prob_names_27[[i]])-2), "mean")
    df_intra_mean <- cbind(df_intra_mean, df_probe_mean)
    df_intra <- cbind(df_intra, df_probe_mean)
    
    #std
    df_probe_std <- round_df(data.frame(apply(df_27[,i:(i+2)], 1, sd, na.rm=TRUE)), digits=1)
    colnames(df_probe_std) <- paste(substr(prob_names_27[[i]], 0 , nchar(prob_names_27[[i]])-2), "sd")
    df_intra_std <- cbind(df_intra_std, df_probe_std)
    df_intra <- cbind(df_intra, df_probe_std)
  }
  for (i in seq(1, ncol(df_28), by=3)) {
    #mean
    df_probe_mean <- round_df(data.frame(x=rowMeans(df_28[,i:(i+2)],na.rm=TRUE)), digits=1)
    colnames(df_probe_mean) <- paste(substr(prob_names_28[[i]], 0 , nchar(prob_names_28[[i]])-2), "mean")
    df_intra_mean <- cbind(df_intra_mean, df_probe_mean)
    df_intra <- cbind(df_intra, df_probe_mean)
    
    #std
    df_probe_std <- round_df(data.frame(apply(df_28[,i:(i+2)], 1, sd, na.rm=TRUE)), digits=1)
    colnames(df_probe_std) <- paste(substr(prob_names_28[[i]], 0 , nchar(prob_names_28[[i]])-2), "sd")
    df_intra_std <- cbind(df_intra_std, df_probe_std)
    df_intra <- cbind(df_intra, df_probe_std)
  }
  return(list(df_intra, df_intra_mean, df_intra_std))
}


#path27 = 'summary/27_20230926_Inter-Intraassay_LRM_Illumina.xlsx'
#path28 = 'summary/28_20230927_Inter-Intraassay-LRM_Illumina.xlsx'

#c(df_intra, df_intra_mean, df_intra_std) %<-% intra_evaluation(path27, path28)

#write.xlsx(df_intra, file = "summary_intra_result.xlsx")