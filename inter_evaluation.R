library(pacman)
pacman::p_load(tidyverse, readxl, openxlsx, zeallot)

get_inter_df <- function(path1, path2){
  df_1 <-read_excel(path1, sheet=1)[-1,]
  df_2 <-read_excel(path2, sheet=1)[-1,]
  prob_names_1 <- df_1[1,][c(-1,-2)]
  prob_names_2 <- df_2[1,][c(-1,-2)]
  features <- df_1[,1]
  
  # get pure data
  df_1 <- df_1[-(1:7),c(-1,-2)]
  df_2 <- df_2[-(1:7),c(-1,-2)]
  df_1 <- data.frame(apply(df_1, 2, function(x) as.numeric(as.character(x))))
  df_2 <- data.frame(apply(df_2, 2, function(x) as.numeric(as.character(x))))
  
  colnames(df_1) <- prob_names_1
  colnames(df_2) <- prob_names_2
  
  df_inter <- cbind(df_1, df_2) 
  return(df_inter)
}

#path23 <- 'summary/23_20230913_Inter-Intraassay-LRM_Illumina.xlsx'
#path24 <- 'summary/24_20230914_Inter-Intraassay-LRM_Illumina.xlsx'
#path25 <- 'summary/25_20230915_Inter-Intraassay-LRM_Illumina.xlsx'
#path26 <- 'summary/26_20230918_Inter-Intraassay_LRM_Illumina.xlsx'

#df_inter_sample_1 <- get_inter_df(path23, path24)
#df_inter_sample_2 <- get_inter_df(path25, path26)



