library(pacman)
pacman::p_load(tidyverse, readxl, openxlsx, zeallot, stringr)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

load_inter_coverage <- function(path_1, path_2){
  files_1 <- Filter(function(file) (!grepl("plot", file, fixed = TRUE)), list.files(path_1))
  files_2 <- Filter(function(file) (!grepl("plot", file, fixed = TRUE)), list.files(path_2))
  
  df_coverage <- data.frame(x=1:207)
  for (file in files_1){
    df_probe <- data.frame(x=read.csv(paste(path_1, file, sep="/"))$Coverage)
    colnames(df_probe) <- substr(file, 1, str_locate(file, ".coverage.csv")[1]-1)
    df_coverage <- cbind(df_coverage, df_probe)
  }
  for (file in files_2){
    df_probe <- data.frame(x=read.csv(paste(path_2, file, sep="/"))$Coverage)
    colnames(df_probe) <- substr(file, 1, str_locate(file, ".coverage.csv")[1]-1)
    df_coverage <- cbind(df_coverage, df_probe)
  }
  return(df_coverage[,-1])
}

#df_coverage_inter_1 <- load_inter_coverage("coverage/coverage_data/23", "coverage/coverage_data/24")
#df_coverage_inter_2 <- load_inter_coverage("coverage/coverage_data/25", "coverage/coverage_data/26")


