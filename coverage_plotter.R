library(pacman)
pacman::p_load(tidyverse, readxl, openxlsx, zeallot, stringr, ggplot2, geomtextpath)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("coverage_evaluation.R")

p23 <- "coverage/coverage_data/23"
p24 <- "coverage/coverage_data/24"
p25 <- "coverage/coverage_data/25"
p26 <- "coverage/coverage_data/26"
p27 <- "coverage/coverage_data/27"
p28 <- "coverage/coverage_data/28"

c(df_inter, 
  df_inter_mean, 
  df_inter_std, 
  df_intra, df_intra_mean, 
  df_intra_std) %<-% evaluate_coverage(p23, p24, p25, p26, p27, p28)

positions <- data.frame(x=read.csv(paste(p23, "H-22-605_T_S7.coverage.csv", sep="/")))[,c(1,2,3,4,8)]

plot_coverage <- function(sample_num){
  sample_name <- substr(colnames(df_inter_mean)[sample_num], 1, 
                        str_locate(colnames(df_inter_mean)[sample_num], "\nmean")[1]-2)
  
  df_plot <- data.frame(cbind(positions$x.Region, 
                              df_inter_mean[,sample_num], 
                              df_intra_mean[,sample_num],
                              df_inter_std[,sample_num],
                              df_intra_std[,sample_num]))
  df_plot$X2 <- as.numeric(as.character(df_plot$X2))
  df_plot$X3 <- as.numeric(as.character(df_plot$X3))
  df_plot$X4 <- as.numeric(as.character(df_plot$X4))
  df_plot$X5 <- as.numeric(as.character(df_plot$X5))
  colnames(df_plot) <- c("Region", "inter", "intra")
  
  colors <- c("intra mean" = "green", "inter mean" = "orange")
  ggplot(df_plot, aes(x= 1:nrow(df_plot)))+
    geom_point(aes(y = df_plot[,2], color="inter mean"), size=2, shape=18) +
    geom_smooth(aes(y = df_plot[,2], color="inter mean"), 
                formula = 'y ~ x', method = 'loess',
                se=TRUE, 
                linewidth=.6) +
    geom_errorbar(aes(ymin=df_plot[,2]-df_plot[,4],
                      ymax=df_plot[,2]+df_plot[,4],
                      color="inter mean"), 
                  width=1.2,
                  linewidth=0.4,
                  position=position_dodge(0.9))+ 
    
    geom_point(aes(y = df_plot[,3], color="intra mean"), size=2, shape=16) +
    geom_smooth(aes(y = df_plot[,3], color="intra mean"), 
                formula = 'y ~ x', method = 'loess',
                se=TRUE, 
                linewidth=.6) +
    geom_errorbar(aes(ymin=df_plot[,3]-df_plot[,5],
                      ymax=df_plot[,3]+df_plot[,5],
                      color="intra mean"), 
                  width=1.2,
                  linewidth=0.4,
                  position=position_dodge(0.9))+ 
    
    scale_x_continuous("Region", breaks = seq(1, nrow(df_plot), 3), 
                       labels = as.character(df_plot[seq(1, nrow(df_plot), 3),1])) +
    scale_y_continuous("Coverage", 
                       breaks = seq(0, max(max(df_plot[,3]), max(df_plot[,2]))+250, 500), 
                       labels = as.character(seq(0,max(max(df_plot[,3]), max(df_plot[,2]))+250, 500))) +
    labs(x = "Region",
         y = "Coverage",
         color = "Legend") +
    theme_light() +
    scale_color_manual(values = colors) +
    scale_color_hue(c=100, l=65)+
    theme(axis.text.x = element_text(face="plain", color="black", size=6, angle=-65, vjust = 0.0, hjust=0.0),
          legend.position = c(.95, .95),
          legend.justification = c("right", "top"),
          legend.box.just = "right",
          legend.margin = margin(6, 6, 6, 6),
          axis.text = element_text(size = 10),
          axis.title = element_text(size = 13),
          legend.text = element_text(size = 12),
          legend.title = element_text(size = 15)) +
    ggtitle(paste("Coverage sample:", sample_name))
  ggsave(paste("coverage/Coverage_", sample_name, ".png", sep=""), width = 3600, height=2400, unit="px")
}

for (i in 1:ncol(df_inter_mean)){
  plot_coverage(i)
}

