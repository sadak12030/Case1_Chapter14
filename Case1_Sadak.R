rm(list = ls())
setwd("~/Desktop/MSBA DUC/6.5504/L2/Chapter 14")
dir.exists("~/Desktop/MSBA DUC/6.5504/L2/Chapter 14")
my_path<- '/Desktop/MSBA DUC/6.5504/L2/Chapter 14'
library(readxl)
buckeyecreek <- read_excel("BuckeyeCreek.xlsx")
summary(buckeyecreek)