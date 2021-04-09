# --- load-packages ----
#-----------------------------------------------#
#'             Required 
#' #--------------------------------------------#
#' 
library("foreign") #penting 1 - read spss/sav
library("lavaan") #penting
library("lavaan.survey")

# FASTER THAN tidyverse and RECOMMENDED
# fread(), fwrite()
library("data.table") 
library("dplyr") # %>% 
library("pander") # adding nice title for table
library(plyr) # arrange() function

library("Rcpp")
library("mice")

# get the data
# overall data
# pisaQ = fread("pisaQQQ.csv",na.strings = "")
# pisaQ = pisaQ[CNTRYID == "Malaysia"]
# # science related data
# # ScRltd = fread("ScRltd.csv")
# # str(ScRltd)
# table(pisaQ$ST129Q01TA)
# 
studData = read.spss("psppStud.sav", to.data.frame = TRUE,use.value.labels = FALSE,string)
fwrite(studData, file = "studData.csv")
studData <- fread("studData.csv", na.strings = "")
studData <- studData[CNTRYID == 458]
summary(studData$ST129Q01TA)

