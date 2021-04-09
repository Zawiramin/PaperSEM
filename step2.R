
#' WHAT ARE THE STEP 2
#' Basically step 2 SUPPOSEDLY are focusing on the data management section
#' 1st we extract only Malaysia from the original database
#' 2nd i rename the school stratum into much simpler one
#' 3rd i subset only the variables needed for the statistic and analysis
#' 4th i performed IMPUTATION using MICE
#' 5th i rename the variable used into readable name

#' #--------------------------------------------------------------------------------------------------#
#'                          LINK AND NOTE USING DATA.TABLE
#' https://okanbulut.github.io/bigdata/wrangling-big-data.html#readingwriting-data-with-data.table
#' #--------------------------------------------------------------------------------------------------#
#------------------------------------------#
# NEW DATA
# using read.spss data at step1, call the data with instruction, label name = false
# thus no need to recode from categorical to numerical
# in other words recodeData.R script might be using to recode reverse-coded variables only, if needed.


#' Check for missing values and its pattern
md.pattern()
names()
summary()

#------------------------------------------#
# NEW:
# recode will only be performed to the desired columns once I've determined which variables to 
# be used for the analysis to save computational time that IMPUTATION algo needed
# so that I'm not wasting 6-10 hours of working worth just to wait for the computation done 
# if doing whole dataset IMPUTATIONS!
#------------------------------------------#
#' Set the seed before splitting the data
set.seed(12345)
library("Rcpp")
library("mice") #' handling missing values, md.pattern()

#--------------------------------#
# IMPUTATION
#' Performed Imputation using MICE
#' extract dataframe for ENVAWARE (ST092), ENVOPT (ST093), and JOYSCIE (ST094)
data.frame(colnames(studData))
# studData <- studData[,-c(923)]
# data.frame(colnames(studData))
# EnvData <- studData[,c(176:189)]
# EnvData.ind <- mice(EnvData,m=5,maxit = 50,method = 'pmm', seed = 1000)
# EnvData <- complete(EnvData.ind) #commenting this as to note that i don't need to run this code again
# fwrite(EnvData,file = "EnvData.csv")
# EnvData <- fread("EnvData.csv")
# summary(envData)

# JoyScie <- studData[,c(190:194)]
# JoyScie.ind <- mice(JoyScie,m=5,maxit = 50,method = 'pmm', seed = 1000)
# JoyScie <- complete(JoyScie.ind) #commenting this as to note that i don't need to run this code again
# fwrite(JoyScie,file = "JoyScie.csv")
# JoyScie <- fread("JoyScie.csv")
# summary(joyfs)

#' convert data.frame to data.table 
#' read here for more details on the disadvantages of data.frame (R base)
#' https://www.quora.com/What-is-the-difference-between-data-frame-and-data-table-in-R-programming-language
# scieData.imp <- data.table(scieData.imp)

#------------------------------------------#
# IMPUTATION DONE
#------------#
#'-------------------------------##'-------------------------------#
#' 5th i rename the variable used into readable name

EnvData<- fread("envData.csv")
summary(EnvData)
# later rename the indiv items into more readable/understandable name for printing on plots
EnvData[,`:=`
        ( #How informed are you about the following environmental issues?
          GreeHseGas1 = ST092Q01TA,
          GeneModOrg1 = ST092Q02TA,
          NucleWaste1 = ST092Q04TA,
          ForestClea1 = ST092Q05TA,
          AirPollute1 = ST092Q06NA,
          PlntAnmlEx1 = ST092Q08NA,
          WaterShort1 = ST092Q09NA,
          
          #Do you think problems associated with the environmental 
          #issues below will improve or get worse over the next 20 years?
          AirPollute2 = ST093Q01TA,
          PlntAnmlEx2 = ST093Q03TA,
          ForestClea2 = ST093Q04TA,
          WaterShort2 = ST093Q05TA,
          NucleWaste2 = ST093Q06TA,
          GreeHseGas2 = ST093Q07NA,
          GeneModOrg2 = ST093Q08NA
        )]
summary(EnvData)
data.frame(colnames(EnvData))

JoyScie <- fread("JoyScie.csv")
JoyScie[,`:=`
        (#How much do you disagree or agree with the statements about yourself below?
          FunLearning = ST094Q01NA,
          LikeReading = ST094Q02NA,
          HappyWorkOn = ST094Q03NA,
          EnjoyStudy  = ST094Q04NA,
          IntrInLearn = ST094Q05NA
        )]
summary(JoyScie)
data.frame(colnames(JoyScie))

# merge studData with EnvData
studData <- cbind(studData,EnvData[,c(15:28)],JoyScie[,c(6:10)])
data.frame(colnames(studData))
fwrite(studData,"studData.csv")

test