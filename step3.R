set.seed(12345)
# ------------------------## ------------------------## ------------------------#
# ------------------------## ------------------------## ------------------------#

# ----------------- #
#     model 4 - using new naming and new dataframe (imputed)
# ----------------- #
model4 <- "
  # the measurement model
  scie =~ PV1SCIE + PV2SCIE + PV3SCIE + PV4SCIE + PV5SCIE + PV6SCIE + PV7SCIE + PV8SCIE + PV9SCIE + PV10SCIE
  ENV.AWARE =~ GreenhouseGasses1 + GMO1 + Nuclear Waste1 + Forest Clearing1 +Air Pollution1+Plant and Animal Extinction1 + Water Shortage1
  JOY.SCIE =~ FunLearning+LikeReading+HappyWorkingOn+EnjoyKnowlegde+InterestInLearning
  INT.BRSCIE =~ Biosphere+Motion+Energry+Universe+PreventDisease
  
  # new path suggested from mi at cfa (highest MI value)
  AirPollution1     ~~    PlantandAnimalExtinction1
  
  # adding new path
  scie ~ ENV.AWARE +JOY.SCIE+INT.BRSCIE
"
sem.model2 <- sem(model4, data = studData) # lots of warning

fitmeasures(sem.model2,c("cfi","rmsea","srmr")) # error

summary(sem.model2, standardized=TRUE,fit.measures=TRUE)

mi.M4 = modificationindices(sem.model2)
mi.M4 %>% arrange(-(mi)) %>%  
  as_data_frame() %>%
  arrange(-mi) %>%
  select(lhs, op, rhs, mi, epc) %>% head(10) %>% 
  pander(caption="10 Largest MI values for mi.M4")

# ------------------------## ------------------------## ------------------------#
model5 <- "
  # the measurement model
  scie =~ PV1SCIE + PV2SCIE + PV3SCIE + PV4SCIE + PV5SCIE + PV6SCIE + PV7SCIE + PV8SCIE + PV9SCIE + PV10SCIE 
  ENV.AWARE =~ GreenhouseGasses1 + GMO1 + Nuclear Waste1 + Forest Clearing1 +Air Pollution1+Plant and Animal Extinction1 + Water Shortage1
  ENV.OPT =~ GreenhouseGasses2 + GMO2 + Nuclear Waste2 + Forest Clearing2 +Air Pollution2+Plant and Animal Extinction2 + Water Shortage2
  
  # adding new path (is predicted by)
  scie ~ ENV.AWARE + ENV.OPT
  
  # new path suggested from mi (highest MI value)
  GMO1 ~~ NuclearWaste1
"
sem.model3 <- sem(model5, data = studData)

fitmeasures(sem.model3,c("cfi","rmsea","srmr"))
# cfi rmsea  srmr 
# 0.978 0.048 0.056

semPlot::semPaths(sem.model3,what='std', residuals=F)


# ------------------------## ------------------------## ------------------------#
# weighted
des.rep = svrepdesign(ids=~1, weights=~W_FSTUWT,
                      data=studData,
                      repweights="W_FSTURWT[0-9]+",
                      type="Fay", rho=0.5)

lavSurvFit1 = lavaan.survey(sem.model3,des.rep)
# summary(lavSurvFit, standardized=TRUE)
fitmeasures(lavSurvFit1,c("chisq","cfi","rmsea","srmr"))
# cfi rmsea  srmr 
# 0.977 0.049 0.052

mi.M5 = modificationindices(lavSurvFit1)
mi.M5 %>% arrange(-(mi)) %>%  
  as_data_frame() %>%
  arrange(-mi) %>%
  select(lhs, op, rhs, mi, epc) %>% head(10) %>% 
  pander(caption="10 Largest MI values for mi.M5")
# # in order to use modification index, and to modify the model according to the MI,
# # I should use it thoughtfully and cautiously as to why certain observed variables 
# # need to be omit
# # MI can be used in all designs, complex or not
# head(modIndComplex)

# ------------------------## ------------------------## ------------------------#
# complex design with replicate weights
model6 <- "
  # the measurement model
  scie =~ PV1SCIE + PV2SCIE + PV3SCIE + PV4SCIE + PV5SCIE + PV6SCIE + PV7SCIE + PV8SCIE + PV9SCIE + PV10SCIE 
  ENV.AWARE =~ GreenhouseGasses1 + GMO1 + Nuclear Waste1 + Forest Clearing1 +Air Pollution1+Plant and Animal Extinction1 + Water Shortage1
  ENV.OPT =~ GreenhouseGasses2 + GMO2 + Nuclear Waste2 + Forest Clearing2 +Air Pollution2+Plant and Animal Extinction2 + Water Shortage2
  
  # adding new path (is predicted by)
  scie ~ ENV.AWARE + ENV.OPT
  
  # new path suggested from mi (highest MI value)
  GMO1 ~~ NuclearWaste1
  AirPollution1 ~~ PlantandAnimalExtinction1
"
sem.model4 <- sem(model6, data = studData)
lavSurvFit2 = lavaan.survey(sem.model4,des.rep)
fitmeasures(lavSurvFit2,c("cfi","rmsea","srmr"))
# cfi rmsea  srmr 
# 0.979 0.046 0.053


# ------------------------## ------------------------## ------------------------#
# ------------------------## ------------------------## ------------------------#
#  RESULTS AND DISCUSSION
# ------------------------## ------------------------## ------------------------#
# ------------------------## ------------------------## ------------------------#
# it is clear that, model 6 and model 5 doesn't produce a significant GoF even after performing 
# the MI.
# Model 4 (original model) vs Model 5 (adding new path from the MI) showed improvement
# using Model 5 as the base, when considering the replicate weights as suggested by Oberski,
# lavaan.survey() can implement weight, however GoF shows no significant change for 
# all three fit measures

# ------------------------## ------------------------## ------------------------#
# modelx <- "
# SCIE.EFF =~ ST129Q01TA +ST129Q02TA +ST129Q03TA +ST129Q04TA +ST129Q05TA +ST129Q06TA +ST129Q07TA +ST129Q08TA 
# INT.BRSCI =~ ST095Q04NA+ST095Q07NA+ST095Q08NA+ST095Q13NA+ST095Q15NA
# JOY.SCIE =~ ST094Q01NA+ST094Q02NA+ST094Q03NA+ST094Q04NA+ST094Q05NA
# INST.SCIE =~ ST113Q01TA+ST113Q02TA+ST113Q03TA+ST113Q04TA
# EPIS.T =~ ST131Q01NA+ST131Q03NA+ST131Q04NA+ST131Q06NA+ST131Q08NA+ST131Q11NA
# ENV.AWARE =~ ST092Q01TA+ST092Q02TA+ST092Q04TA+ST092Q05TA+ST092Q06NA+ST092Q08NA+ST092Q09NA
# ENV.OPT =~ ST093Q01TA+ST093Q03TA +ST093Q04TA+ST093Q05TA+ST093Q06TA+ST093Q07NA+ST093Q08NA
# ANX.TEST =~ ST118Q01NA+ST118Q02NA+ST118Q03NA+ST118Q04NA+ST118Q05NA
# MOTIVA.T =~ ST119Q01NA+ST119Q02NA+ST119Q03NA+ST119Q04NA+ST119Q05NA
# CO.OPERATE =~ ST082Q02NA+ST082Q03NA+ST082Q08NA+ST082Q12NA
# CPS.VALUE =~ ST082Q01NA+ST082Q09NA+ST082Q13NA+ST082Q14NA
# 
# +INT.BRSCI+JOY.SCIE+INST.SCIE+EPIS.T+ENV.AWARE+ENV.OPT+ANX.TEST+MOTIVA.T+CO.OPERATE+CPS.VALUE
# BEL.ONG =~ ST034Q01TA +ST034Q02TA+ST034Q03TA+ST034Q04TA+ST034Q05TA+ST034Q06TA
# scie =~ PV1SCIE + PV2SCIE + PV3SCIE + PV4SCIE + PV5SCIE + PV6SCIE + PV7SCIE + PV8SCIE + PV9SCIE + PV10SCIE 
# 
# # Activities before/ after school
# act.before =~ ST076Q01NA+ST076Q02NA+ST076Q03NA+ST076Q04NA+ST076Q05NA+ST076Q06NA+ST076Q07NA+ST076Q08NA+ST076Q09NA+ST076Q10NA+ST076Q11NA
# act.after =~ ST078Q01NA+ST078Q02NA+ST078Q03NA+ST078Q04NA+ST078Q05NA+ST078Q06NA+ST078Q07NA+ST078Q08NA+ST078Q09NA+ST078Q10NA+ST078Q11NA
# exercise.b4 =~ST076Q11NA
# exercise.after =~ ST078Q11NA
# "

# ----------------- #
#     model 1
# ----------------- #

model1 <- "
  # latent variable def
  scie =~ PV1SCIE + PV2SCIE + PV3SCIE + PV4SCIE + PV5SCIE + PV6SCIE + PV7SCIE + PV8SCIE + PV9SCIE + PV10SCIE 
  ENV.AWARE =~ GreenhouseGasses1 + GMO1 + Nuclear Waste1 + Forest Clearing1 +Air Pollution1+Plant and Animal Extinction1 + Water Shortage1
  JOY.SCIE =~ FunLearning+LikeReading+HappyWorkingOn+EnjoyKnowlegde+InterestInLearning
  INT.BRSCIE =~ Biosphere+Motion+Energry+Universe+PreventDisease
"

cfa.M1 = cfa(model1,studData)
# summary(cfa.M1,fit.measures=TRUE)
# 
# 
# cfi = comparative fit index; should > 0.9
# rmsea = root mean square error approximation; should < 0.08
# srmr = standardize root mean square residual; should < 0.08
fitmeasures(cfa.M1,c("cfi","rmsea","srmr"))
# cfi rmsea  srmr
# 0.971 0.050 0.053
# 
# semPlot::semPaths(cfa.M1)
# 
# 
mi.M1 = modificationindices(cfa.M1)
mi.M1 %>% arrange(-(mi)) %>%  
  as_data_frame() %>%
  arrange(-mi) %>%
  select(lhs, op, rhs, mi, epc) %>% head(10)
# # ----------------- #
# #     model 1.1 modified
# # ----------------- #
model1.1 <- "
  # latent variable def
  scie =~ PV1SCIE + PV2SCIE + PV3SCIE + PV4SCIE + PV5SCIE + PV6SCIE + PV7SCIE + PV8SCIE + PV9SCIE + PV10SCIE
  ENV.AWARE =~ GreenhouseGasses1 + GMO1 + Nuclear Waste1 + Forest Clearing1 +Air Pollution1+Plant and Animal Extinction1 + Water Shortage1
  JOY.SCIE =~ FunLearning+LikeReading+HappyWorkingOn+EnjoyKnowlegde+InterestInLearning
  INT.BRSCIE =~ Biosphere+Motion+Energry+Universe+PreventDisease

  AirPollution1     ~~    PlantandAnimalExtinction1
"
# 
cfa.M1.1 = cfa(model1.1,studData)
# summary(cfa.M1.1,fit.measures=TRUE)
# 
# 
# # cfi = comparative fit index; should > 0.9
# # rmsea = root mean square error approximation; should < 0.08
# # srmr = standardize root mean square residual; should < 0.08
fitmeasures(cfa.M1.1,c("cfi","rmsea","srmr"))
# cfi rmsea  srmr
# 0.977 0.045 0.051
# 
# # ------------------------## ------------------------## ------------------------#
# # ----------------- #
# #     model 2
# # ----------------- #
# 
# model2 <- "
#   # latent variable def
#   scie =~ PV1SCIE + PV2SCIE + PV3SCIE + PV4SCIE + PV5SCIE + PV6SCIE + PV7SCIE + PV8SCIE + PV9SCIE + PV10SCIE
#   ENV.AWARE =~ ST092Q01TA+ST092Q02TA+ST092Q04TA+ST092Q05TA+ST092Q06NA+ST092Q08NA+ST092Q09NA
#   ENV.OPT =~ ST093Q01TA+ST093Q03TA +ST093Q04TA+ST093Q05TA+ST093Q06TA+ST093Q07NA+ST093Q08NA
# "
# 
# 
# cfa.M2 <- cfa(model2, data = studData)
# cfa.M2
# summary(cfa.M2, standardized=TRUE,fit.measures=TRUE)
# fitmeasures(cfa.M2,c("cfi","rmsea","srmr"))
# # cfi rmsea  srmr 
# # 0.971 0.055 0.059 
# 
# mi.M2 = modificationindices(cfa.M2)
# mi.M2 %>% arrange(-(mi))
# mi.M2[mi.M2$op == "~~",] %>% arrange(-(mi))
# 
# mi.M2 %>%
#   as_data_frame() %>%
#   arrange(-mi) %>%
#   select(lhs, op, rhs, mi, epc) %>% head(10) %>% 
#   pander(caption="10 Largest MI values for mi.M2")
# 
# model2.1 <- "
#   # latent variable def
#   scie =~ PV1SCIE + PV2SCIE + PV3SCIE + PV4SCIE + PV5SCIE + PV6SCIE + PV7SCIE + PV8SCIE + PV9SCIE + PV10SCIE
#   ENV.AWARE =~ ST092Q01TA+ST092Q02TA+ST092Q04TA+ST092Q05TA+ST092Q06NA+ST092Q08NA+ST092Q09NA
#   ENV.OPT =~ ST093Q01TA+ST093Q03TA +ST093Q04TA+ST093Q05TA+ST093Q06TA+ST093Q07NA+ST093Q08NA
#   
#   # new path suggested from mi
#   ST092Q02TA ~~ ST092Q04TA
#   ST092Q06NA ~~ ST092Q08NA
#   
#   # ST092Q01TA ~~ ST092Q02TA
#   # ST092Q04TA ~~ ST092Q05TA
#   # ST092Q02TA ~~ ST092Q06NA
#   # ST092Q04TA ~~ ST092Q06NA
#   # ST092Q01TA ~~ ST092Q04TA
#   # ST092Q04TA ~~ ST092Q08NA
#   # ST092Q01TA ~~ ST092Q05TA
#   # ST092Q02TA ~~ ST092Q08NA
#   # ST093Q06TA ~~ ST093Q08NA
#   # ST093Q05TA ~~ ST093Q06TA
#   # ST092Q01TA ~~ ST092Q08NA  
#   # ST092Q05TA ~~ ST092Q08NA  
#   # ST092Q02TA ~~ ST092Q05TA  
#   # ST093Q01TA ~~ ST093Q03TA  
#   # ST093Q03TA ~~ ST093Q04TA  
#   # ST093Q01TA ~~ ST093Q06TA  
#   # ST093Q01TA ~~ ST093Q07NA  
#   # ST093Q01TA ~~ ST093Q08NA  
#   # ST093Q03TA ~~ ST093Q07NA
#   # ST093Q03TA ~~ ST093Q07NA
# "
# 
# cfa.M2.1 <- cfa(model2.1, data = studData)
# # cfa.M2.1
# summary(cfa.M2, standardized=TRUE,fit.measures=TRUE)
# fitmeasures(cfa.M2.1,c("cfi","rmsea","srmr"))
# # 0.980 0.045 0.057
# semPlot::semPaths(cfa.M2.1)
# 
# # ------------------------## ------------------------## ------------------------#
# # ----------------- #
# #     model 3
# # ----------------- #
# model3 <- "
#   # the measurement model
#   scie =~ PV1SCIE + PV2SCIE + PV3SCIE + PV4SCIE + PV5SCIE + PV6SCIE + PV7SCIE + PV8SCIE + PV9SCIE + PV10SCIE 
#   ENV.AWARE =~ ST092Q01TA+ST092Q02TA+ST092Q04TA+ST092Q05TA+ST092Q06NA+ST092Q08NA+ST092Q09NA
#   ENV.OPT =~ ST093Q01TA+ST093Q03TA +ST093Q04TA+ST093Q05TA+ST093Q06TA+ST093Q07NA+ST093Q08NA
#   
#   # adding new path
#   scie ~ ENV.AWARE + ENV.OPT
# "
# 
# 
# sem.model1 <- sem(model3, data = studData)
# 
# fitmeasures(sem.model1,c("cfi","rmsea","srmr"))
# # cfi rmsea  srmr 
# # 0.970 0.051 0.056
# 
# summary(sem.model1, standardized=TRUE,fit.measures=TRUE)
# 
# mi.M3 = modificationindices(sem.model1)
# mi.M3 %>% arrange(-(mi)) %>%  
#   as_data_frame() %>%
#   arrange(-mi) %>%
#   select(lhs, op, rhs, mi, epc) %>% head(10) %>% 
#   pander(caption="10 Largest MI values for mi.M3")
#
# # ------------------------## ------------------------## ------------------------#
# # ------------------------## ------------------------## ------------------------#
# 
# model3.1 <- "
#   # the measurement model
#   scie =~ PV1SCIE + PV2SCIE + PV3SCIE + PV4SCIE + PV5SCIE + PV6SCIE + PV7SCIE + PV8SCIE + PV9SCIE + PV10SCIE 
#   ENV.AWARE =~ ST092Q01TA+ST092Q02TA+ST092Q04TA+ST092Q05TA+ST092Q06NA+ST092Q08NA+ST092Q09NA
#   ENV.OPT =~ ST093Q01TA+ST093Q03TA +ST093Q04TA+ST093Q05TA+ST093Q06TA+ST093Q07NA+ST093Q08NA
#   
#   # new path suggested from mi
#   ST092Q02TA ~~ ST092Q04TA
#   ST092Q06NA ~~ ST092Q08NA
#   
#   # adding new path
#   scie ~ ENV.AWARE + ENV.OPT
# "
# 
# 
# sem.model1.1 <- sem(model3.1, data = studData)
# 
# fitmeasures(sem.model1.1,c("cfi","rmsea","srmr"))
# # cfi rmsea  srmr 
# # 0.980 0.045 0.057
# 
# semPlot::semPaths(sem.model1.1,what='std', residuals=F)
# 

# ------------------------## ------------------------## ------------------------#

# 