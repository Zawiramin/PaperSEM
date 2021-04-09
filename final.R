# ---- NOTES ----
# THIS IS THE FINAL CODE USE AS A CODE CHUNCK THAT IS BEING USED IN THE RMARKDOWN FILE

# ---- data ----
# fwrite(studData,"studData.csv") 
studData <- fread("studData.csv")

# ---- female ----
female = length(studData$ST004D01T[studData$ST004D01T == "1"])

# ---- male ----
male = length(studData$ST004D01T[studData$ST004D01T == "2"])

# ---- table1 ----
# using kable
table1 <- data.table(studData$SCH.TYPE,
                     studData$ST004D01T)

table1 %>% table() %>%
  kbl(caption = "Summary of the type of schools and gender of Malaysian student participated in PISA 2015", 
      col.names = c("Female", "Male"), align = "c", booktabs = T) %>% 
  kable_styling(latex_options = "hold_position") %>% column_spec(1,width = "20em")


# ---- table2 ----



# ---- des.rep ----
des.rep = svrepdesign(ids=~1, weights=~W_FSTUWT,
                      data=studData,
                      repweights="W_FSTR[0-9]+",
                      type="Fay", rho=0.5)
# ------------------------## ------------------------## ------------------------#

# ---- model 1: CFA model ----
cfa.m1 <- "
  # the measurement model
  scie =~ PV1SCIE + PV2SCIE + PV3SCIE + PV4SCIE + PV5SCIE + PV6SCIE + PV7SCIE + PV8SCIE + PV9SCIE + PV10SCIE 
  ENV.AWARE =~ GreeHseGas1 + GeneModOrg1 + NucleWaste1 + ForestClea1 + AirPollute1 + PlntAnmlEx1 + WaterShort1
  ENV.OPT =~ GreeHseGas2 + GeneModOrg2 + NucleWaste2 + ForestClea2 + AirPollute2 + PlntAnmlEx2 + WaterShort2
  JOY.SCIE =~ FunLearning + LikeReading + HappyWorkOn + EnjoyStudy + IntrInLearn

  # adding new path
  #scie ~  ENV.AWARE+ENV.OPT+JOY.SCIE
"
cfa.model1 <- cfa(cfa.m1, data = studData,
                  estimator="MLM",meanstructure=TRUE,
                  int.ov.free=TRUE,auto.var=TRUE,std.lv=TRUE)
fitmeasures(cfa.model1,c("cfi","rmsea","srmr"))
# without meanstructure
# cfi rmsea  srmr 
# 0.971 0.049 0.053 
# with meanstructure
# cfi rmsea  srmr 
# 0.971 0.049 0.052 

# lavaan.survey 
# using lavaan.survey()
lavsrvy.cfaM1 <- lavaan.survey(cfa.model1,des.rep)
fitmeasures(lavsrvy.cfaM1,c("chisq","cfi","rmsea","srmr"))
# chisq      cfi    rmsea     srmr 
# 8117.890    0.970    0.049    0.050
summary(lavsrvy.cfaM1)

# ---- model1 ----
model1 <- "
  # the measurement model
  scie =~ PV1SCIE + PV2SCIE + PV3SCIE + PV4SCIE + PV5SCIE + PV6SCIE + PV7SCIE + PV8SCIE + PV9SCIE + PV10SCIE 
  ENV.AWARE =~ GreeHseGas1 + GeneModOrg1 + NucleWaste1 + ForestClea1 + AirPollute1 + PlntAnmlEx1 + WaterShort1
  ENV.OPT =~ GreeHseGas2 + GeneModOrg2 + NucleWaste2 + ForestClea2 + AirPollute2 + PlntAnmlEx2 + WaterShort2
  
  # adding new path (is predicted by)
  scie ~ ENV.AWARE + ENV.OPT
"
# conventional analsis using SEM
# sem.model1 <- sem(model1, data = studData,estimator="MLM",meanstructure=TRUE,
#                   int.ov.free=TRUE,auto.var=TRUE,std.lv=TRUE)
# summary(sem.model1)
# fitmeasures(sem.model1,c("cfi","rmsea","srmr"))
# cfi rmsea  srmr 
# 0.971 0.055 0.056
# summary(sem.model1, standardized=TRUE,fit.measures=TRUE)
# sem.model1
# using lavaan.survey() for complex sampling analysis using the survey design 
# lavsrvy.semM1 <- lavaan.survey(lavaan.fit =sem.model1,survey.design = des.rep)
# fitmeasures(lavsrvy.semM1,c("chisq","cfi","rmsea","srmr"))
# chisq      cfi    rmsea     srmr
# 6878.987  0.970   0.055     0.055 

#------------------------#
# conventional analsis using lavaan() with specs estimator = MLM, 
# it uses the option of calculating nonnormality-robust std errors and chisq, that'd yield 
# a "scaling correction" (average generalized "design" effect of nonnormality) of 1.1.
# This serves to make the conventional analysis more comparable to the complex sampling analysis, 
# which can be expected to increase the scaling correction relative to the value after taking nonnormality into account. 
# (lavaan.survey = Daniel Oberski)
# ---- lav.model1 ----
lav.model1 <- lavaan(model1, data = studData,estimator="MLM",meanstructure=TRUE,
                     int.ov.free=TRUE,auto.var=TRUE,std.lv=TRUE)
# fitmeasures(lav.model1,c("chisq","cfi","tli","rmsea","srmr"))
# cfi rmsea  srmr 
# 0.971 0.055 0.062
# ----- lavsrvy.lavM1 ----
# try to fit the model by taking into acc the complex design
# lavaan.survey 
# using lavaan.survey() for complex sampling analysis using the survey design 
lavsrvy.lavM1 <- lavaan.survey(lavaan.fit =lav.model1,survey.design = des.rep)
# fitmeasures(lavsrvy.lavM1,c("chisq","tli","cfi","rmsea","srmr"))
# chisq      cfi    rmsea     srmr 
# 6985.826    0.970    0.055    0.060

# ---- chisq.M1 ----
chisq.M1 <-fitmeasures(lavsrvy.lavM1,"chisq")

# ----- cfi.M1 -----
cfi.M1 <- fitmeasures(lavsrvy.lavM1,"cfi")
# ----- rmsea.M1 -----
rmsea.M1 <- fitmeasures(lavsrvy.lavM1,"rmsea")
# ----- srmr.M1 -----
srmr.M1 <-fitmeasures(lavsrvy.lavM1,c("srmr"))

# ---- table.MI.lavaan conventionalM1 ----
# mi.lavM1 = modificationindices(lav.model1)
# mi.lavM1 %>% arrange(-(mi)) %>%  
#   as_data_frame() %>%
#   arrange(-mi) %>%
#   select(lhs, op, rhs, mi, epc) %>% head(10) %>% 
#   pander(caption="10 Largest MI values for mi.lavM1")

# ---- table.MI.lavaan.surv.M1 ----
mi.lavsrvyM1 = modificationIndices(lavsrvy.lavM1)
mi.lavsrvyM1[mi.lavsrvyM1$op=="~",]
mi.lavsrvyM1 %>% arrange(-(mi.scaled)) %>%  
  as_data_frame() %>%
  arrange(-mi) %>%
  select(lhs, op, rhs, mi, epc) %>% head(10) %>%
  pander(caption="10 Largest MI values for mi.lavsrvyM1")
# the list printed out to be the same for both mi.lavM1 and mi.lavsrvyM1
# but only the last variable is not. However the mi and epc values is not.


# ---- final.Plot1 ---- #
# semPlot::semPaths(sem.model1,what='std', residuals=F)


# ---- model2 ----

model2 <- "
  # the measurement model
  scie =~ PV1SCIE + PV2SCIE + PV3SCIE + PV4SCIE + PV5SCIE + PV6SCIE + PV7SCIE + PV8SCIE + PV9SCIE + PV10SCIE 
  ENV.AWARE =~ GreeHseGas1 + GeneModOrg1 + NucleWaste1 + ForestClea1 + AirPollute1 + PlntAnmlEx1 + WaterShort1
  ENV.OPT =~ GreeHseGas2 + GeneModOrg2 + NucleWaste2 + ForestClea2 + AirPollute2 + PlntAnmlEx2 + WaterShort2
  JOY.SCIE =~ FunLearning + LikeReading + HappyWorkOn + EnjoyStudy + IntrInLearn
  
  # adding new path
  scie ~  ENV.AWARE+ENV.OPT+JOY.SCIE
  
"
# ---- lav.model2 ----
lav.model2 <- lavaan(model2, data = studData,estimator="MLM",meanstructure=TRUE,
                       int.ov.free=TRUE,auto.var=TRUE,std.lv=TRUE)
# cfi = comparative fit index; should > 0.9
# rmsea = root mean square error approximation; should < 0.08
# srmr = standardize root mean square residual; should < 0.08
# fitmeasures(lav.model2,c("chisq","tli","cfi","rmsea","srmr"))
# chisq        tli      cfi    rmsea     srmr 
# 9647.971    0.962    0.965    0.053    0.107 

# ---- lavsrvy.lavM2 ----
# lavaan.survey 
# using lavaan.survey() for complex sampling analysis using the survey design 
lavsrvy.lavM2 <- lavaan.survey(lavaan.fit =lav.model2,survey.design = des.rep)
# fitmeasures(lavsrvy.lavM2,c("chisq","tli","cfi","rmsea","srmr"))
# chisq      tli      cfi    rmsea     srmr 
# 9473.750    0.962    0.965    0.052    0.099 
#----------------------#

# ---- table.MI.sem.M2 ----
mi.lavsrvyM2 = modificationindices(lavsrvy.lavM2)
mi.lavsrvyM2 %>% arrange(-(mi)) %>%  
  as_data_frame() %>%
  arrange(-mi) %>%
  select(lhs, op, rhs, mi, epc) %>% head(10) %>% 
  pander(caption="10 Largest MI values for mi.lavsrvyM2")

# ---- chisq.M2 ----
chisq.M2 <-fitmeasures(lavsrvy.lavM2,"chisq")
# ----- cfi.M2 -----
cfi.M2 <- fitmeasures(lavsrvy.lavM2,"cfi")
# ----- rmsea.M2 -----
rmsea.M2 <- fitmeasures(lavsrvy.lavM2,"rmsea")
# ----- srmr.M2 -----
srmr.M2 <-fitmeasures(lavsrvy.lavM2,c("srmr"))
# ---- table.MI.lavaan.surv.M2 ----
mi.lavsrvyM2 = modificationIndices(lavsrvy.lavM2)
mi.lavsrvyM2[mi.lavsrvyM2$op=="=~",] %>% arrange(mi)
mi.lavsrvyM2 %>%
  as_data_frame() %>%
  arrange(mi) %>%
  select(lhs, op, rhs, mi) %>% head(100) %>%
  pander(caption="10 Largest MI values for mi.lavsrvyM1")
# the list printed out to be the same for both mi.lavM1 and mi.lavsrvyM1
# but only the last variable is not. However the mi and epc values is not.

# ---- model3 ----
model3 <- "
  # the measurement model
  scie =~ PV1SCIE + PV2SCIE + PV3SCIE + PV4SCIE + PV5SCIE + PV6SCIE + PV7SCIE + PV8SCIE + PV9SCIE + PV10SCIE 
  ENV.AWARE =~ GreeHseGas1 + GeneModOrg1 + NucleWaste1 + ForestClea1 + AirPollute1 + PlntAnmlEx1 + WaterShort1
  ENV.OPT =~ GreeHseGas2 + GeneModOrg2 + NucleWaste2 + ForestClea2 + AirPollute2 + PlntAnmlEx2 + WaterShort2
  JOY.SCIE =~ FunLearning + LikeReading + HappyWorkOn + EnjoyStudy + IntrInLearn

  # adding new path
  scie ~  ENV.AWARE+ENV.OPT+JOY.SCIE
  
  # adding new path from MI
  ENV.AWARE ~ JOY.SCIE
  ENV.OPT ~ JOY.SCIE
  ENV.OPT ~ ENV.AWARE
"

# ---- lav.model3 ----
# conventional analsis using lavaan() with specs estimator = MLM,
lav.model3 <- lavaan(model3, data = studData,estimator="MLM",meanstructure=TRUE,
                     int.ov.free=TRUE,auto.var=TRUE,std.lv=TRUE)
# fitmeasures(lav.model3,c("chisq","tli","cfi","rmsea","srmr"))
# chisq       tli      cfi    rmsea     srmr 
# 8108.778    0.968    0.971    0.049    0.052 

# ---- lavsrvy.lavM3 ----
# try to fit the model by taking into acc the complex design
# lavaan.survey 
# using lavaan.survey() for complex sampling analysis using the survey design 
lavsrvy.lavM3 = lavaan.survey(lavaan.fit =lav.model3,survey.design = des.rep)
# fitmeasures(lavsrvy.lavM3,c("chisq","tli","cfi","rmsea","srmr"))
# chisq       tli      cfi    rmsea     srmr 
# 8117.890    0.967    0.970    0.049    0.050
# summary(lavsrvy.lavM3,standardized=TRUE,fit.measures=TRUE)

# ---- chisq.M3 ----
chisq.M3 <-fitmeasures(lavsrvy.lavM3,"chisq")
# ----- cfi.M3 -----
cfi.M3 <- fitmeasures(lavsrvy.lavM3,"cfi")
# ----- rmsea.M3 -----
rmsea.M3 <- fitmeasures(lavsrvy.lavM3,"rmsea")
# ----- srmr.M3 -----
srmr.M3 <-fitmeasures(lavsrvy.lavM3,c("srmr"))

# ---- semPlot1 -----

semPlot::semPaths(lavsrvy.lavM3,
                  sizeMan =8,sizeMan2 =3,
                  sizeLat2 = 5,intercepts = FALSE,shapeLat = "circle",
                  layout = "tree3",rotation = 4,curve = -1.5,
                  as.expression = c("nodes"),layoutSplit = TRUE)


# ---- discussion ----
# ------------------------## ------------------------## ------------------------#
# ------------------------## ------------------------## ------------------------#
#  RESULTS AND DISCUSSION
# ------------------------## ------------------------## ------------------------#
# ------------------------## ------------------------## ------------------------#
# model 3 did not converge AKA error

# Model 1 (original model) vs Model 2 (adding new path from the MI) showed improvement
# using Model 2 as the base when considering the replicate weights as suggested by Oberski,
# lavaan.survey() can implement weight, however GoF shows an insignificant value when adding weight in the analysis
# however, model 1 and model 2 showed that lat var env.A and Joy have high covariance

# ----

# ---- model4 ----
model4 <- "
  # the measurement model
  scie =~ PV1SCIE + PV2SCIE + PV3SCIE + PV4SCIE + PV5SCIE + PV6SCIE + PV7SCIE + PV8SCIE + PV9SCIE + PV10SCIE 
  ENV.AWARE =~ GreenhouseGasses1 + GMO1 + Nuclear Waste1 + Forest Clearing1 +Air Pollution1+Plant and Animal Extinction1 + Water Shortage1
  ENV.OPT =~ GreenhouseGasses2 + GMO2 + Nuclear Waste2 + Forest Clearing2 +Air Pollution2+Plant and Animal Extinction2 + Water Shortage2
  JOY.SCIE =~ FunLearning+LikeReading+HappyWorkingOn+EnjoyKnowlegde+InterestInLearning

  # adding new path
  scie ~  ENV.AWARE+ENV.OPT+JOY.SCIE
  
  # adding new path from MI
  ENV.AWARE ~ JOY.SCIE
  ENV.OPT ~ JOY.SCIE
  ENV.OPT ~ ENV.AWARE
"

# ---- lav.model4 ----
# conventional analsis using lavaan() with specs estimator = MLM,
lav.model4 <- lavaan(model4, data = studData,estimator="MLM",meanstructure=TRUE,
                     int.ov.free=TRUE,auto.var=TRUE,std.lv=TRUE)
fitmeasures(lav.model4,c("chisq","tli","cfi","rmsea","srmr"))
# chisq       tli      cfi    rmsea     srmr 
# 8108.778    0.968    0.971    0.049    0.052   

# ---- lavsrvy.lavM4 ----
# try to fit the model by taking into acc the complex design
# lavaan.survey 
# using lavaan.survey() for complex sampling analysis using the survey design 
lavsrvy.lavM4 = lavaan.survey(lavaan.fit =lav.model4,survey.design = des.rep)
fitmeasures(lavsrvy.lavM4,c("chisq","tli","cfi","rmsea","srmr"))
# chisq       tli      cfi    rmsea     srmr 
# 8117.890    0.967    0.970    0.049    0.050 
# summary(lavsrvy.lavM3)

# ---- chisq.M4 ----
chisq.M3 <-fitmeasures(lavsrvy.lavM3,"chisq")
# ----- cfi.M4 -----
cfi.M3 <- fitmeasures(lavsrvy.lavM3,"cfi")
# ----- rmsea.M4 -----
rmsea.M3 <- fitmeasures(lavsrvy.lavM3,"rmsea")
# ----- srmr.M4 -----
srmr.M3 <-fitmeasures(lavsrvy.lavM3,c("srmr"))

