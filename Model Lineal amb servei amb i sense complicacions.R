setwd("C:/Users/mpgonzalez/Desktop/Costos per complicació")
library(readxl)
library(tidyverse)
library(caret)
library(leaps)
library(car)
#install.packages("dummies")
library(dummies)

taula_tot<- read_excel("C:/Users/mpgonzalez/Desktop/Costos per complicació/Taula amb i sense complicacions.xlsx")
attach(taula_tot)
View(taula_tot)
length(taula_tot)

#fem model lineal
models3<-regsubsets(`Total Cost Sense Estructura`~.-`Servei Alta (Codi)`,data = taula_tot, nvmax = 56,really.big = T)
res.sum3<-summary(models3)
res.sum3

data.frame(
  Adj.R2 = which.max(res.sum3$adjr2),
  CP = which.min(res.sum3$cp),
  BIC = which.min(res.sum3$bic)
)
# id: model id
# object: regsubsets object
# data: data used to fit regsubsets
# outcome: outcome variable
get_model_formula <- function(id, object, outcome){
  # get models data
  models3 <- summary(object)$which[id,-1]
  # Get outcome variable
  #form <- as.formula(object$call[[2]])
  #outcome <- all.vars(form)[1]
  # Get model predictors
  predictors <- names(which(models3 == TRUE))
  predictors <- paste(predictors, collapse = "+")
  # Build model formula
  as.formula(paste0(outcome, "~", predictors))
}

get_cv_error <- function(model.formula, data){
  set.seed(1)
  train.control <- trainControl(method = "cv", number = 53)
  cv <- train(model.formula, data = data, method = "lm",
              trControl = train.control)
  cv$results$RMSE
}
model.ids <- 1:53
cv.errors <-  map(model.ids, get_model_formula, models3, "`Total Cost Sense Estructura`") %>%
  map(get_cv_error, data = taula_tot) %>%
  unlist()
cv.errors
# Select the model that minimize the CV error
which.min(cv.errors)#52 Var
coef(models2, 52)
 #                    (Intercept)                   `Sexe (Desc)` 
 #                   -1257.244840                      -28.420202 
 #    `Edat Pacient a l'Admissió`      `És Quirúrgic (Indicador)` 
 #                      -9.830394                      985.578633 
 #              `Ind Passa X UCI`   `Circumstància Ingrés (Codi)` 
 #                    4696.652711                     1196.128623 
 #    `Circumstància Alta (Codi)` `Estada HCB (en dies naturals)` 
 #                     152.237397                      564.268161 
 #  `\\`Servei Alta (Codi)\\`ANE` `\\`Servei Alta (Codi)\\`B-CTR` 
 #                    -962.267453                     4198.734816 
 #  `\\`Servei Alta (Codi)\\`CAR`   `\\`Servei Alta (Codi)\\`CCV` 
 #                    2187.256446                    -1640.911389 
 #  `\\`Servei Alta (Codi)\\`CGI`   `\\`Servei Alta (Codi)\\`CIP` 
 #                    -754.467931                    -1323.582182 
 #  `\\`Servei Alta (Codi)\\`COT`   `\\`Servei Alta (Codi)\\`CTR` 
 #                    -639.636176                     -746.707393 
 #  `\\`Servei Alta (Codi)\\`DER`   `\\`Servei Alta (Codi)\\`END` 
 #                   -1378.053874                     -752.336108 
 #  `\\`Servei Alta (Codi)\\`ERM`   `\\`Servei Alta (Codi)\\`GAS` 
 #                     632.459116                     -451.439066 
 #  `\\`Servei Alta (Codi)\\`GIN`   `\\`Servei Alta (Codi)\\`HBP` 
 #                    -933.200430                    -1083.465395 
 #  `\\`Servei Alta (Codi)\\`HDM`   `\\`Servei Alta (Codi)\\`HEM` 
 #                   -3915.537114                     2902.525226 
 #  `\\`Servei Alta (Codi)\\`HEP`   `\\`Servei Alta (Codi)\\`INF` 
 #                     119.653576                     -756.233185 
 #  `\\`Servei Alta (Codi)\\`MAS`   `\\`Servei Alta (Codi)\\`MDI` 
 #                    -698.054403                    -1678.100461 
 #  `\\`Servei Alta (Codi)\\`MNC`  `\\`Servei Alta (Codi)\\`N.A.` 
 #                   -1477.467574                    -1974.657469 
 #  `\\`Servei Alta (Codi)\\`NEF`   `\\`Servei Alta (Codi)\\`NMO` 
 #                    1357.781113                    -1239.741086 
 #  `\\`Servei Alta (Codi)\\`NNT`   `\\`Servei Alta (Codi)\\`NRC` 
 #                   -2672.672196                     2005.215275 
 #  `\\`Servei Alta (Codi)\\`NRL`   `\\`Servei Alta (Codi)\\`OBS` 
 #                     400.007984                     -594.573203 
 #  `\\`Servei Alta (Codi)\\`OFT`   `\\`Servei Alta (Codi)\\`ONC` 
 #                   -1091.351074                    -1034.076645 
 #  `\\`Servei Alta (Codi)\\`ORL`   `\\`Servei Alta (Codi)\\`PIJ` 
 #                   -1409.842344                    -6200.616361 
 #  `\\`Servei Alta (Codi)\\`PSI`   `\\`Servei Alta (Codi)\\`RAD` 
 #                   -5595.585174                     -858.734011 
 #  `\\`Servei Alta (Codi)\\`RDT`   `\\`Servei Alta (Codi)\\`RMT` 
 #                    -309.380940                     -981.103484 
 # `\\`Servei Alta (Codi)\\`SINT`  `\\`Servei Alta (Codi)\\`UCIR` 
 #                   -1185.025805                      478.653197 
 # `\\`Servei Alta (Codi)\\`UCOT`   `\\`Servei Alta (Codi)\\`UDT` 
 #                     901.102598                      683.933572 
 # `\\`Servei Alta (Codi)\\`UGIN`  `\\`Servei Alta (Codi)\\`UORL` 
 #                     313.503504                      740.724199 
 # `\\`Servei Alta (Codi)\\`UPSI`   `\\`Servei Alta (Codi)\\`URM` 
 #                     254.717038                      665.041613 
 #  `\\`Servei Alta (Codi)\\`URO` 
 #                    -957.291525 
get_model_formula(52, models3, "`Total Cost Sense Estructura`")
# `Total Cost Sense Estructura` ~ `Presenta Alguna Complicació` + 
#     `Sexe (Desc)` + `Edat Pacient a l'Admissió` + 
#     `És Quirúrgic (Indicador)` + `Ind Passa X UCI` + 
#     `Circumstància Ingrés (Codi)` + `Circumstància Alta (Codi)` + 
#     `Estada HCB (en dies naturals)` + `\`Servei Alta (Codi)\`ANE` + 
#     `\`Servei Alta (Codi)\`B-CTR` + `\`Servei Alta (Codi)\`CAR` + 
#     `\`Servei Alta (Codi)\`CCV` + `\`Servei Alta (Codi)\`CGI` + 
#     `\`Servei Alta (Codi)\`COT` + `\`Servei Alta (Codi)\`CTR` + 
#     `\`Servei Alta (Codi)\`END` + `\`Servei Alta (Codi)\`ERM` + 
#     `\`Servei Alta (Codi)\`GAS` + `\`Servei Alta (Codi)\`GIN` + 
#     `\`Servei Alta (Codi)\`HBP` + `\`Servei Alta (Codi)\`HDM` + 
#     `\`Servei Alta (Codi)\`HEM` + `\`Servei Alta (Codi)\`HEP` + 
#     `\`Servei Alta (Codi)\`INF` + `\`Servei Alta (Codi)\`MAS` + 
#     `\`Servei Alta (Codi)\`MDI` + `\`Servei Alta (Codi)\`MNC` + 
#     `\`Servei Alta (Codi)\`N.A.` + `\`Servei Alta (Codi)\`NEF` + 
#     `\`Servei Alta (Codi)\`NMO` + `\`Servei Alta (Codi)\`NNT` + 
#     `\`Servei Alta (Codi)\`NRC` + `\`Servei Alta (Codi)\`NRL` + 
#     `\`Servei Alta (Codi)\`OBS` + `\`Servei Alta (Codi)\`OFT` + 
#     `\`Servei Alta (Codi)\`ONC` + `\`Servei Alta (Codi)\`ORL` + 
#     `\`Servei Alta (Codi)\`PIJ` + `\`Servei Alta (Codi)\`PSI` + 
#     `\`Servei Alta (Codi)\`RAD` + `\`Servei Alta (Codi)\`RDT` + 
#     `\`Servei Alta (Codi)\`RMT` + `\`Servei Alta (Codi)\`SINT` + 
#     `\`Servei Alta (Codi)\`UCIR` + `\`Servei Alta (Codi)\`UCOT` + 
#     `\`Servei Alta (Codi)\`UDT` + `\`Servei Alta (Codi)\`UGIN` + 
#     `\`Servei Alta (Codi)\`UORL` + `\`Servei Alta (Codi)\`UPSI` + 
#     `\`Servei Alta (Codi)\`URM` + `\`Servei Alta (Codi)\`URO` + 
#     `\`Servei Alta (Codi)\`UTH`





mod52<-lm(`Total Cost Sense Estructura` ~ `Presenta Alguna Complicació` + 
     `Sexe (Desc)` + `Edat Pacient a l'Admissió` + 
     `És Quirúrgic (Indicador)` + `Ind Passa X UCI` + 
     `Circumstància Ingrés (Codi)` + `Circumstància Alta (Codi)` + 
     `Estada HCB (en dies naturals)` + `\`Servei Alta (Codi)\`ANE` + 
     `\`Servei Alta (Codi)\`B-CTR` + `\`Servei Alta (Codi)\`CAR` + 
     `\`Servei Alta (Codi)\`CCV` + `\`Servei Alta (Codi)\`CGI` + 
     `\`Servei Alta (Codi)\`COT` + `\`Servei Alta (Codi)\`CTR` + 
     `\`Servei Alta (Codi)\`END` + `\`Servei Alta (Codi)\`ERM` + 
     `\`Servei Alta (Codi)\`GAS` + `\`Servei Alta (Codi)\`GIN` + 
     `\`Servei Alta (Codi)\`HBP` + `\`Servei Alta (Codi)\`HDM` + 
     `\`Servei Alta (Codi)\`HEM` + `\`Servei Alta (Codi)\`HEP` + 
     `\`Servei Alta (Codi)\`INF` + `\`Servei Alta (Codi)\`MAS` + 
     `\`Servei Alta (Codi)\`MDI` + `\`Servei Alta (Codi)\`MNC` + 
     `\`Servei Alta (Codi)\`N.A.` + `\`Servei Alta (Codi)\`NEF` + 
     `\`Servei Alta (Codi)\`NMO` + `\`Servei Alta (Codi)\`NNT` + 
     `\`Servei Alta (Codi)\`NRC` + `\`Servei Alta (Codi)\`NRL` + 
     `\`Servei Alta (Codi)\`OBS` + `\`Servei Alta (Codi)\`OFT` + 
     `\`Servei Alta (Codi)\`ONC` + `\`Servei Alta (Codi)\`ORL` + 
     `\`Servei Alta (Codi)\`PIJ` + `\`Servei Alta (Codi)\`PSI` + 
     `\`Servei Alta (Codi)\`RAD` + `\`Servei Alta (Codi)\`RDT` + 
     `\`Servei Alta (Codi)\`RMT` + `\`Servei Alta (Codi)\`SINT` + 
     `\`Servei Alta (Codi)\`UCIR` + `\`Servei Alta (Codi)\`UCOT` + 
     `\`Servei Alta (Codi)\`UDT` + `\`Servei Alta (Codi)\`UGIN` + 
     `\`Servei Alta (Codi)\`UORL` + `\`Servei Alta (Codi)\`UPSI` + 
     `\`Servei Alta (Codi)\`URM` + `\`Servei Alta (Codi)\`URO` + 
     `\`Servei Alta (Codi)\`UTH`)
smod52<-summary(mod52)
smod52
# Call:
# lm(formula = `Total Cost Sense Estructura` ~ `Presenta Alguna Complicació` + 
#     `Sexe (Desc)` + `Edat Pacient a l'Admissió` + `És Quirúrgic (Indicador)` + 
#     `Ind Passa X UCI` + `Circumstància Ingrés (Codi)` + `Circumstància Alta (Codi)` + 
#     `Estada HCB (en dies naturals)` + `\`Servei Alta (Codi)\`ANE` + 
#     `\`Servei Alta (Codi)\`B-CTR` + `\`Servei Alta (Codi)\`CAR` + 
#     `\`Servei Alta (Codi)\`CCV` + `\`Servei Alta (Codi)\`CGI` + 
#     `\`Servei Alta (Codi)\`COT` + `\`Servei Alta (Codi)\`CTR` + 
#     `\`Servei Alta (Codi)\`END` + `\`Servei Alta (Codi)\`ERM` + 
#     `\`Servei Alta (Codi)\`GAS` + `\`Servei Alta (Codi)\`GIN` + 
#     `\`Servei Alta (Codi)\`HBP` + `\`Servei Alta (Codi)\`HDM` + 
#     `\`Servei Alta (Codi)\`HEM` + `\`Servei Alta (Codi)\`HEP` + 
#     `\`Servei Alta (Codi)\`INF` + `\`Servei Alta (Codi)\`MAS` + 
#     `\`Servei Alta (Codi)\`MDI` + `\`Servei Alta (Codi)\`MNC` + 
#     `\`Servei Alta (Codi)\`N.A.` + `\`Servei Alta (Codi)\`NEF` + 
#     `\`Servei Alta (Codi)\`NMO` + `\`Servei Alta (Codi)\`NNT` + 
#     `\`Servei Alta (Codi)\`NRC` + `\`Servei Alta (Codi)\`NRL` + 
#     `\`Servei Alta (Codi)\`OBS` + `\`Servei Alta (Codi)\`OFT` + 
#     `\`Servei Alta (Codi)\`ONC` + `\`Servei Alta (Codi)\`ORL` + 
#     `\`Servei Alta (Codi)\`PIJ` + `\`Servei Alta (Codi)\`PSI` + 
#     `\`Servei Alta (Codi)\`RAD` + `\`Servei Alta (Codi)\`RDT` + 
#     `\`Servei Alta (Codi)\`RMT` + `\`Servei Alta (Codi)\`SINT` + 
#     `\`Servei Alta (Codi)\`UCIR` + `\`Servei Alta (Codi)\`UCOT` + 
#     `\`Servei Alta (Codi)\`UDT` + `\`Servei Alta (Codi)\`UGIN` + 
#     `\`Servei Alta (Codi)\`UORL` + `\`Servei Alta (Codi)\`UPSI` + 
#     `\`Servei Alta (Codi)\`URM` + `\`Servei Alta (Codi)\`URO` + 
#     `\`Servei Alta (Codi)\`UTH`)
# 
# Residuals:
#    Min     1Q Median     3Q    Max 
# -69102   -881    -35    712 168158 
# 
# Coefficients:
#                                  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                     -2975.066    162.679 -18.288  < 2e-16 ***
# `Presenta Alguna Complicació`     356.847    104.670   3.409 0.000652 ***
# `Sexe (Desc)`                     -14.910     42.832  -0.348 0.727759    
# `Edat Pacient a l'Admissió`       -10.980      1.241  -8.851  < 2e-16 ***
# `És Quirúrgic (Indicador)`       1232.782     63.149  19.522  < 2e-16 ***
# `Ind Passa X UCI`                4814.679     87.971  54.730  < 2e-16 ***
# `Circumstància Ingrés (Codi)`    1193.173     52.530  22.714  < 2e-16 ***
# `Circumstància Alta (Codi)`       185.266     22.935   8.078 6.75e-16 ***
# `Estada HCB (en dies naturals)`   582.332      2.369 245.764  < 2e-16 ***
# `\\`Servei Alta (Codi)\\`ANE`     550.814    201.300   2.736 0.006216 ** 
# `\\`Servei Alta (Codi)\\`B-CTR`  5896.960   4109.692   1.435 0.151325    
# `\\`Servei Alta (Codi)\\`CAR`    3927.930    144.299  27.221  < 2e-16 ***
# `\\`Servei Alta (Codi)\\`CCV`     -12.110    154.361  -0.078 0.937467    
# `\\`Servei Alta (Codi)\\`CGI`     593.814    146.205   4.062 4.88e-05 ***
# `\\`Servei Alta (Codi)\\`COT`     713.486    131.205   5.438 5.42e-08 ***
# `\\`Servei Alta (Codi)\\`CTR`     745.250    202.434   3.681 0.000232 ***
# `\\`Servei Alta (Codi)\\`END`     904.442    660.128   1.370 0.170662    
# `\\`Servei Alta (Codi)\\`ERM`    2350.364    240.762   9.762  < 2e-16 ***
# `\\`Servei Alta (Codi)\\`GAS`    1172.341    175.933   6.664 2.70e-11 ***
# `\\`Servei Alta (Codi)\\`GIN`     575.737    144.659   3.980 6.90e-05 ***
# `\\`Servei Alta (Codi)\\`HBP`     315.724    150.227   2.102 0.035590 *  
# `\\`Servei Alta (Codi)\\`HDM`   -2624.856    171.998 -15.261  < 2e-16 ***
# `\\`Servei Alta (Codi)\\`HEM`    4397.557    166.724  26.376  < 2e-16 ***
# `\\`Servei Alta (Codi)\\`HEP`    1794.105    190.321   9.427  < 2e-16 ***
# `\\`Servei Alta (Codi)\\`INF`     771.838    192.392   4.012 6.04e-05 ***
# `\\`Servei Alta (Codi)\\`MAS`     904.076    307.532   2.940 0.003286 ** 
# `\\`Servei Alta (Codi)\\`MDI`    -186.925    156.425  -1.195 0.232099    
# `\\`Servei Alta (Codi)\\`MNC`     223.873    785.188   0.285 0.775553    
# `\\`Servei Alta (Codi)\\`N.A.`   -379.876   4110.135  -0.092 0.926361    
# `\\`Servei Alta (Codi)\\`NEF`    3132.773    167.002  18.759  < 2e-16 ***
# `\\`Servei Alta (Codi)\\`NMO`     362.508    173.217   2.093 0.036373 *  
# `\\`Servei Alta (Codi)\\`NNT`   -1232.448    196.313  -6.278 3.46e-10 ***
# `\\`Servei Alta (Codi)\\`NRC`    3442.389    175.542  19.610  < 2e-16 ***
# `\\`Servei Alta (Codi)\\`NRL`    1888.803    168.496  11.210  < 2e-16 ***
# `\\`Servei Alta (Codi)\\`OBS`     949.513    135.037   7.032 2.07e-12 ***
# `\\`Servei Alta (Codi)\\`OFT`     428.226    131.080   3.267 0.001088 ** 
# `\\`Servei Alta (Codi)\\`ONC`     381.020    174.381   2.185 0.028895 *  
# `\\`Servei Alta (Codi)\\`ORL`      19.728    170.830   0.115 0.908061    
# `\\`Servei Alta (Codi)\\`PIJ`   -4842.504    247.475 -19.568  < 2e-16 ***
# `\\`Servei Alta (Codi)\\`PSI`   -4323.524    241.008 -17.939  < 2e-16 ***
# `\\`Servei Alta (Codi)\\`RAD`     869.388    393.137   2.211 0.027012 *  
# `\\`Servei Alta (Codi)\\`RDT`    1202.347   1032.909   1.164 0.244414    
# `\\`Servei Alta (Codi)\\`RMT`     560.645    550.045   1.019 0.308080    
# `\\`Servei Alta (Codi)\\`SINT`    447.359    562.310   0.796 0.426284    
# `\\`Servei Alta (Codi)\\`UCIR`   2212.371    800.226   2.765 0.005700 ** 
# `\\`Servei Alta (Codi)\\`UCOT`   2645.588    310.458   8.522  < 2e-16 ***
# `\\`Servei Alta (Codi)\\`UDT`    2422.968    364.687   6.644 3.09e-11 ***
# `\\`Servei Alta (Codi)\\`UGIN`   2010.203   1557.688   1.291 0.196882    
# `\\`Servei Alta (Codi)\\`UORL`   2454.781    800.117   3.068 0.002156 ** 
# `\\`Servei Alta (Codi)\\`UPSI`   1956.015    332.282   5.887 3.97e-09 ***
# `\\`Servei Alta (Codi)\\`URM`    2395.299    155.388  15.415  < 2e-16 ***
# `\\`Servei Alta (Codi)\\`URO`     549.493    139.315   3.944 8.02e-05 ***
# `\\`Servei Alta (Codi)\\`UTH`    2293.509    182.892  12.540  < 2e-16 ***
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 4108 on 44770 degrees of freedom
# Multiple R-squared:  0.705,	Adjusted R-squared:  0.7047 
# F-statistic:  2057 on 52 and 44770 DF,  p-value: < 2.2e-16