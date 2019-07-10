setwd("C:/Users/mpgonzalez/Desktop/Costos per complicació")
library(readxl)
library(tidyverse)
library(caret)
library(leaps)
library(car)
#install.packages("dummies")
library(dummies)


taula_s_compl<- read_excel("C:/Users/mpgonzalez/Desktop/Costos per complicació/Taula Sense Complicacions.xlsx")
View(taula_s_compl)
attach(taula_s_compl)


length(taula_s_compl)
#fem model lineal
models2<-regsubsets(`Total Cost Sense Estructura`~.-`Servei Alta (Codi)`,data = taula_s_compl, nvmax = 55, really.big = T)
res.sum2<-summary(models2)
res.sum2

data.frame(
  Adj.R2 = which.max(res.sum2$adjr2),
  CP = which.min(res.sum2$cp),
  BIC = which.min(res.sum2$bic)
)
# id: model id
# object: regsubsets object
# data: data used to fit regsubsets
# outcome: outcome variable
get_model_formula <- function(id, object, outcome){
  # get models data
  models2 <- summary(object)$which[id,-1]
  # Get outcome variable
  #form <- as.formula(object$call[[2]])
  #outcome <- all.vars(form)[1]
  # Get model predictors
  predictors <- names(which(models2 == TRUE))
  predictors <- paste(predictors, collapse = "+")
  # Build model formula
  as.formula(paste0(outcome, "~", predictors))
}

get_cv_error <- function(model.formula, data){
  set.seed(1)
  train.control <- trainControl(method = "cv", number = 52)
  cv <- train(model.formula, data = data, method = "lm",
              trControl = train.control)
  cv$results$RMSE
}
model.ids <- 1:52
cv.errors <-  map(model.ids, get_model_formula, models2, "`Total Cost Sense Estructura`") %>%
  map(get_cv_error, data = taula_s_compl) %>%
  unlist()
cv.errors
# Select the model that minimize the CV error
which.min(cv.errors)#23 Var
coef(models2, 41)
get_model_formula(41, models2, "`Total Cost Sense Estructura`")

Total_Cost2=-2247.638282-9.846188*`Edat Pacient a l'Admissió` + 
  996.094555*`És Quirúrgic (Indicador)` + 4697.572646*`Ind Passa X UCI` + 
  1199.799849*`Circumstància Ingrés (Codi)` + 151.199142*`Circumstància Alta (Codi)` + 
  564.122152*`Estada HCB (en dies naturals)` + 5155.087187*`\`Servei Alta (Codi)\`B-CTR` + 
  3166.599548*`\`Servei Alta (Codi)\`CAR` -672.002224* `\`Servei Alta (Codi)\`CCV` + 
  211.376389*`\`Servei Alta (Codi)\`CGI` -365.484823 *`\`Servei Alta (Codi)\`CIP` + 
  322.166702*`\`Servei Alta (Codi)\`COT` + 221.953608*`\`Servei Alta (Codi)\`CTR` - 
  414.441910*`\`Servei Alta (Codi)\`DER` + 1608.009493*`\`Servei Alta (Codi)\`ERM` + 
  524.489214*`\`Servei Alta (Codi)\`GAS` -116.780416 *`\`Servei Alta (Codi)\`HBP` -
  2938.656646*`\`Servei Alta (Codi)\`HDM` + 3874.472220*`\`Servei Alta (Codi)\`HEM` + 
  1099.522369*`\`Servei Alta (Codi)\`HEP` + 222.033916*`\`Servei Alta (Codi)\`INF` + 
  267.880470*`\`Servei Alta (Codi)\`MAS` -701.781374 *`\`Servei Alta (Codi)\`MDI` + 
  2332.787035*`\`Servei Alta (Codi)\`NEF` -264.230552 *`\`Servei Alta (Codi)\`NMO` - 
  1700.211827*`\`Servei Alta (Codi)\`NNT` + 2970.792835*`\`Servei Alta (Codi)\`NRC` + 
  1375.518886*`\`Servei Alta (Codi)\`NRL` + 362.028678*`\`Servei Alta (Codi)\`OBS` - 
  132.473428*`\`Servei Alta (Codi)\`OFT` - 444.493284*`\`Servei Alta (Codi)\`ORL` - 
  5228.766851*`\`Servei Alta (Codi)\`PIJ` -4617.888138* `\`Servei Alta (Codi)\`PSI` + 
  1455.980081*`\`Servei Alta (Codi)\`UCIR` + 1874.120084*`\`Servei Alta (Codi)\`UCOT` + 
  1661.834550*`\`Servei Alta (Codi)\`UDT` + 1273.490244*`\`Servei Alta (Codi)\`UGIN` + 
  1721.490864*`\`Servei Alta (Codi)\`UORL` + 1229.797638*`\`Servei Alta (Codi)\`UPSI` + 
  1640.387586*`\`Servei Alta (Codi)\`URM` + 978.129605*`\`Servei Alta (Codi)\`UTH`
par(mfrow=c(1,2))

plot(`Total Cost Sense Estructura`,ylim=c(0,250000))
plot(Total_Cost2,ylim=c(0,250000))

mean(`Total Cost Sense Estructura`)
mean(Total_Cost2)

mod38<-lm(`Total Cost Sense Estructura` ~ `Edat Pacient a l'Admissió` + 
            `És Quirúrgic (Indicador)` + `Ind Passa X UCI` + 
            `Circumstància Ingrés (Codi)` + `Circumstància Alta (Codi)` + 
            `Estada HCB (en dies naturals)` + `\`Servei Alta (Codi)\`B-CTR` + 
            `\`Servei Alta (Codi)\`CAR` + `\`Servei Alta (Codi)\`CCV` + 
            `\`Servei Alta (Codi)\`CGI` + `\`Servei Alta (Codi)\`CIP` + 
            `\`Servei Alta (Codi)\`COT` + `\`Servei Alta (Codi)\`CTR` + 
            `\`Servei Alta (Codi)\`DER` + `\`Servei Alta (Codi)\`ERM` + 
            `\`Servei Alta (Codi)\`GAS` + `\`Servei Alta (Codi)\`HBP` + 
            `\`Servei Alta (Codi)\`HDM` + `\`Servei Alta (Codi)\`HEM` + 
            `\`Servei Alta (Codi)\`HEP` + `\`Servei Alta (Codi)\`INF` + 
            `\`Servei Alta (Codi)\`MAS` + `\`Servei Alta (Codi)\`MDI` + 
            `\`Servei Alta (Codi)\`NEF` + `\`Servei Alta (Codi)\`NMO` + 
            `\`Servei Alta (Codi)\`NNT` + `\`Servei Alta (Codi)\`NRC` + 
            `\`Servei Alta (Codi)\`NRL` + `\`Servei Alta (Codi)\`OBS` + 
            `\`Servei Alta (Codi)\`OFT` + `\`Servei Alta (Codi)\`ORL` + 
            `\`Servei Alta (Codi)\`PIJ` + `\`Servei Alta (Codi)\`PSI` + 
            `\`Servei Alta (Codi)\`UCIR` + `\`Servei Alta (Codi)\`UCOT` + 
            `\`Servei Alta (Codi)\`UDT` + `\`Servei Alta (Codi)\`UGIN` + 
            `\`Servei Alta (Codi)\`UORL` + `\`Servei Alta (Codi)\`UPSI` + 
            `\`Servei Alta (Codi)\`URM` + `\`Servei Alta (Codi)\`UTH`)
smod38<-summary(mod38)
smod38

#  Call:
# lm(formula = `Total Cost Sense Estructura` ~ `Edat Pacient a l'Admissió` + 
#     `És Quirúrgic (Indicador)` + `Ind Passa X UCI` + `Circumstància Ingrés (Codi)` + 
#      `Circumstància Alta (Codi)` + `Estada HCB (en dies naturals)` + 
#    `\`Servei Alta (Codi)\`B-CTR` + `\`Servei Alta (Codi)\`CAR` + 
#     `\`Servei Alta (Codi)\`CCV` + `\`Servei Alta (Codi)\`CGI` + 
#      `\`Servei Alta (Codi)\`CIP` + `\`Servei Alta (Codi)\`COT` + 
#     `\`Servei Alta (Codi)\`CTR` + `\`Servei Alta (Codi)\`DER` + 
#    `\`Servei Alta (Codi)\`ERM` + `\`Servei Alta (Codi)\`GAS` + 
#   `\`Servei Alta (Codi)\`HBP` + `\`Servei Alta (Codi)\`HDM` + 
#  `\`Servei Alta (Codi)\`HEM` + `\`Servei Alta (Codi)\`HEP` + 
#      `\`Servei Alta (Codi)\`INF` + `\`Servei Alta (Codi)\`MAS` + 
#      `\`Servei Alta (Codi)\`MDI` + `\`Servei Alta (Codi)\`NEF` + 
#      `\`Servei Alta (Codi)\`NMO` + `\`Servei Alta (Codi)\`NNT` + 
#      `\`Servei Alta (Codi)\`NRC` + `\`Servei Alta (Codi)\`NRL` + 
#      `\`Servei Alta (Codi)\`OBS` + `\`Servei Alta (Codi)\`OFT` + 
#      `\`Servei Alta (Codi)\`ORL` + `\`Servei Alta (Codi)\`PIJ` + 
#      `\`Servei Alta (Codi)\`PSI` + `\`Servei Alta (Codi)\`UCIR` + 
#      `\`Servei Alta (Codi)\`UCOT` + `\`Servei Alta (Codi)\`UDT` + 
#      `\`Servei Alta (Codi)\`UGIN` + `\`Servei Alta (Codi)\`UORL` + 
#      `\`Servei Alta (Codi)\`UPSI` + `\`Servei Alta (Codi)\`URM` + 
#      `\`Servei Alta (Codi)\`UTH`)
# 
# Residuals:
#   Min     1Q Median     3Q    Max 
# -65041   -808    -43    606 149495 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                     -2384.332    125.488 -19.001  < 2e-16 ***
#   `Edat Pacient a l'Admissió`        -8.272      1.145  -7.227 5.02e-13 ***
#   `És Quirúrgic (Indicador)`       1039.953     57.765  18.003  < 2e-16 ***
#   `Ind Passa X UCI`                4700.634     86.823  54.140  < 2e-16 ***
#   `Circumstància Ingrés (Codi)`    1183.548     49.155  24.078  < 2e-16 ***
#   `Circumstància Alta (Codi)`       150.770     23.076   6.534 6.50e-11 ***
#   `Estada HCB (en dies naturals)`   563.721      2.432 231.765  < 2e-16 ***
#   `\\`Servei Alta (Codi)\\`B-CTR`  5271.578   3798.200   1.388  0.16517    
# `\\`Servei Alta (Codi)\\`CAR`    3222.872     96.177  33.510  < 2e-16 ***
#   `\\`Servei Alta (Codi)\\`CCV`    -647.697    116.241  -5.572 2.53e-08 ***
#   `\\`Servei Alta (Codi)\\`CGI`     250.212    106.786   2.343  0.01913 *  
#   `\\`Servei Alta (Codi)\\`CIP`    -314.871    132.653  -2.374  0.01762 *  
#   `\\`Servei Alta (Codi)\\`COT`     347.698     85.125   4.085 4.42e-05 ***
#   `\\`Servei Alta (Codi)\\`CTR`     262.668    168.934   1.555  0.11999    
# `\\`Servei Alta (Codi)\\`DER`    -383.422    229.760  -1.669  0.09517 .  
# `\\`Servei Alta (Codi)\\`ERM`    1655.375    205.071   8.072 7.08e-16 ***
#   `\\`Servei Alta (Codi)\\`GAS`     585.912    135.417   4.327 1.52e-05 ***
#   `\\`Servei Alta (Codi)\\`HBP`     -77.489    109.802  -0.706  0.48037    
# `\\`Servei Alta (Codi)\\`HDM`   -2886.305    131.772 -21.904  < 2e-16 ***
#   `\\`Servei Alta (Codi)\\`HEM`    3939.164    125.981  31.268  < 2e-16 ***
#   `\\`Servei Alta (Codi)\\`HEP`    1166.377    151.953   7.676 1.68e-14 ***
#   `\\`Servei Alta (Codi)\\`INF`     286.250    155.834   1.837  0.06623 .  
# `\\`Servei Alta (Codi)\\`MAS`     340.529    269.711   1.263  0.20675    
# `\\`Servei Alta (Codi)\\`MDI`    -665.035    111.198  -5.981 2.24e-09 ***
#   `\\`Servei Alta (Codi)\\`NEF`    2397.772    127.880  18.750  < 2e-16 ***
#   `\\`Servei Alta (Codi)\\`NMO`    -209.702    131.009  -1.601  0.10946    
# `\\`Servei Alta (Codi)\\`NNT`   -1527.812    162.630  -9.394  < 2e-16 ***
#   `\\`Servei Alta (Codi)\\`NRC`    3011.577    141.125  21.340  < 2e-16 ***
#   `\\`Servei Alta (Codi)\\`NRL`    1432.112    127.858  11.201  < 2e-16 ***
#   `\\`Servei Alta (Codi)\\`OBS`     452.107     90.176   5.014 5.36e-07 ***
#   `\\`Servei Alta (Codi)\\`OFT`    -119.892     84.250  -1.423  0.15473    
# `\\`Servei Alta (Codi)\\`ORL`    -397.313    134.518  -2.954  0.00314 ** 
#   `\\`Servei Alta (Codi)\\`PIJ`   -5089.032    212.351 -23.965  < 2e-16 ***
#   `\\`Servei Alta (Codi)\\`PSI`   -4526.862    204.955 -22.087  < 2e-16 ***
#   `\\`Servei Alta (Codi)\\`UCIR`   1511.822    733.891   2.060  0.03940 *  
#   `\\`Servei Alta (Codi)\\`UCOT`   1910.628    270.924   7.052 1.79e-12 ***
#   `\\`Servei Alta (Codi)\\`UDT`    1705.741    324.727   5.253 1.50e-07 ***
#   `\\`Servei Alta (Codi)\\`UGIN`   1375.246   1437.214   0.957  0.33863    
# `\\`Servei Alta (Codi)\\`UORL`   1794.929    762.491   2.354  0.01858 *  
#   `\\`Servei Alta (Codi)\\`UPSI`   1323.430    294.043   4.501 6.79e-06 ***
#   `\\`Servei Alta (Codi)\\`URM`    1689.384    109.325  15.453  < 2e-16 ***
#   `\\`Servei Alta (Codi)\\`UTH`    1043.841    144.631   7.217 5.39e-13 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 3798 on 43035 degrees of freedom
# Multiple R-squared:  0.6802,	Adjusted R-squared:  0.6799 
# F-statistic:  2232 on 41 and 43035 DF,  p-value: < 2.2e-16
# 


