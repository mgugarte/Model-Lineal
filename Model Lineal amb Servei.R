setwd("C:/Users/mpgonzalez/Desktop/Costos per complicació")
library(readxl)
library(tidyverse)
library(caret)
library(leaps)
library(car)
library(dummies)
taula_compl <- read_excel("C:/Users/mpgonzalez/Desktop/Costos per complicació/Taula_compl.xlsx")

taula_compl<-cbind(taula_compl,servei_alta)
attach(taula_compl)
View(taula_compl)

Export(taula_compl,"Taula amb complicacions.xlsx")
#Busquem el millor model amb aquestes variables: Sexe (Desc)	Edat Pacient a l'Admissió	És Quirúrgic (Indicador)	Ind Passa X UCI	Circumstància Ingrés (Codi)	Circumstància Alta (Codi)	Estada HCB (en dies naturals) Servei Alta (Codi)
models1<-regsubsets(`Total Cost Sense Estructura`~.-`Servei Alta (Codi)`,data = taula_compl, nvmax = 42)
res.sum1<-summary(models1)
res.sum1

data.frame(
  Adj.R2 = which.max(res.sum1$adjr2),
  CP = which.min(res.sum1$cp),
  BIC = which.min(res.sum1$bic)
)
# id: model id
# object: regsubsets object
# data: data used to fit regsubsets
# outcome: outcome variable
get_model_formula <- function(id, object, outcome){
  # get models data
  models1 <- summary(object)$which[id,-1]
  # Get outcome variable
  #form <- as.formula(object$call[[2]])
  #outcome <- all.vars(form)[1]
  # Get model predictors
  predictors <- names(which(models1 == TRUE))
  predictors <- paste(predictors, collapse = "+")
  # Build model formula
  as.formula(paste0(outcome, "~", predictors))
}
#Millor model 22 var
get_model_formula(22, models1, "`Total Cost Sense Estructura`")

get_cv_error <- function(model.formula, data){
  set.seed(1)
  train.control <- trainControl(method = "cv", number = 40)
  cv <- train(model.formula, data = data, method = "lm",
              trControl = train.control)
  cv$results$RMSE
}
model.ids <- 1:40
cv.errors <-  map(model.ids, get_model_formula, models1, "`Total Cost Sense Estructura`") %>%
  map(get_cv_error, data = taula_compl) %>%
  unlist()
cv.errors
# Select the model that minimize the CV error
which.min(cv.errors)#23 Var
coef(models1, 23)
get_model_formula(23, models1, "`Total Cost Sense Estructura`")
Total_Cost1= -3110.2134-42.1043*`Edat Pacient a l'Admissió` +2577.0054*`És Quirúrgic (Indicador)` +5138.2267*`Ind Passa X UCI` + 
  1352.8398*`Circumstància Ingrés (Codi)` + 575.4247*`Circumstància Alta (Codi)` + 
  652.6778*`Estada HCB (en dies naturals)` + 5853.6922*`\`Servei Alta (Codi)\`CAR` + 
  1700.5094*`\`Servei Alta (Codi)\`CCV` -1395.0353*`\`Servei Alta (Codi)\`CGI` -
  4201.9071*`\`Servei Alta (Codi)\`COT` + 3692.2534*`\`Servei Alta (Codi)\`ERM` + 
  1387.3217*`\`Servei Alta (Codi)\`GAS` -1959.9067*`\`Servei Alta (Codi)\`HBP` -
  5772.4299*`\`Servei Alta (Codi)\`HDM` + 6010.0061*`\`Servei Alta (Codi)\`HEM` + 
  4162.3657*`\`Servei Alta (Codi)\`HEP` + 5720.6230*`\`Servei Alta (Codi)\`NEF` + 
  2410.0320*`\`Servei Alta (Codi)\`NRC` - 1960.7520*`\`Servei Alta (Codi)\`ONC` - 
  3528.0781*`\`Servei Alta (Codi)\`ORL` -9903.3739*`\`Servei Alta (Codi)\`PSI` + 
  3798.0742*`\`Servei Alta (Codi)\`URM` + 14316.0442*`\`Servei Alta (Codi)\`UTH`

par(mfrow=c(1,2))
plot(`Total Cost Sense Estructura`,ylim=c(0,250000))
plot(Total_Cost1,ylim=c(0,250000))

par(mfrow=c(1,2))
hist(`Total Cost Sense Estructura`,xlim = c(0,250000),ylim = c(0,1750))
hist(Total_Cost1,xlim = c(0,250000),ylim = c(0,1750))

sum(Total_Cost1)
sum(`Total Cost Sense Estructura`)

mod23<-lm(`Total Cost Sense Estructura` ~ `Edat Pacient a l'Admissió` + 
              `És Quirúrgic (Indicador)` + `Ind Passa X UCI` + 
              `Circumstància Ingrés (Codi)` + `Circumstància Alta (Codi)` + 
              `Estada HCB (en dies naturals)` + `\`Servei Alta (Codi)\`CAR` + 
              `\`Servei Alta (Codi)\`CCV` + `\`Servei Alta (Codi)\`CGI` + 
              `\`Servei Alta (Codi)\`COT` + `\`Servei Alta (Codi)\`ERM` + 
              `\`Servei Alta (Codi)\`GAS` + `\`Servei Alta (Codi)\`HBP` + 
              `\`Servei Alta (Codi)\`HDM` + `\`Servei Alta (Codi)\`HEM` + 
              `\`Servei Alta (Codi)\`HEP` + `\`Servei Alta (Codi)\`NEF` + 
              `\`Servei Alta (Codi)\`NRC` + `\`Servei Alta (Codi)\`ONC` + 
              `\`Servei Alta (Codi)\`ORL` + `\`Servei Alta (Codi)\`PSI` + 
              `\`Servei Alta (Codi)\`URM` + `\`Servei Alta (Codi)\`UTH`)
smod23<-summary(mod23)
smod23
