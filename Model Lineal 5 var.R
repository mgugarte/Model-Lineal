setwd("C:/Users/mpgonzalez/Desktop/Costos per complicació")
library(readxl)
library(tidyverse)
library(caret)
library(leaps)
library(car)

taula_compl <- read_excel("C:/Users/mpgonzalez/Desktop/Costos per complicació/Taula_compl.xlsx")
attach(taula_compl)

#Busquem el millor model amb aquestes variables: Sexe (Desc)	Edat Pacient a l'Admissió	És Quirúrgic (Indicador)	Ind Passa X UCI	Circumstància Ingrés (Codi)	Circumstància Alta (Codi)	Estada HCB (en dies naturals)

models <- regsubsets(`Total Cost Sense Estructura`~.-`Servei Alta (Codi)`, data = taula_compl, nvmax = 8)
res.sum<-summary(models)
res.sum

data.frame(
  Adj.R2 = which.max(res.sum$adjr2),
  CP = which.min(res.sum$cp),
  BIC = which.min(res.sum$bic)
)
# id: model id
# object: regsubsets object
# data: data used to fit regsubsets
# outcome: outcome variable
get_model_formula <- function(id, object, outcome){
  # get models data
  models <- summary(object)$which[id,-1]
  # Get outcome variable
  #form <- as.formula(object$call[[2]])
  #outcome <- all.vars(form)[1]
  # Get model predictors
  predictors <- names(which(models == TRUE))
  predictors <- paste(predictors, collapse = "+")
  # Build model formula
  as.formula(paste0(outcome, "~", predictors))
}
#Millor model 3 var
get_model_formula(3, models, "`Total Cost Sense Estructura`")


get_cv_error <- function(model.formula, data){
  set.seed(1)
  train.control <- trainControl(method = "cv", number = 7)
  cv <- train(model.formula, data = data, method = "lm",
              trControl = train.control)
  cv$results$RMSE
}
model.ids <- 1:7
cv.errors <-  map(model.ids, get_model_formula, models, "`Total Cost Sense Estructura`") %>%
  map(get_cv_error, data = taula_compl) %>%
  unlist()
cv.errors
# Select the model that minimize the CV error
which.min(cv.errors)
#El model de 6 variables és el millor model , té l'error de predicció més petit.
#Els coeficientes d'aquest  model sòn:

get_model_formula(5, models, "`Total Cost Sense Estructura`")
coef(models, 5)
Total_Cost= -537.34933-41.08537*`Edat Pacient a l'Admissió` + 
  2437.32572*`És Quirúrgic (Indicador)` + 7280.73301*`Ind Passa X UCI` + 
  283.15086*`Circumstància Alta (Codi)` + 637.59119*`Estada HCB (en dies naturals)`

#Comparem amb el cost total sense estrucutra
par(mfrow=c(1,2))
plot(`Total Cost Sense Estructura`,ylim=c(0,250000))
plot(Total_Cost,ylim=c(0,250000))

par(mfrow=c(1,2))
hist(`Total Cost Sense Estructura`,xlim = c(0,250000),ylim = c(0,1750))
hist(Total_Cost,xlim = c(0,250000),ylim = c(0,1750))

sum(Total_Cost)
sum(`Total Cost Sense Estructura`)

comp_c5<-cbind(Total_Cost,`Total Cost Sense Estructura`)
View(comp_c5)
comp_c23<-cbind(Total_Cost1,`Total Cost Sense Estructura`)
View(comp_c23)
comp_models<-cbind(Total_Cost,Total_Cost1,`Total Cost Sense Estructura`)
View(comp_models)

mod5<-lm(`Total Cost Sense Estructura` ~ `Edat Pacient a l'Admissió` + 
           `És Quirúrgic (Indicador)` + `Ind Passa X UCI` + 
           `Circumstància Alta (Codi)` + `Estada HCB (en dies naturals)`)
smod5<-summary(mod5)
smod5
