#########################################################
#Deveolped by Pablo Martinez Angerosa - Vanessa Alcalde #
#########################################################


############
# librarys #
############

library(tidyverse)
library(HH)
library(lmtest)
library(leaps)
library(nortest)
library(MASS)
library(boot)
library(glmulti)
library(MLmetrics)
library(dplyr)
library(magrittr)
library(glmnet)
library(stats)
library(boot)
library(glmulti)
library(car)
library(tree)

################
# Source files #
################

source("databaseMerge.R")

database_coins = DATABASE_COINS

# Seleccionamos aleatoriamente 30% para train y 70% para test.
set.seed(1234)
dataprice.train = slice_sample(database_coins, prop = 0.3)
dataprice.test = anti_join(database_coins, dataprice.train)

ancho_ventana_entrenamiento = length(dataprice.train$ltc_open)
ancho_ventana_prediccion = length(dataprice.test$btc_trend)

##################################################################################
# Algoritmo de fuerza bruta utilizado para encontrar el modelo con el  minimo RMSE
##################################################################################

# Buscamos todas las combinaciones posibles entre n variables.  
combinaciones_posibles = crossing(
   var1 = 0:1, 
   var2 = 0:1,
   var3 = 0:1,
   var4 = 0:1,
   var5 = 0:1,
   var6 = 0:1,
   var7 = 0:1,
   var8 = 0:1,
   var9 = 0:1,
   var10 = 0:1,
   var11 = 0:1,
   var12 = 0:1,
   var13 = 0:1,
   var14 = 0:1,
   var15 = 0:1
   # var17 = 0:1, 
   # var18 = 0:1,
   # var19 = 0:1,
   # var20 = 0:1
  )

# el grupo de variables para buscar todas las posibles combinaciones surge de la prueba y el error, y la literatura existente. 
variables_lag = c(
  "btc_trend", 
  "btc_close_lag1",
  "btc_close_lag2",
  "btc_close_lag3",
  "btc_close_lag4",
  "btc_close_lag5",
  # #"btc_close_lag6",
  # #"btc_close_lag7",
  # #"btc_close_lag8",
  "btc_vol_lag1",
  "btc_vol_lag2",
  "btc_vol_lag3",
  "btc_vol_lag4",
  "btc_vol_lag5",
  # #"btc_vol_lag6"
   "eth_open",
   "eth_vol",
    "ltc_open" , 
    "ltc_vol"  
  #  "bch_open"  , 
  #  "bch_vol"   ,
  #  "etc_open"  , 
  #  "etc_vol"   ,
  #  "link_open" , 
  #  "link_vol"  ,
  #  "rep_open"  , 
  #  "rep_vol"  
  
  )

# multiple linear regression codigo para eval
lm_eval_first = "lmClose = lm(btc_close~  "
lm_eval_end = ", data=dataprice.train)"

# ridge codigo para eval
ridge_eval_first = "ridge_fit = cv.glmnet(x = model.matrix(btc_close ~  "
ridge_eval_end = ", data = dataprice.train), y = dataprice.train$btc_close,alpha = 0)"
ridge_eval_model = "test_matrix = model.matrix(btc_close ~"
ridge_eval_model_end = ", data = dataprice.test)"

# GAM poly (more flexible algorithms) 
gam_eval_first = "gam_fit = lm(btc_close~"
gam_eval_end = ", data=dataprice.train)"


# Regression Tree
regression_tree_eval_first = "tree_fit = tree(formula =btc_close~"
regression_tree_eval_end = ",data= database_coins, subset= dataprice.train)"

RMSE_dataprice = c()
RMSE_ridge_dataprice = c()
RMSE_gam_dataprice = c()
rRMSE_regression_tree_dataprice = c()
MAPE_dataprice = c()
models_fit = c()
models_fit_ridge = c()
models_fit_gam = c()
models_fit_regression_tree_dataprice = c()
total_combinaciones = nrow(combinaciones_posibles)

# recorremos todas las combinaciones posibles #
# cuidado este algoritmo puede tomar tiempo considerado

for (i in c(1:total_combinaciones)){
  
  eval = ""
  eval_gam = ""
  
  # modificar degree para cambiar el grado del Poly GAM 
  
  for( variable in variables_lag[combinaciones_posibles[i,]==1]){
    if(eval == ""){
      variable_gam = paste("poly(", variable, ", degree = 2)")
      variable = paste(" ", variable)
    } else {
      
      variable_gam = paste("+ poly(", variable, ", degree = 2)")
      variable = paste("+ ", variable)  
    }
    
    eval = paste(eval, variable)
    eval_gam = paste(eval_gam, variable_gam)
  }
  
  # nos aseguramos que el modelo tiene variables explicativas
  if(eval!=""){
    
    # #############################
    # # Multiple Linear Regresion #
    # #############################

    lm_eval = paste(lm_eval_first,eval,lm_eval_end)
    # se ejecuta una instancia d ajuste del lmClose
    eval(parse(text=lm_eval))
    pred.dataprice = predict(lmClose, dataprice.test, interval = "prediction")
    # RMSE
    RMSE_dataprice[length(RMSE_dataprice) + 1] = sqrt(sum((pred.dataprice[,1] - dataprice.test$btc_close )^2)/length(dataprice.test$btc_close))
    # MAPE
    #MAPE_dataprice[length(MAPE_dataprice) + 1] = MAPE(pred.dataprice[,1], dataprice.test$btc_close)
    # Guardamos el Modelo
    models_fit[length(models_fit) + 1] = lm_eval
     
     
    # #########
    # # Ridge #
    # #########
    
     
    ridge_eval = paste(ridge_eval_first,eval,ridge_eval_end)
    # se ejecuta una instancia d ajuste del lmClose
    eval(parse(text=ridge_eval))

    opt_lambda = ridge_fit$lambda.min
    log_opt_lambda = log(opt_lambda)

    matrix_eval = paste(ridge_eval_model,eval,ridge_eval_model_end)
    eval(parse(text=matrix_eval))
    # RMSE
    RMSE_ridge_dataprice[length(RMSE_ridge_dataprice) + 1] = sqrt(mean((dataprice.test$btc_close - predict(ridge_fit, s = opt_lambda, newx = test_matrix))^2))
    # Guardamos el Modelo
    models_fit_ridge[length(models_fit_ridge) + 1] = ridge_eval
    
    
    #######
    # GAM #
    #######
    
    gam_eval = paste(gam_eval_first,eval_gam,gam_eval_end)
    eval(parse(text=gam_eval))
    # RMSE
    RMSE_gam_dataprice[length(RMSE_gam_dataprice) + 1] = sqrt(mean((dataprice.test$btc_close - predict(gam_fit, dataprice.test))^2))
    models_fit_gam[length(models_fit_gam) + 1] = gam_eval
    
    ###################
    # Regression Tree #
    ###################
    
    regression_tree_eval = paste(regression_tree_eval_first,eval,regression_tree_eval_end)
    eval(parse(text=regression_tree_eval))
    # rRMSE
    rRMSE_regression_tree_dataprice[length(rRMSE_regression_tree_dataprice) + 1] = sqrt(mean(((dataprice.test$btc_close - predict(tree_fit, dataprice.test))/dataprice.test$btc_close )^2))
    models_fit_regression_tree_dataprice[length(models_fit_regression_tree_dataprice) + 1] = regression_tree_eval
    
    # estado del algoritmo 
    print((i/total_combinaciones))
  }
}


# Dataframe con el resultado RMSE y los modelos de todas las posibles combinaciones ejecutadas #
# El que tiene menor RMSE en cada dataframe es el algoritmo optimo para cada tipo de modelado #
info = data.frame("model" = models_fit, "RMSE" = RMSE_dataprice)
infoRidge = data.frame("model" = models_fit_ridge, "RMSE" = RMSE_ridge_dataprice)
infoGamPoly = data.frame("model" = models_fit_gam, "RMSE" = RMSE_gam_dataprice )
ingoRegressionTree = data.frame("model" = models_fit_regression_tree_dataprice, "rRMSE"=rRMSE_regression_tree_dataprice)

#########################################################################
# Resultados obtenidos de las pruebas con distintos grupos de variables #
#########################################################################


# best fit multiple linear regression, 20 variables test!
# RMSE 291.9711
lmClose = lm(btc_close~ btc_close_lag1 +  btc_close_lag2 +  btc_close_lag4 +  btc_vol_lag1 +  btc_vol_lag3 +  btc_vol_lag4 +  btc_trend +  ltc_open , data=dataprice.train)
pred.dataprice = predict(lmClose, dataprice.test, interval = "prediction")
# RMSE
RMSE = sqrt(sum((pred.dataprice[,1] - dataprice.test$btc_close )^2)/length(dataprice.test$btc_close))


# best fit Ridge
#ridge_fit = cv.glmnet(x = model.matrix(btc_close ~      btc_close_lag1 +  btc_close_lag2 +  btc_close_lag5 +  btc_vol_lag1 +  btc_vol_lag4 +  eth_vol , data = dataprice.train),
# RMSE 302.8086


# best fit gam poly 4
# RMSE 311.7134
# gam_fit = lm(btc_close~  poly( btc_close_lag1 , degree = 4) , data=dataprice.train)


# best fit gam poly 3
# RMSE 307.6025
# gam_fit = lm(btc_close~  poly( btc_close_lag1 , degree = 3) , data=dataprice.train)

# best fit gam poly 2
# RMSE 295.9326
# gam_fit = lm(btc_close~  poly( btc_close_lag1 , degree = 2) + poly( btc_close_lag2 , degree = 2) , data=dataprice.train)

# best fit gam poly 1 == multiple linear regression,  como esperabamos es un modelo lineal y tiene igual comportamiento
# RMSE 293.9643
# gam_fit = lm(btc_close~  poly( btc_close_lag1 , degree = 1) + poly( btc_close_lag2 , degree = 1) , data=dataprice.train)



################################################################################
# Analisis Inferencial (Modelos linales diagnostico) del modelo con menor RMSE #
################################################################################

lmClose = lm(btc_close~ btc_close_lag1 +  btc_close_lag2 +  btc_close_lag4 +  btc_vol_lag1 +  btc_vol_lag3 +  btc_vol_lag4 +  btc_trend +  ltc_open , data=dataprice.train)


summary(lmClose)
# btc_close_lag1 btc_close_lag2 presenta multicolinealidad
vif(lmClose)

# rechaza normalidad , H_0 es normal
shapiro.test(lmClose$residuals)

# analizamos diagnostico residuos
par(mfrow=c(2,2))
plot(lmClose) # la 49 parece outlier

# homogenidad en varianza / 1 rechaza y el otro no
ncvTest(lmClose)
bptest(lmClose)

# outlier 
outlierTest(lmClose) # 49 es un outlier

# influyentes 
dffits(lmClose)
cooks.distance(lmClose)
sum(cooks.distance(lmClose)>=(0.5))



###############################################################
# Regresion Logistica con el Modelo resultante con menor RMSE #
###############################################################


set.seed(1234)
dataprice.train = slice_sample(DATABASE_DIRECTION, prop = 0.3)
dataprice.test = anti_join(DATABASE_DIRECTION, dataprice.train)


# ajuste con el modelo con mejor performance obtenido
glm.fits = glm(price_direction ~ btc_close_lag1 +  btc_close_lag2 +  btc_close_lag4 +  btc_vol_lag1 +  btc_vol_lag3 +  btc_vol_lag4 +  btc_trend +  ltc_open , data=dataprice.train ,family=binomial )
glm.probs = predict(glm.fits,dataprice.test, type="response")

glm.pred = rep("Down",length(dataprice.test$price_direction))
glm.pred[glm.probs >.5] = "Up"
table(glm.pred ,dataprice.test$price_direction)

# glm.pred Down Up
# Down   23 15
# Up     55 70

# > 70/(55+70)
# [1] 0.56
# > 23/(23+15)
# [1] 0.6052632

#####################################################################################
# Glmulti con las variables de la base y las interacciones puede tomar mucho tiempo #
#####################################################################################
glm_fit3 = glmulti(
  y = price_direction ~ .*.,
  data =  dataprice.train,
  family = binomial,
  method = "g",
  plotty = FALSE,
  report = TRUE,
  marginality = TRUE,
  deltaB = 0,
  deltaM = 0.01,
  conseq = 6,
  sexrate = 0.15,
  imm = 0.2
) 

fit = glm_fit3@objects[[1]]

# Resultado para GLM multi despues de varias horas de computos! # opcional
glm.fits = glm(price_direction ~ 1 + btc_trend + btc_close_lag1 + btc_close_lag2 +
                 btc_close_lag3 + btc_close_lag4 + btc_close_lag5 + btc_vol_lag1 +
                 btc_vol_lag2 + btc_vol_lag3 + btc_vol_lag4 + btc_vol_lag5 +
                 eth_open + eth_vol + ltc_open + ltc_vol + btc_close_lag2:btc_trend +
                 btc_close_lag2:btc_close_lag1 + btc_close_lag3:btc_close_lag1 +
                 btc_close_lag4:btc_close_lag1 + btc_close_lag4:btc_close_lag2 +
                 btc_close_lag5:btc_close_lag1 + btc_close_lag5:btc_close_lag2 +
                 btc_close_lag5:btc_close_lag4 + btc_vol_lag1:btc_close_lag1 +
                 btc_vol_lag1:btc_close_lag4 + btc_vol_lag2:btc_trend + btc_vol_lag2:btc_vol_lag1 +
                 btc_vol_lag3:btc_trend + btc_vol_lag3:btc_close_lag3 + btc_vol_lag3:btc_vol_lag1 +
                 btc_vol_lag3:btc_vol_lag2 + btc_vol_lag4:btc_close_lag1 +
                 btc_vol_lag4:btc_close_lag3 + btc_vol_lag4:btc_close_lag4 +
                 btc_vol_lag4:btc_close_lag5 + btc_vol_lag4:btc_vol_lag2 +
                 btc_vol_lag4:btc_vol_lag3 + btc_vol_lag5:btc_trend + btc_vol_lag5:btc_close_lag1 +
                 btc_vol_lag5:btc_close_lag3 + btc_vol_lag5:btc_close_lag4 +
                 btc_vol_lag5:btc_close_lag5 + btc_vol_lag5:btc_vol_lag3 +
                 btc_vol_lag5:btc_vol_lag4 + eth_open:btc_trend + eth_open:btc_close_lag2 +
                 eth_open:btc_close_lag3 + eth_open:btc_vol_lag2 + eth_open:btc_vol_lag4 +
                 eth_vol:btc_trend + eth_vol:btc_close_lag1 + eth_vol:btc_close_lag2 +
                 eth_vol:btc_vol_lag1 + eth_vol:btc_vol_lag2 + eth_vol:btc_vol_lag5 +
                 ltc_open:btc_trend + ltc_open:btc_close_lag4 + ltc_open:btc_vol_lag1 +
                 ltc_open:btc_vol_lag2 + ltc_open:btc_vol_lag3 + ltc_open:btc_vol_lag5 +
                 ltc_vol:btc_trend + ltc_vol:btc_close_lag3 + ltc_vol:btc_close_lag4 +
                 ltc_vol:btc_vol_lag1 + ltc_vol:btc_vol_lag3 , data=dataprice.train ,family=binomial )

glm.probs = predict(glm.fits,dataprice.test, type="response")
glm.probs = predict(fit,dataprice.test, type="response")
glm.pred = rep("Down",length(dataprice.test$price_direction))
glm.pred[glm.probs >.5] = "Up"

table(glm.pred ,dataprice.test$price_direction)

#glm.pred Down Up
#Down   42 39           46/(36+46) = 0.5609756
#Up     36 46           42/(42+39) = 0.5185185






