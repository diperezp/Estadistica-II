#---------------------------------------------------------------------
#---------------------------------------------------------------------
#                                  Analisis
#                                 de los datos
#---------------------------------------------------------------------
#---------------------------------------------------------------------

#librerias para la implementacion 
library(car)
library(klaR)
library(mctest)
library(glmnet)
library(tidymodels)
path="C:/Users/diego/Documents/GitHub/Estadistica-II/Trabajo I/Datos/Base04.csv"
path_resources="C:/Users/diego/Documents/GitHub/Estadistica-II/Trabajo I/resorces/Functions.R"

#lectura de los datos en un dataframe
data=read.csv(file=path)

#visualizacion de los datos
View(data)
str(data)
summary(data)

#informacion de las variables
#• Rendimiento: kg/ hecta kilogramos de cafe por hectarea
#• Roya:  presenta/no presenta (1/0) brote de roya
#• Broca: presenta/ no presenta (1/0) numero de focos identificados en el lote
#• X1: Altitud del lote (en metros sobre el nivel del mar).
#• X2: Temperatura media anual del lote (en ◦C).
#• X3: Precipitación acumulada anual (en mm).
#• X4: Humedad relativa media (en %).
#• X5: Densidad de siembra (en miles de plantas por hectárea).
#• X6: Edad del cafetal desde la siembra o la última renovación (en años).
#• X7: Fertilización nitrogenada (en kg de nitrógeno por hectárea al año).
#• X9: Pendiente del terreno (en %).
#• X10: Distancia del lote a la cabecera municipal (en km).
#• W: Variedad sembrada en el lote (Castillo, variedad resistente a la roya de amplio
data_name=data

name=c("Rendimiento",
       "Roya",
       "Broca",
       "Alti",
       "Temp",
       "Agua",
       "RH",
       "Rho",
       "Age",
       "N2",
       "Cob",
       "Mt",
       "Dt",
       "V")

names(data_name)<-name
names(data_name)


#dividimos los datos en 70-30 para entrenamiento y prueba
set.seed(123)   #semilla de aleatoridad
train_index=sample(1:nrow(data_name),0.7*nrow(data_name)) #division de los datos 70-30
train_data=data_name[train_index,]                        # datos de entrenamiento
test_data=data_name[-train_index,]                        # datos de testeo

#visualizamos los conjuntos de datos
str(train_data)
str(test_data)




#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#                                     Regresion lineal
#                                     ridge y lasso
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------




#modelo de regresion lineal multiple con variables categoricas
modelo_lineal=lm(Rendimiento~.-Broca-Roya,data=data_name)

#...............................................................................
# Evaluacion de significancia
#...............................................................................
#resumen de la tabla anova y la significancia individual
anova(modelo_lineal)
summary(modelo_lineal)




#...............................................................................
# Evaluacion de multicolinealidad
#...............................................................................

#matrix de correlacion
X<-model.matrix(modelo_lineal)
cor(X[,-1])
#VIFj
#---------------------------------
#VIF<5 No existe multicolinealidad
#VIF<=10 multicolin moderada
#VIF>10 multicolin grave
#---------------------------------
vif(modelo_lineal)



#/////////////////////////////////////////////////////////////////////////
# Para poder hacer la pruebas de multicolinalidad con el numero de condicion y 
# los indices de condicion se debe de escalar los datos para tener resultados congruentes.
#////////////////////////////////////////////////////////////////////////

# numero condicion (Kappa)
# kappa = lambda_max/lambda_min
#------------------------------------
#sqrt(K)<=10 No hay multicolinealidad
#sqrt(K)<=31.62 multicolin moderada
#sqrt(K)>31.62 multicolin grave
#------------------------------------
X<-model.matrix(modelo_lineal) #matrix del modelo
X_scale<-scale(X[,-1])        #matrix del modelo excluyendo el intercepto
kappa(X_scale,exact=TRUE)


#Indice de condicion
#kappa_i =lambda_max/lambda_i
#-------------------------------------
#sqrt(K)<=10 No hay multicolinealidad
#sqrt(K)<=31.62 multicolin moderada
#sqrt(K)>31.62 multicolin grave
#-------------------------------------
# Calcular SVD sobre la matriz centrada y escalada
svd_res <- svd(X_scale)
indices_centrados <- max(svd_res$d) / svd_res$d
indices_centrados


#...............................................................................
# Verificacion de modelos con Ridge lasso
#..............................................................................


#----------------
#       Ridge
#----------------
#Extraemos los datos para los aplicar ridge

Y=data_name$Rendimiento
#metodo de ridge grafico
ridgefit = glmnet(X,Y,alpha=0,lambda=seq(0,5,0.0001))
plot(ridgefit, xvar="lambda",xlab=expression(log(lambda)))





ridgecv=cv.glmnet(X,Y,alpha=0,nfolds=10,lambda=seq(0,100,length.out=100))
cvmridge = min(ridgecv$cvm)
small.lambda.index=which(ridgecv$cvm==cvmridge)
plot(ridgecv,xlab=expression(log(lambda)))
#Horizontal line: Minimum mean cross-validated error
abline(h= ridgecv$cvm[small.lambda.index])
#Vertical line: Optimum lambda
abline(v=-log(ridgecv$lambda[small.lambda.index]))

ridgecv$lambda.min
coef(ridgecv)



#--------------
#lasso
#--------------

lassocv=cv.glmnet(X,Y,alpha=1,nfolds=10,lambda=seq(1e-2,length.out=100))
cvmlasso = min(lassocv$cvm)

small.lambda.index=which(lassocv$cvm==cvmlasso)

plot(lassocv,xlab=expression(log(lambda)))

#Horizontal line: Minimum mean cross-validated error
abline(h= lassocv$cvm[small.lambda.index])
#Vertical line: Optimum lambda
abline(v=-log(lassocv$lambda[small.lambda.index]))

lassocv$lambda.min
coef(lassocv)



#-------------------------------------------------------------------------------
#                             Seleccion del mejor modelo
#-------------------------------------------------------------------------------


#modelos


modelo_1=modelo_lineal #modelo completo
modelo_2=lm(Rendimiento~.-Broca-Roya-Temp-RH-Dt,data=data_name)  #modelo restringido por lasso

#modificacion de los datos para añadir el termico cuadratico
data_new=data_name      #buffer
data_new$Temp_sqr=(data_new$Temp)^2         #adicion del termino cuadratico de la temperatura
modelo_3=lm(Rendimiento~.-Broca-Roya-Temp,data=data_new)      #modelo con el termino cuadratico de la temperatura

#Resumen de los modelos propuestos
summary(modelo_1)
summary(modelo_2)
summary(modelo_3)


#division de los datos para el entrenamiento y testeo

#metodo del conjunto de validacion: 80% train y 20% test
data_folds1=mc_cv(data_name,prop=0.8,times=1)

#metodo leave one out cross validation (LOOCV)
loo=rsample::loo_cv(data_name)
data_folds2=rsample::manual_rset(loo$splits,loo$id)

#metodo k folds
data_folds3=vfold_cv(data_name,v=10,repeats=1)


#se especifica el tipo de modelo
lm_model=linear_reg()%>%set_engine("lm")


#Criterios de seleccion de modelo
modelos=list(
  model_1=Rendimiento~.-Broca-Roya,              #modelo completo
  model_2=Rendimiento~.-Broca-Roya-Temp-RH-Dt,   #modelo lasso
  model_3=Rendimiento~.-Broca-Roya-Temp          #modelo Temp_sqrt
)


#se combinan las formulas de los diferentes modelos
lm_models<-workflow_set(
  preproc = modelos,
  models = list(lm_model)
)

#definimos el metodo de validacion a emplear
cv_results = lm_models%>%workflow_map(fn="fit_resamples",
                                      resamples=data_folds2,
                                      metrics=metric_set(rmse),
                                      control=control_resamples(save_pred=TRUE))


modelo_performance=collect_metrics(
  cv_results,summarize = FALSE)%>%
  group_by(wflow_id)%>%
  transmute(modelo = wflow_id,
            RMSE=.estimate, MSE=.estimate^2) %>%
  arrange(MSE)


#Resultado
print(modelo_performance)


#ajustamos el modelo seleccionado con todos los datos de entrenamiento
modelo_final<-extract_workflow(
  cv_results,id=modelo_performance$modelo[1])%>%
  fit(data=data_name)
