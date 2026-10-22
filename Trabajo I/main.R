#---------------------------------------------------------------------
#---------------------------------------------------------------------
#                                  Analisis
#                                 de los datos
#---------------------------------------------------------------------
#---------------------------------------------------------------------

#librerias para la implementacion 
library(car)
library(klaR)

path="C:/Users/diego/Documents/GitHub/Estadistica-II/Trabajo I/Datos/Base04.csv"
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
modelo_lineal=lm(Rendimiento~.-Broca-Roya-V-Temp-Alti-RH,data=data_name)

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


# numero condicion (Kappa)
#kappa = lambda_max/lambda_min
#------------------------------------
#sqrt(K)<=10 No hay multicolinealidad
#sqrt(K)<=31.62 multicolin moderada
#sqrt(K)>31.62 multicolin grave
#------------------------------------
kappa(modelo_lineal)


#Indice de condicion
#kappa_i =lambda_max/lambda_i
#-------------------------------------
#sqrt(K)<=10 No hay multicolinealidad
#sqrt(K)<=31.62 multicolin moderada
#sqrt(K)>31.62 multicolin grave
#-------------------------------------
cond.index(Rendimiento~.-Broca-Roya-V-Temp-Alti-RH,data=data_name)





