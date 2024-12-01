library(lubridate) # para manejar fechas
library(dplyr) # acomodo de fechas
library(nnet) # redes neuronales
library(NeuralNetTools) # graficar las redes neuronales

################### Datos del transformador sin huecos ################### 

### Temperatura TF sin huecos### 

# Lectura del archivo 

# Se lee el archivo CSV de la temperatura

temp_data_0H <-read.csv("TF0HT.csv",sep = ",",dec = ".",header=T,fileEncoding="UTF-8-BOM")

# Las dos primeras columnas son inútiles porque son identificadores, por lo tanto las vamos a eliminar

temp_data_0H <- temp_data_0H[,-(1:2)]

# Nombrando las columnas de los datos de temperatura

colnames(temp_data_0H) <- c("Fecha","Hora","TNucleoSup","TDevPrim","TDevSec","Tambiente")

# Pasando fecha a formato de date

temp_data_0H$Fecha <- as.Date(temp_data_0H$Fecha, format = "%m/%d/%Y")

# Transformando la columna de horas a formato de lubridate

temp_data_0H$Hora <- hms(temp_data_0H$Hora)


### Datos eléctricos TF sin huecos ###

# Leyendo el archivo de datos extraido del Sonel, se eliminan las primeras 16 líneas porque son informativos del equipo

edata_0H <-read.csv("TF0H.csv",sep = ";",dec = ",",header=T,skip =16)

# Las 7 primeras columnas son datos vacíos, por lo tanto se eliminan

edata_0H <- edata_0H[,-(1:7)]

# La columna de "U.N.PE.avg..3.s..V." porque es un valor que el equipo calcula incorrectamente y no se va a utilizar

edata_0H <- subset(edata_0H, select=-c(U.N.PE.avg..3.s..V.))

#Nombrando columnas variables eléctricas - revisar siempre nombres

colnames(edata_0H)<- c("Fecha","Hora","Frecuencia","TVSec(V)","IPrim(A)","ISec(A)")

# Eliminando strings que tengan el texto ---

edata_0H <- edata_0H[!grepl('---', edata_0H$Fecha),]
edata_0H <- edata_0H[!grepl('---', edata_0H$Hora),]
edata_0H <- edata_0H[!grepl('---', edata_0H$Frecuencia),]
edata_0H <- edata_0H[!grepl('---', edata_0H$`TVSec(V)`),]
edata_0H <- edata_0H[!grepl('---', edata_0H$`ISec(A)`),]
edata_0H <- edata_0H[!grepl('---', edata_0H$`IPrim(A)`),]

# Pasando fecha a formato de date

edata_0H$Fecha <- as.Date(edata_0H$Fecha, format = "%Y-%m-%d")

# Pasando frecuencia a un valor numérico

edata_0H$Frecuencia <- as.numeric(gsub(",", ".", edata_0H$Frecuencia))

# Quitando los milisegundos de la columna Hora

edata_0H$Hora <- substr(edata_0H$Hora,1,nchar(edata_0H$Hora)-6)

# Pasando hora a una formato de tiempo

edata_0H$Hora <- hms(edata_0H$Hora)

## Asociación datos eléctricos y térmicos ##

DF_T0H <- left_join(edata_0H, temp_data_0H, by = join_by(Fecha == Fecha,closest(Hora >= Hora)))



# Eliminando la columna duplicada de hora

DF_T0H <- subset(DF_T0H, select=-c(Hora.y))

# Cambiando el nombre a la variable Hora que quedó

names(DF_T0H)[names(DF_T0H) == 'Hora.x'] <- 'Hora'

# Colocando el estado

DF_T0H$Estado <- "NH"


################### Datos del transformador con 1 hueco ###################  

### Temperatura TF 1 hueco ### 

# Lectura del archivo 

# Se lee el archivo CSV de la temperatura

temp_data_1H <-read.csv("TF1HT.csv",sep = ",",dec = ".",header=T,fileEncoding="UTF-8-BOM")

# Las dos primeras columnas son inútiles porque son identificadores, por lo tanto las vamos a eliminar

temp_data_1H <- temp_data_1H[,-(1:2)]

# Nombrando las columnas de los datos de temperatura

colnames(temp_data_1H) <- c("Fecha","Hora","TNucleoSup","TDevPrim","TDevSec","Tambiente")

# Pasando fecha a formato de date

temp_data_1H$Fecha <- as.Date(temp_data_1H$Fecha, format = "%m/%d/%Y")

# Transformando la columna de horas a formato de lubridate

temp_data_1H$Hora <- hms(temp_data_1H$Hora)


### Datos eléctricos TF 1 Hueco ###

# Leyendo el archivo de datos extraido del Sonel, se eliminan las primeras 16 líneas porque son informativos del equipo

edata_1H <-read.csv("TF1H.csv",sep = ";",dec = ",",header=T,skip =16)

# Las 7 primeras columnas son datos vacíos, por lo tanto se eliminan

edata_1H <- edata_1H[,-(1:7)]


#Nombrando columnas variables eléctricas

colnames(edata_1H)<- c("Fecha","Hora","Frecuencia","TVSec(V)","IPrim(A)","ISec(A)")

# Eliminando strings que tengan el texto ---

edata_1H <- edata_1H[!grepl('---', edata_1H$Fecha),]
edata_1H <- edata_1H[!grepl('---', edata_1H$Hora),]
edata_1H <- edata_1H[!grepl('---', edata_1H$Frecuencia),]
edata_1H <- edata_1H[!grepl('---', edata_1H$`TVSec(V)`),]
edata_1H <- edata_1H[!grepl('---', edata_1H$`IPrim(A)`),]
edata_1H <- edata_1H[!grepl('---', edata_1H$`ISec(A)`),]

# Pasando fecha a formato de date

edata_1H$Fecha <- as.Date(edata_1H$Fecha, format = "%Y-%m-%d")

# Pasando frecuencia a un valor numérico

edata_1H$Frecuencia <- as.numeric(gsub(",", ".", edata_1H$Frecuencia))

# Quitando los milisegundos  de la columna Hora

edata_1H$Hora <- substr(edata_1H$Hora,1,nchar(edata_1H$Hora)-6)

# Pasando hora a una formato de tiempo

edata_1H$Hora <- hms(edata_1H$Hora)


## Asociación datos eléctricos y térmicos ##

DF_T1H <- left_join(edata_1H, temp_data_1H, by = join_by(Fecha == Fecha,closest(Hora >= Hora)))

# Eliminando la columna duplicada de hora

DF_T1H <- subset(DF_T1H, select = -c(Hora.y))

# Cambiando el nombre a la variable Hora que quedó

names(DF_T1H)[names(DF_T1H) == 'Hora.x'] <- 'Hora'

# Colocando el estado de "con hueco"

DF_T1H$Estado <- "CH"



################### Datos del transformador con 2 huecos ###################  

### Temperatura TF 2 huecos ### 

# Lectura del archivo 

# Se lee el archivo CSV de la temperatura

temp_data_2H <-read.csv("TF2HT.csv",sep = ",",dec = ".",header=T,fileEncoding="UTF-8-BOM")

# Las dos primeras columnas son inútiles porque son identificadores, por lo tanto las vamos a eliminar

temp_data_2H <- temp_data_2H[,-(1:2)]

# Nombrando las columnas de los datos de temperatura

colnames(temp_data_2H) <- c("Fecha","Hora","TNucleoSup","TDevPrim","TDevSec","Tambiente")

# Pasando fecha a formato de date

temp_data_2H$Fecha <- as.Date(temp_data_2H$Fecha, format = "%m/%d/%Y")

# Transformando la columna de horas a formato de lubridate

temp_data_2H$Hora <- hms(temp_data_2H$Hora)


### Datos eléctricos TF 2 Huecos ###

# Leyendo el archivo de datos extraido del Sonel, se eliminan las primeras 16 líneas porque son informativos del equipo

edata_2H <-read.csv("TF2H.csv",sep = ";",dec = ",",header=T,skip =16)

# Las 7 primeras columnas son datos vacíos, por lo tanto se eliminan

edata_2H <- edata_2H[,-(1:7)]

#Nombrando columnas variables eléctricas

colnames(edata_2H)<- c("Fecha","Hora","Frecuencia","TVSec(V)","IPrim(A)","ISec(A)")

# Eliminando strings que tengan el texto ---

edata_2H <- edata_2H[!grepl('---', edata_2H$Fecha),]
edata_2H <- edata_2H[!grepl('---', edata_2H$Hora),]
edata_2H <- edata_2H[!grepl('---', edata_2H$Frecuencia),]
edata_2H <- edata_2H[!grepl('---', edata_2H$`TVSec(V)`),]
edata_2H <- edata_2H[!grepl('---', edata_2H$`IPrim(A)`),]
edata_2H <- edata_2H[!grepl('---', edata_2H$`ISec(A)`),]

# Pasando fecha a formato de date

edata_2H$Fecha <- as.Date(edata_2H$Fecha, format = "%Y-%m-%d")

# Pasando frecuencia a un valor numérico

edata_2H$Frecuencia <- as.numeric(gsub(",", ".", edata_2H$Frecuencia))

# Quitando los milisegundos  de la columna Hora

edata_2H$Hora <- substr(edata_2H$Hora,1,nchar(edata_2H$Hora)-6)

# Pasando hora a una formato de tiempo

edata_2H$Hora <- hms(edata_2H$Hora)


## Asociación datos eléctricos y térmicos ## se cambia la dirección del signo para asociar mejor las fechas##

DF_T2H <- left_join(edata_2H, temp_data_2H, by = join_by(Fecha == Fecha,closest(Hora <= Hora)))

# Eliminando la columna duplicada de hora

DF_T2H <- subset(DF_T2H, select = -c(Hora.y))

# Cambiando el nombre a la variable Hora que quedó

names(DF_T2H)[names(DF_T2H) == 'Hora.x'] <- 'Hora'

# Colocando el estado de "con hueco"

DF_T2H$Estado <- "CH"




################### Datos del transformador con 3 huecos ###################  

### Temperatura TF 3 huecos ### 

# Lectura del archivo 

# Se lee el archivo CSV de la temperatura

temp_data_3H <-read.csv("TF3HT.csv",sep = ",",dec = ".",header=T,fileEncoding="UTF-8-BOM")

# Las dos primeras columnas son inútiles porque son identificadores, por lo tanto las vamos a eliminar

temp_data_3H <- temp_data_3H[,-(1:2)]

# Nombrando las columnas de los datos de temperatura

colnames(temp_data_3H) <- c("Fecha","Hora","TNucleoSup","TDevPrim","TDevSec","Tambiente")

# Pasando fecha a formato de date

temp_data_3H$Fecha <- as.Date(temp_data_3H$Fecha, format = "%m/%d/%Y")

# Transformando la columna de horas a formato de lubridate

temp_data_3H$Hora <- hms(temp_data_3H$Hora)


### Datos eléctricos TF 3 Huecos ###

# Leyendo el archivo de datos extraido del Sonel, se eliminan las primeras 16 líneas porque son informativos del equipo

edata_3H <-read.csv("TF3H.csv",sep = ";",dec = ",",header=T,skip =16)

# Las 7 primeras columnas son datos vacíos, por lo tanto se eliminan

edata_3H <- edata_3H[,-(1:7)]

#Nombrando columnas variables eléctricas

colnames(edata_3H)<- c("Fecha","Hora","Frecuencia","TVSec(V)","IPrim(A)","ISec(A)")

# Eliminando strings que tengan el texto ---

edata_3H <- edata_3H[!grepl('---', edata_3H$Fecha),]
edata_3H <- edata_3H[!grepl('---', edata_3H$Hora),]
edata_3H <- edata_3H[!grepl('---', edata_3H$Frecuencia),]
edata_3H <- edata_3H[!grepl('---', edata_3H$`TVSec(V)`),]
edata_3H <- edata_3H[!grepl('---', edata_3H$`IPrim(A)`),]
edata_3H <- edata_3H[!grepl('---', edata_3H$`ISec(A)`),]

# Pasando fecha a formato de date

edata_3H$Fecha <- as.Date(edata_3H$Fecha, format = "%Y-%m-%d")

# Pasando frecuencia a un valor numérico

edata_3H$Frecuencia <- as.numeric(gsub(",", ".", edata_3H$Frecuencia))

# Quitando los milisegundos  de la columna Hora

edata_3H$Hora <- substr(edata_3H$Hora,1,nchar(edata_3H$Hora)-6)

# Pasando hora a una formato de tiempo

edata_3H$Hora <- hms(edata_3H$Hora)


## Asociación datos eléctricos y térmicos ## se cambia la dirección del signo para asociar mejor las fechas##

DF_T3H <- left_join(edata_3H, temp_data_3H, by = join_by(Fecha == Fecha,closest(Hora <= Hora)))

# Eliminando la columna duplicada de hora

DF_T3H <- subset(DF_T3H, select = -c(Hora.y))

# Cambiando el nombre a la variable Hora que quedó

names(DF_T3H)[names(DF_T3H) == 'Hora.x'] <- 'Hora'

# Colocando el estado de "con hueco"

DF_T3H$Estado <- "CH"





################### Datos del transformador con 4 huecos ###################  


### Temperatura TF 4 huecos ### 

# Lectura del archivo 

# Se lee el archivo CSV de la temperatura

temp_data_4H <-read.csv("TF4HT.csv",sep = ",",dec = ".",header=T,fileEncoding="UTF-8-BOM")

# Las dos primeras columnas son inútiles porque son identificadores, por lo tanto las vamos a eliminar

temp_data_4H <- temp_data_4H[,-(1:2)]

# Nombrando las columnas de los datos de temperatura

colnames(temp_data_4H) <- c("Fecha","Hora","TNucleoSup","TDevPrim","TDevSec","Tambiente")

# Pasando fecha a formato de date

temp_data_4H$Fecha <- as.Date(temp_data_4H$Fecha, format = "%m/%d/%Y")

# Transformando la columna de horas a formato de lubridate

temp_data_4H$Hora <- hms(temp_data_4H$Hora)


### Datos eléctricos TF 4 Huecos ###

# Leyendo el archivo de datos extraido del Sonel, se eliminan las primeras 16 líneas porque son informativos del equipo

edata_4H <-read.csv("TF4H.csv",sep = ";",dec = ",",header=T,skip =16)

# Las 7 primeras columnas son datos vacíos, por lo tanto se eliminan

edata_4H <- edata_4H[,-(1:7)]

#Nombrando columnas variables eléctricas

colnames(edata_4H)<- c("Fecha","Hora","Frecuencia","TVSec(V)","IPrim(A)","ISec(A)")

# Eliminando strings que tengan el texto ---

edata_4H <- edata_4H[!grepl('---', edata_4H$Fecha),]
edata_4H <- edata_4H[!grepl('---', edata_4H$Hora),]
edata_4H <- edata_4H[!grepl('---', edata_4H$Frecuencia),]
edata_4H <- edata_4H[!grepl('---', edata_4H$`TVSec(V)`),]
edata_4H <- edata_4H[!grepl('---', edata_4H$`IPrim(A)`),]
edata_4H <- edata_4H[!grepl('---', edata_4H$`ISec(A)`),]

# Pasando fecha a formato de date

edata_4H$Fecha <- as.Date(edata_4H$Fecha, format = "%Y-%m-%d")

# Pasando frecuencia a un valor numérico

edata_4H$Frecuencia <- as.numeric(gsub(",", ".", edata_4H$Frecuencia))

# Quitando los milisegundos  de la columna Hora

edata_4H$Hora <- substr(edata_4H$Hora,1,nchar(edata_4H$Hora)-6)

# Pasando hora a una formato de tiempo

edata_4H$Hora <- hms(edata_4H$Hora)


## Asociación datos eléctricos y térmicos ## se cambia la dirección del signo para asociar mejor las fechas##

DF_T4H <- left_join(edata_4H, temp_data_4H, by = join_by(Fecha == Fecha,closest(Hora <= Hora)))

# Eliminando la columna duplicada de hora

DF_T4H <- subset(DF_T4H, select = -c(Hora.y))

# Cambiando el nombre a la variable Hora que quedó

names(DF_T4H)[names(DF_T4H) == 'Hora.x'] <- 'Hora'

# Colocando el estado de "con hueco"

DF_T4H$Estado <- "CH"





################################# Uniendo los dataframes sin hueco y con huecos #########################################

DF_TH<- rbind(DF_T0H,DF_T1H,DF_T2H,DF_T3H,DF_T4H)

# Pasando todo a tipo de variable numeral

DF_TH$`TVSec(V)` <- as.numeric(DF_TH$`TVSec(V)`)
DF_TH$`IPrim(A)` <- as.numeric(DF_TH$`IPrim(A)`)
DF_TH$`ISec(A)` <- as.numeric(DF_TH$`ISec(A)`)

# Pasando el identificador bueno o malo a factor

DF_TH$Estado <- as.factor(DF_TH$Estado)

#Quitando fecha y hora del DF porque eso no debería tomarse en cuenta en el algoritmo

DF_TH <- DF_TH[,-(1:2)]



###################################### Algoritmos ############################################################

##### Obteniendo las tablas de aprendizaje y testing

# Obteniendo tamano para tablas de testing y training
tam<-dim(DF_TH)

# obteniendo el tamaño de la tabla
n<-tam[1]

# Creando tablas de testing y training 

#variable de muestra de 30 por ciento de n y floor se utiliza para redondear el número
muestra <- sample(1:n,floor(n*0.3))


#tabla de testing del tamaño de la muestra
ttesting <- DF_TH[c(muestra),]

#tabla de aprendizaje del resto
taprendizaje <- DF_TH[-c(muestra),]


######## Algoritmo de redes neuronales

# size = numero de nodos de las capa oculta
# rang = pesos iniciales
# decay = grado de decrecimiento de los pesos
# maxit = numero maximo de iteraciones (default=100)
# MaxNWts = Numero maximo de pesos (default=1000)


mod_nnet<-nnet(Estado~.,data=taprendizaje,size = 10,maxit = 1000, decay=0.1)


# Type="class" hace que el modelo prediga clases y no valores de Regresion

pred_nnet<-predict(mod_nnet, ttesting[,-9],type = "class")

# Convirtiendo los valores a factor
real <- factor(ttesting[,9],levels = c("NH","CH"))
pred_nnet <- factor(pred_nnet,levels = c("NH","CH"))


# Matriz de Confusion
MC_nnet<-table(real,pred_nnet)


#Graficando la red neuronal
plotnet(mod_nnet)

# Graficando las variables más importantes

olden(mod_nnet)



############## Indices de resultado de algoritmo

# Se hace una función para obtener resultados a partir de matriz de confusión
indices.general <- function(MC) {
  precision.global <- sum(diag(MC))/sum(MC)
  error.global <- 1 - precision.global
  precision.positiva <- MC[1,1]/(MC[1,1]+MC[1,2])
  precision.negativa <- MC[2,2]/(MC[2,2]+MC[2,1])
  falsos.positivos <- MC[2,1]/(MC[2,2]+MC[2,1])
  falsos.negativos <- MC[1,2]/(MC[1,2]+MC[1,1])
  asertividad.positiva <- MC[1,1]/(MC[1,1]+MC[2,1])
  asertividad.negativa <- MC[2,2]/(MC[2,2]+MC[1,2])
  
  res <- list(matriz.confusion = MC, precision.global = precision.global, error.global = error.global, 
              precision.positiva = precision.positiva, precision.negativa = precision.negativa,
              falsos.positivos=falsos.positivos, falsos.negativos=falsos.negativos,
              asertividad.positiva=asertividad.positiva, asertividad.negativa=asertividad.negativa)
  names(res) <- c("Matriz de Confusion", "Precision Global", "Error Global", 
                  "Precision Positiva","Precision Negativa","Falsos Positivos","Falsos Negativos",
                  "Asertividad Positiva", "Asertividad Negativa")
  return(res)
}

indice_NNET <- indices.general(MC_nnet)



########################## Correlaciones ###########################

# Creando un solo dataframe para revisar las correlaciones
DF_cor <- DF_TH

# Pasando los estados a número

DF_cor$Estado <-  as.numeric(gsub("NH", 1, gsub("CH", 2, DF_cor$Estado)))

# Revisando que todos sean números

str(DF_cor)

Table_cor <- round(cor(DF_cor), digits = 3)
 
ret <- write.csv(x=Table_cor, file="borrar.csv")





##################################### Sección sin la temperatura ambiente


# Quitando la variable de Temperatura ambiente 

DF_TH_STA <- subset(DF_TH, select = -c(Tambiente))



# Obteniendo la tablas de muestra y prueba

ttesting_STA <- DF_TH_STA[c(muestra),]

taprendizaje_STA <- DF_TH_STA[-c(muestra),]

# Aplicando el algoritmo

mod_nnet_STA<-nnet(Estado~.,data=taprendizaje_STA,size = 10,maxit = 1000, decay=0.1)


# Type="class" hace que el modelo prediga clases y no valores de Regresion

pred_nnet_STA<-predict(mod_nnet_STA, ttesting_STA[,-8],type = "class")

# Convirtiendo los valores a factor
real_STA <- factor(ttesting_STA[,8],levels = c("NH","CH"))
pred_nnet_STA <- factor(pred_nnet_STA,levels = c("NH","CH"))


# Matriz de Confusion
MC_nnet_STA<-table(real_STA,pred_nnet_STA)


#Graficando la red neuronal
plotnet(mod_nnet_STA)

# Graficando las variables más importantes

olden(mod_nnet_STA)

# Comprobando los índices

indice_NNET_STA <- indices.general(MC_nnet_STA)

# Creando un solo dataframe para revisar las correlaciones
DF_cor_STA <- DF_TH_STA

# Pasando los estados a número

DF_cor_STA$Estado <-  as.numeric(gsub("NH", 1, gsub("CH", 2, DF_cor_STA$Estado)))

# Revisando que todos sean números

Table_cor_STA <- round(cor(DF_cor_STA), digits = 3)

ret <- write.csv(x=Table_cor_STA, file="borrar.csv")
