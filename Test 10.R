library(lubridate) # para manejar fechas
library(dplyr) # acomodo de fechas
library(e1071) # metodo de Bayes
library(nnet) 
library(NeuralNetTools) # graficar las redes neuronales

#### Datos del transformador bueno ####

### Temperatura ### 

# Lectura del archivo 

# Se lee el archivo CSV de la temperatura

temp_data_bueno <-read.csv("TFBBT-10.csv",sep = ",",dec = ".",header=T,fileEncoding="UTF-8-BOM")

# Las dos primeras columnas son inútiles porque son identificadores, por lo tanto las vamos a eliminar

temp_data_bueno <- temp_data_bueno[,-(1:2)]

#Nombrando las columnas de los datos de temperatura

colnames(temp_data_bueno) <- c("Fecha","Hora","TNucleoSup","TDevPrim","TDevSec","Tambiente")

# Pasando fecha a formato de date

temp_data_bueno$Fecha <- as.Date(temp_data_bueno$Fecha, format = "%m/%d/%Y")

# Transformando la columna de horas a formato de lubridate

temp_data_bueno$Hora <- hms(temp_data_bueno$Hora)


### Datos eléctricos ###

# Leyendo el archivo de datos extraido del Sonel, se eliminan las primeras 16 líneas porque son informativos del equipo

edata_bueno <-read.csv("TFBB-10.csv",sep = ";",dec = ",",header=T,skip =16)

# Las 7 primeras columnas son datos vacíos, por lo tanto se eliminan

edata_bueno <- edata_bueno[,-(1:7)]

#Eliminando columnas de variables que no se monitorean

edata_bueno <- edata_bueno[,-(6:7)]

################# Eliminando la columna de tensión de neutro

edata_bueno <- edata_bueno[,-(5)]

################# Eliminando columnas que no se van a usar:

edata_bueno <- edata_bueno[,-(7:588)]

#Nombrando columnas variables eléctricas

colnames(edata_bueno)<- c("Fecha","Hora","Frecuencia","TVSec(V)","ISec(A)","Iprim(A)")

# Eliminando strings que tengan el texto ---

edata_bueno <- edata_bueno[!grepl('---', edata_bueno$Fecha),]
edata_bueno <- edata_bueno[!grepl('---', edata_bueno$Hora),]
edata_bueno <- edata_bueno[!grepl('---', edata_bueno$Frecuencia),]
edata_bueno <- edata_bueno[!grepl('---', edata_bueno$`TVSec(V)`),]
edata_bueno <- edata_bueno[!grepl('---', edata_bueno$`ISec(A)`),]
edata_bueno <- edata_bueno[!grepl('---', edata_bueno$`Iprim(A)`),]

# Pasando fecha a formato de date

edata_bueno$Fecha <- as.Date(edata_bueno$Fecha, format = "%Y-%m-%d")

# Pasando frecuencia a un valor numérico

edata_bueno$Frecuencia <- as.numeric(gsub(",", ".", edata_bueno$Frecuencia))

# Quitando los milisegundos de la columna Hora

edata_bueno$Hora <- substr(edata_bueno$Hora,1,nchar(edata_bueno$Hora)-6)

# Pasando hora a una formato de tiempo

edata_bueno$Hora <- hms(edata_bueno$Hora)

## Asociación datos eléctricos y térmicos ##

DF_TBueno <- left_join(edata_bueno, temp_data_bueno, by = join_by(Fecha == Fecha,closest(Hora >= Hora)))

# Eliminando la columna duplicada de hora

DF_TBueno <- DF_TBueno[,-(7)]

# Cambiando el nombre a la variable Hora que quedó

names(DF_TBueno)[names(DF_TBueno) == 'Hora.x'] <- 'Hora'


########################### Leyendo archivo de tensión de entrada

Tension_Entrada <-read.csv("Tin-01.csv",sep = ";",dec = ",",check.names = F)

#Eliminando columnas de variables que no se monitorean

Tension_Entrada <- Tension_Entrada[-c(1,4,6)]

#Nombrando columnas variables eléctricas

colnames(Tension_Entrada)<- c("Fecha","Hora","V_In")

# Actualizando fecha
Tension_Entrada$Fecha <- "21/6/24"

# Pasando fecha a formato de date

Tension_Entrada$Fecha <- as.Date(Tension_Entrada$Fecha, format = "%d/%m/%y")

# Pasando hora a una formato de tiempo

Tension_Entrada$Hora <- hms(Tension_Entrada$Hora)

## Asociación datos eléctricos y térmicos  con tensión de entrada##

DF_TBueno <- left_join(DF_TBueno, Tension_Entrada, by = join_by(Fecha == Fecha,closest(Hora <= Hora)))

# Eliminando la columna duplicada de hora

DF_TBueno <- subset(DF_TBueno, select = -c(Hora.y))

# Cambiando el nombre a la variable Hora que quedó

names(DF_TBueno)[names(DF_TBueno) == 'Hora.x'] <- 'Hora'

# Agregando la columna de identificador de bueno

DF_TBueno$Estado <- "Bueno"






##### Datos del transformador malo #### 


### Temperatura ### 

# Lectura del archivo

# Se lee el archivo CSV de la temperatura

temp_data_malo <-read.csv("TFBMAT-10.csv",sep = ",",dec = ".",header=T,fileEncoding="UTF-8-BOM")

# Las dos primeras columnas son inútiles porque son identificadores, por lo tanto las vamos a eliminar

temp_data_malo <- temp_data_malo[,-(1:2)]

#Nombrando las columnas de los datos de temperatura

colnames(temp_data_malo) <- c("Fecha","Hora","TNucleoSup","TDevPrim","TDevSec","Tambiente")

# Pasando fecha a formato de date

temp_data_malo$Fecha <- as.Date(temp_data_malo$Fecha, format = "%m/%d/%Y")

# Transformando la columna de horas a formato de lubridate

temp_data_malo$Hora <- hms(temp_data_malo$Hora)


### Datos eléctricos ###

# Leyendo el archivo de datos extraido del Sonel, se eliminan las primeras 16 líneas porque son informativos del equipo

edata_malo <-read.csv("TFBMA-10.csv",sep = ";",dec = ",",header=T,skip =16)

# Las 7 primeras columnas son datos vacíos, por lo tanto se eliminan

edata_malo <- edata_malo[,-(1:7)]

#Eliminando columnas de variables que no se monitorean

#edata_malo <- edata_malo[,-(6:7)]

################# Eliminando la columna de tensión de neutro

edata_malo <- edata_malo[,-(5)]

#Nombrando columnas variables eléctricas

colnames(edata_malo)<- c("Fecha","Hora","Frecuencia","TVSec(V)","ISec(A)","Iprim(A)")

# Eliminando strings que tengan el texto ---

edata_malo <- edata_malo[!grepl('---', edata_malo$Fecha),]
edata_malo <- edata_malo[!grepl('---', edata_malo$Hora),]
edata_malo <- edata_malo[!grepl('---', edata_malo$Frecuencia),]
edata_malo <- edata_malo[!grepl('---', edata_malo$`TVSec(V)`),]
edata_malo <- edata_malo[!grepl('---', edata_malo$`ISec(A)`),]
edata_malo <- edata_malo[!grepl('---', edata_malo$`Iprim(A)`),]

# Pasando fecha a formato de date

edata_malo$Fecha <- as.Date(edata_malo$Fecha, format = "%Y-%m-%d")

# Pasando frecuencia a un valor numérico

edata_malo$Frecuencia <- as.numeric(gsub(",", ".", edata_malo$Frecuencia))

# Quitando los milisegundos  de la columna Hora

edata_malo$Hora <- substr(edata_malo$Hora,1,nchar(edata_malo$Hora)-6)

# Pasando hora a una formato de tiempo

edata_malo$Hora <- hms(edata_malo$Hora)


## Asociación datos eléctricos y térmicos ##

DF_TMalo <- left_join(edata_malo, temp_data_malo, by = join_by(Fecha == Fecha,closest(Hora >= Hora)))

# Eliminando la columna duplicada de hora

DF_TMalo <- subset(DF_TMalo, select = -c(Hora.y))

# Cambiando el nombre a la variable Hora que quedó

names(DF_TMalo)[names(DF_TMalo) == 'Hora.x'] <- 'Hora'


########################### Leyendo archivo de tensión de entrada

Tension_Entrada_Malo <-read.csv("Tin-01.csv",sep = ";",dec = ",",check.names = F)

#Eliminando columnas de variables que no se monitorean

Tension_Entrada_Malo <- Tension_Entrada_Malo[-c(1,4,6)]

#Nombrando columnas variables eléctricas

colnames(Tension_Entrada_Malo)<- c("Fecha","Hora","V_In")

# Actualizando fecha
Tension_Entrada_Malo$Fecha <- "21/6/24"

# Pasando fecha a formato de date

Tension_Entrada_Malo$Fecha <- as.Date(Tension_Entrada_Malo$Fecha, format = "%d/%m/%y")

# Pasando hora a una formato de tiempo

Tension_Entrada_Malo$Hora <- hms(Tension_Entrada_Malo$Hora)

## Asociación datos eléctricos y térmicos  con tensión de entrada##

DF_TMalo <- left_join(DF_TMalo, Tension_Entrada_Malo, by = join_by(Fecha == Fecha,closest(Hora <= Hora)))

# Eliminando la columna duplicada de hora

DF_TMalo <- subset(DF_TMalo, select = -c(Hora.y))

# Cambiando el nombre a la variable Hora que quedó

names(DF_TMalo)[names(DF_TMalo) == 'Hora.x'] <- 'Hora'

# Agregando la columna de identificador de bueno

DF_TMalo$Estado <- "Malo"





################################# Uniendo los dataframes malo y bueno #########################################

DF_TIAislamiento <- rbind(DF_TBueno,DF_TMalo)

# Pasando el identificador bueno o malo a factor

DF_TIAislamiento$Estado <- as.factor(DF_TIAislamiento$Estado)

#Quitando fecha y hora del DF porque eso no debería tomarse en cuenta en el algoritmo

DF_TIAislamiento <- DF_TIAislamiento[,-(1:2)]

ret <- write.csv(x=DF_TIAislamiento, file="borrar.csv")

###################################### Algoritmos ############################################################

##### Obteniendo las tablas de aprendizaje y testing

# Obteniendo tama?o para tablas de testing y training
tam<-dim(DF_TIAislamiento)

# obteniendo el tamaño de la tabla
n<-tam[1]

# Creando tablas de testing y training 

#variable de muestra de 30% de n y floor se utiliza para redondear el número
muestra <- sample(1:n,floor(n*0.30))

#tabla de testing del tamaño de la muestra
ttesting <- DF_TIAislamiento[muestra,]

#tabla de aprendizaje del resto
taprendizaje <- DF_TIAislamiento[-muestra,]


######## Algoritmo de redes neuronales

# size = numero de nodos de las capa oculta
# rang = pesos iniciales
# decay = grado de decrecimiento de los pesos
# maxit = numero maximo de iteraciones (default=100)
# MaxNWts = Numero maximo de pesos (default=1000)

mod_nnet<-nnet(Estado~.,data=taprendizaje,size = 10,maxit = 1000,MaxNWts=5500,trace=FALSE, decay=0.1)

# Type="class" hace que el modelo prediga clases y no valores de Regresion

pred_nnet<-as.factor(predict(mod_nnet, ttesting[,-10],type = "class"))



real <- factor(ttesting[,10],levels = c("Bueno","Malo"))
pred_nnet <- factor(pred_nnet,levels = c("Bueno","Malo"))


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
                  "Precisi?n Positiva","Precision Negativa","Falsos Positivos","Falsos Negativos",
                  "Asertividad Positiva", "Asertividad Negativa")
  return(res)
}


indice_NNET <- indices.general(MC_nnet)





####### Correlaciones

# le quitamos la hora que no debería afectar
DF_cor <- DF_TIAislamiento


# ahora pasamos a factor la última columna
DF_cor$Estado <- as.factor(DF_cor$Estado)

DF_cor$Estado <- as.numeric(DF_cor$Estado)

correlation <-  round(cor(DF_cor), digits = 3)













