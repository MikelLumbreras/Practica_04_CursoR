##################
### Curso de Doctorado
### Análisis de datos para cargas energéticas en edificios. Contexto, Métodos de análisis, Herramientas y Aplicaciones
### UPV/EHU, 2021
### Roberto GARAY MARTINEZ, roberto@robertogaray.com / roberto.garay@Tecnalia.com
### Mikel LUMBRERAS MUGAGUREN
##################

##################
### Práctica 04
### Script 2: IDENTIFICACION DE OUTLIERS
##### VAMOS A APRENDER 2 MODOS DE IDENTFICAR OUTLIERS

## LIMPIAMOS EL ENTORNO
rm(list = ls())

### SI NO LO TIENES INSLATALADO
install.packages("dbscan")
library("dbscan")
install.packages("ggplot2")
library("ggplot2")

### CARGAMOS EL ARCHIVO HORARIO DE CON LAS DELTA Ts
setwd("C:/Users/MikelLumbreras/OneDrive - Managing Innovation Strategies (MainStrat)/EHU/Sesion_4/Practica_04_CursoR-main/Data/")

BuildingData <- read.csv("BuildingData.csv" , header = T , sep = ",")
### QUITAMOS LA PRIMERA COLUMNA
BuildingData <- BuildingData[,-1]

### OBJETIVO: IDENTIFICAR POSIBLES OUTLIERS PARA LA DELTA T

### METODO 1: INTERQUARTILE METHOD
OutliersIQR <- function(DatosEdificio)
{
  
  outliers <- boxplot(DatosEdificio$Flow.T.ºC. , plot=FALSE)$out
  if (length(outliers) >= 1)
    Edificio_NO <- DatosEdificio[-which(DatosEdificio$Power.kW. %in% outliers),]
  return(outliers)
}

### METODO 2: DENSITY BASED CLUSTERING
OutliersDBSCAN <- function(DatosEdificio)
{
  FlowTemp <- as.matrix(cbind(DatosEdificio$Temperature, DatosEdificio$Flow.T.ºC.)) 
  ### FUNCION PRINCIPAL PARA QUITAR OUTLIERS
  dim <- 2 ###  Q-T 
  dbscanResult <- fpc::dbscan(FlowTemp, eps = 1, MinPts = 3)
  ### PARA CALCULAR CUAL ES EL NUMERO BUENO
  knee <- dbscan::kNNdist(FlowTemp, k =  3)
  #dev.new()
  knee <- sort(sort(knee, decreasing = F))
  ### AHORA DEBEMOS CALCULAR DONDE ESTA EL CODO
  get.elbow.points.indices <- function(x, y, threshold)
  {
    d1 <- diff(y) / diff(x) # first derivative
    d2 <- diff(d1) / diff(x[-1]) # second derivative
    indices <- which(abs(d2) > threshold)  
    return(indices)
  }
  # first approximate the function, since we have only a few points
  indices <- get.elbow.points.indices(c(1:length(knee)), knee, 0.01) # threshold for huge jump = 10
  dbscan::kNNdistplot(FlowTemp, k =  3)
  abline(h = knee[indices[1]], lty = 2)
  #dev.off()
  ### FRAME PARA DIBUJAR
  FrameNN <- cbind.data.frame(c(1:length(knee)),knee)
  #png(paste(paste("Building" , IDBuilding ,sep = ""), "_2.png" , sep = "") , height = 720 , width = 720 , units = "px")
  plot(FrameNN , type = "l"  , yaxt = "none" , xaxt = "none" , xlab = "" , ylab = "")
  axis(2,seq(0,4,1) ,las=2, font=2 , cex.axis=1.5)
  axis(1, seq(0,9000,1000),las=2, font=2 , cex.axis=1.5)
  mtext(side=1, line=3.8, "Points (sample) sorted by distance", font=2,cex=1.5)
  mtext(side=2, line=3, "3-NN DISTANCE", font=2, cex=1.5)
  abline(h = knee[indices[1]], lty = 2)
  dev.off()
  #### ENTONCES LA EPSILON OPTIMIZADA ES LA SIGUIENTE
  EpsOpt <- knee[indices[1]]
  #### VOLVEMOS A HACER LA CLUSTERIZACION
  dbscanResultOpt <- fpc::dbscan(FlowTemp, eps = EpsOpt, MinPts = 3)
  #dev.new()
  #plot(dbscanResultOpt, Load, main = "DBSCAN", frame = FALSE)
  DatosEdificio_NO <- DatosEdificio[which(dbscanResultOpt[["isseed"]] == T), ]
  OutliersBuilding <- DatosEdificio[which(dbscanResultOpt[["isseed"]] == F),]
  #### VAMOS A DIBUJAR LOS OUTLIERS
  OutlierPlot <- data.frame(matrix(ncol = 3 , nrow = length(DatosEdificio[,1])))
  colnames(OutlierPlot) <- c("T[ºC]" , "kWh" , "OutlierDet")
  OutlierPlot[,1:2] <- FlowTemp
  OutlierPlot[,3] <- dbscanResult[["isseed"]]
  #### EL GGPLOT --------------------------------------------------------------------------------------------
  dev.new()
  ggplot(data = OutlierPlot , aes(x = `T[ºC]` , y = kWh , colour = OutlierDet)) + geom_point(size = 1.5) + 
    scale_color_brewer(palette = "Set1") + 
    theme(axis.title = element_text(size = 16 , face = "bold") , axis.text = element_text(size = 16 , face = "bold")) + 
    theme(legend.title = element_text(size = 16 , face = "bold") , legend.text = element_text(size = 16, face = "bold")) + 
    guides(colour = guide_legend(override.aes = list(size=10))) + ggtitle("Outliers Flow T") + 
    theme(title = element_text(size = 16 , face = "bold"))
  return(OutliersBuilding)
}


#### FALTARIA COMPARAR LOS OUTLIERS IDENTIFICADOS --------------------------------------------------------------------------
### COGER DOS VECTORES Y JUNTARLOS EN UN FRAME Y COMPARAR
Outliers_IQR <- OutliersIQR(BuildingData)
Outliers_DBSCAN <- OutliersDBSCAN(BuildingData)

### COMPARAMOS USANDO BOXPLOTS
dev.new()
boxplot(Outliers_IQR , Outliers_DBSCAN$Power.kW.)




