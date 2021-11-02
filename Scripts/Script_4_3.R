### SCRIPT 4.3: CLUSTERIZACION NO-SUPERVISADA: K-MEANS

#### LIMPIAMOS EL ENTORNO POR SI ACASO
rm(list = ls())

#### AHORA VAMOS A VER LAS LIBRERIAS QUE VAMOS A CARGAR E INSTALAR
## ESTA LIBRERIA CONTIENE EL ALGORITMO DE K-MEANS
install.packages("stats")
library("stats")
## ESTA LIBRERIA TRAE INCLUIDOS LOS CVI-S QUE NOS INTERESAN
install.packages("clusterCrit")
library("clusterCrit")

## PRIMER PASO: CARGAMOS LOS DATOS EN BRUTO, LES CAMBIAMOS EL LA FORMA
setwd("C:/Users/mlumbreras001/OneDrive/Tecnalia/ProyectosR/Practica_04_CursoR/Data")
BuildingData <- read.csv(file = "BuildingData.csv" , header = T , sep = ",")
BuildingData <- BuildingData[,-1]

#### ESTA FUNCION LA PODEIS COPIAR Y PEGAR, NO PERDAIS MAS TIEMPO
CambiandoFormato <- function(Building_Frame)
{
  Dias_Año <- unique(Building_Frame$Day_Year)
  Frame_Dia <- data.frame(matrix(ncol = 24 , nrow = length(Dias_Año)))
  for (n in 1:24)
    colnames(Frame_Dia) <- c(0:23)
  #### VAMOS A CREAR EL FRAME DE SALIDA
  ### HACEMOS LA TRANFORMADA PERO SOLO COGIENDO LOS DIAS ENTEROS
  Vector_sin_NA <- c()
  for (h in 1:length(Dias_Año))
  {
    Vector_Demanda <- Building_Frame[which(Building_Frame$Day_Year == Dias_Año[h]),]$Dela_T
    if (length(Vector_Demanda) == 24)
    {
      Vector_sin_NA <- c(Vector_sin_NA , h)
      Frame_Dia[h,1:24] <- Vector_Demanda
    }
    #### SI FALTA ALGUN DATO NO NOS VALE
  }
  ### BORRAMOS LAS FILAS QUE SEAN NAs
  Frame_Dia <- Frame_Dia[Vector_sin_NA, ] 
  return(Frame_Dia)
}

### SIGUIENTE PASO: NORMALIZAR LOS DATOS
NormalizandoDatos <- function(FrameProfiles)
{
  for (i in 1:length(FrameProfiles[,1]))
  {
    if (sd(matrix(t(FrameProfiles[i, c(1:24)]))) == 0)
      FrameProfiles[i,] <- 0
    else
      FrameProfiles[i,] <- scale(matrix(t(FrameProfiles[i,])) , scale =  T) 
  }
  return(FrameProfiles)
}

### CREAMOS LA FUNCION PARA EL K-MEANS
### inputs <- K y el frame
ClusteringKmeansFunction <- function(FrameProfilesNormalizado , K_mean)
{
  set.seed(101)
  ## nstart es las veces que se inicia el algoritmo desde puntos diferentes
  ## centers = en cuantos grupos se quiere clusterizar
  ClustersList <- kmeans(FrameProfilesNormalizado , nstart = 50 , centers = K_mean , iter.max = 50)
  ### OBSERVAR LA LISTA DE LOS CLUSTERS
  Clusters <- ClustersList[["cluster"]]
  return(Clusters)
}

CVIAnalysis <- function(VectorCluster , FrameProfilesNormalizado)
{
  ## EL 1: DUNN, EL 2: SILHOUETTE Y EL 3: DAVIES-BOULDIN
  VectorCVI <- c()
  #PODEIS EJECUTAR EL SIGUIENTE LINEA, ARA VER COMO SE ORDENAN LOS CVIs 
  #getCriteriaNames(TRUE)
  NombresCVI <- c("Dunn" , "Silhouette" , "Davies_Bouldin")
  for (i in 1:length(NombresCVI))
    VectorCVI[i] <- intCriteria(as.matrix(FrameProfilesNormalizado) , VectorCluster , NombresCVI[i])[[1]]
  return(VectorCVI)
}

##### 1. GENERAMOS EL FRAME
FrameProfiles <- CambiandoFormato(BuildingData)
##### 2. NORMALIZAMOS
FrameProfilesNormalizado <- NormalizandoDatos(FrameProfiles)
##### 3. APLICAMOS EL KMEANS PARA K <- 3 Y PARA K <- 4
TresClusters <- ClusteringKmeansFunction(FrameProfilesNormalizado , 3)
CuatroClusters <- ClusteringKmeansFunction(FrameProfilesNormalizado , 4)

##### AHORA QUEDA ANALIZAR CUAL ES MEJOR CLUSTERIZACION: PARA ELLO VAMOS A ANALIZAR LOS INDICES DE SILHOUETTE, Y DUNN
### GENERAMOS EL FRAME PARA GUARDAR LOS INDICES
ClusterValidation <- data.frame(matrix(ncol = 2 , nrow = 3))
colnames(ClusterValidation) <- c("3 Clusters" , "4 Clusters")
rownames(ClusterValidation) <- c("Dunn Index" , "Silhouette Index" , "Davies-Bouldin Index")
ClusterValidation[,1] <- CVIAnalysis(TresClusters , FrameProfilesNormalizado)
ClusterValidation[,2] <- CVIAnalysis(CuatroClusters , FrameProfilesNormalizado)


### FIN DE LA PRACTICA OBLIGATORIA
## OPTATIVA: CUAL PROCESO ES MEJOR?

### SI TENEIS TIEMPO, DIBUJAR LOS PERFILES DE DELTA_T, SEPARADOS POR CLUSTERS
### facet_wrap()

rm(list=ls())

########### FIN DEL CURSO ################





