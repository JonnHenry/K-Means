library(plyr)

data(iris)
datos=iris

datos$Species<-revalue(datos$Species, c("virginica"=1))
datos$Species<-revalue(datos$Species, c("setosa"=2))
datos$Species<-revalue(datos$Species, c("versicolor"=3))


obtenerDistancia <- function(distOrigen,distFinal,numcol) {
  resultado = 0
  for (i in 1:numcol){
    resultado = (distFinal[[i]][[1]]-distOrigen[[i]][[1]])^2 + resultado
  }
  return(resultado^0.5)
}

graficaClasificacion <- function(datos){
  par(mfrow=c(1,2))
  plot(x = 1:nrow(datos),y=as.numeric(as.character(datos[[5]])),xlab = "Numero de Datos", ylab = "Clasificación",main = "Clasificación Correcta")
  plot(x = 1:nrow(datos),y=datos[[6]],xlab = "Numero de Datos", ylab = "Clasificación",main = "Clasificación Realizada")
  #legend("bottomleft",c("1<-virginica","2<-setosa","3<-versicolor"), ncol=2,bty="n",fill = c("royalblue","","grey"))
}


agrupaDatos<-function(clasificacion,k){
  datosTotales<-data.frame()
  for (i in 1:k){
    datosTotales<- rbind(clasificacion[[i]], datosTotales)
  }
  return(datosTotales)
}


obtenerMuestra<-function(datos,kMuestra,columnas){
  muestraDatos<- datos[sample(nrow(datos), kMuestra),] # k filas
  muestraInicial<-muestraDatos[,1:columnas]
  return(muestraInicial)
}

kMeans<-function(datos,k,columnas){
  distFila<-list()
  listSalida<-list()
  listDatosMinimos<-list()
  for (m in 1:k){
    listDatosMinimos[[m]] <- obtenerMuestra(datos,1,columnas+1)
    listDatosMinimos[[m]] <- listDatosMinimos[[m]][-c(1),]
  }
  centroides<-obtenerMuestra(datos,k,columnas)
  centroidesAnterior<-centroides
  while(TRUE){
    
    for(i in 1:nrow(datos)){
      for (j in 1:k) {
        distFila[[j]]  <- obtenerDistancia(datos[i,1:columnas],centroides[j,],columnas)#Obtiene las distancias
      }
      listDatosMinimos[[which.min(distFila)]] <- rbind(listDatosMinimos[[which.min(distFila)]], datos[i,])#Clasificar por la menor distancia
    }
    
    if(identical(centroides, centroidesAnterior)){ #Comparar si las dos matrices son identicas para finalizar el algoritmo
      for (m in 1:k){
        centroides[m,]<-colMeans(listDatosMinimos[[m]][,1:columnas])#Obtener los nuevos centroides
        rownames(listDatosMinimos[[m]])=NULL
        
        listDatosMinimos[[m]] <- cbind(listDatosMinimos[[m]], ClasificionCluster=m)
      }
      break
    }else{
      for (m in 1:k){
        centroides[m,]<-colMeans(listDatosMinimos[[m]][,1:columnas])#Obtener los nuevos centroides
        listDatosMinimos[[m]] <- listDatosMinimos[[m]][-c(1:nrow(listDatosMinimos[[m]])),]#Vaciar los datos
      }
      centroidesAnterior<-centroides
    }
  }
  rownames(centroides)=NULL
  listSalida[["clusters"]] <- centroides
  listSalida[["clasificacion"]] <- listDatosMinimos
  return(listSalida)
}

resultK_Means<-kMeans(datos,3,4)

cat("Cluster Means\n")
print(resultK_Means[["clusters"]])
cat("Resultados de la clasificación\n")
cat("Asignación a los tipos de clases\n")
cat("1 <- virginica\n")
cat("2 <- setosa\n")
cat("3 <- versicolor\n")
print(resultK_Means[["clasificacion"]])
#Species Corresponde a la clasificación realizada por los mismos datos
#ClasificionCluster son las clasificaciones hechas por el algoritmo k-means
datosAgrupados<-agrupaDatos(resultK_Means[["clasificacion"]],3)#Agrupar de los clusters para su analisis
graficaClasificacion(datosAgrupados)

