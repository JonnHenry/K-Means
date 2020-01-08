data(iris)
datos=iris
k<-3

obtenerDistancia <- function(distOrigen,distFinal) {
  resultado = 0
  for (i in 1:length(distOrigen)){
    resultado = (distFinal[[i]][[1]]-distOrigen[[i]][[1]])^2 + resultado
  }
  return(resultado^0.5)
}

obtenerMuestra<-function(datos,kMuestra,columnas){
  muestraDatos<- datos[sample(nrow(datos), kMuestra),] # k filas
  muestraInicial<-muestraDatos[,1:columnas]
  return(muestraInicial)
}

kMeans<-function(datos,k,columnas,dataFrame){
  distFila<-list()
  listDatosMinimos<-list()
  for (m in 1:k){
    listDatosMinimos[[m]] <- obtenerMuestra(datos,1,columnas)
    listDatosMinimos[[m]] <- listDatosMinimos[[m]][-c(1),]
  }
  centroides<-obtenerMuestra(datos,k,columnas)
  centroidesAnterior<-centroides
  while(TRUE){
    
    for(i in 1:nrow(datos)){
      for (j in 1:k) {
        distFila[[j]]  <- obtenerDistancia(datos[i,1:columnas],centroides[j,])#Obtiene las distancias
      }
      listDatosMinimos[[which.min(distFila)]] <- rbind(listDatosMinimos[[which.min(distFila)]], datos[i,1:columnas])#Clasificar por la menor distancia
    }
    
    for (m in 1:k){
      centroides[m,]<-colMeans(listDatosMinimos[[m]])#Obtener los nuevos centroides
      listDatosMinimos[[m]] <- listDatosMinimos[[m]][-c(1:nrow(listDatosMinimos[[m]])),]#Vaciar los datos
    }
    
    if(identical(centroides, centroidesAnterior)){ #Comparar si las dos matrices son identicas para finalizar el algoritmo
      break
    }else{
      centroidesAnterior<-centroides
    }
  }
  
  return(centroides)
}

print(kMeans(datos,4,4))


