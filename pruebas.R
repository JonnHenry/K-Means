data(iris)
datos=iris
k<-3


obtenerMuestra<-function(datos,kMuestra,columnas){
  muestraDatos<- datos[sample(nrow(datos), kMuestra),] # k filas
  muestraInicial<-muestraDatos[,1:columnas]
  return(muestraInicial)
}



library(cluster.datasets)
data(all.mammals.milk.1956)
head(all.mammals.milk.1956)
input <- all.mammals.milk.1956[,2:6]

#kmeans(data, obtenerMuestra(datos,k,columnas), iter.max = 10, nstart = 1)

kmeans(obtenerMuestra(datos,4,4), centers = 4, nstart = 20)


clusters <- kmeans(obtenerMuestra(datos,3,4), 5)
