## Las funciones que usare lo que haran son el almacenamiento en cache de la inversa de una matriz.

## -makeCacheMatrix crea el objeto "matriz" el cual podra almacenar el inverso en cache.
## esta "matriz" creada es una lista que tiene la funcion para
## 1. Establecer el valor de la matriz
## 2.obtener el valor de la matriz
## 3. Establecer la inversa de la matriz
## 4. Obtener la inversa de la matriz

makeCacheMatrix <- function(x = matrix()) {
inv<- NULL
set<- function(y){
  x<<- y
  inv<<- NULL
}
get<- function() x
setInverse <- function(solveMatrix) inv<<- solveMayrix
getInverse <- function() inv
list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## La funcion "cacheSolve" calcula la inversa de la matriz creada anteriormente
## En primer lugar comprueba si la inversa de la matriz ya se ha calculado 
## Si es asi entonces obtiene la inversa del cache y omite el calculo
## Si no es asi entonces calcula el inverso de los datos y establece el inverso de la matriz en el cache a traves de la funcion setInverse.

cacheSolve <- function(x, ...) 
{
  inv<- x$getInverse()
  if (!is.null (inv)){
    message ("obteniendo datos en cache")
    return(inv)
  }
  data<- x$get()
  inv<- solve(data, ...)
  x$setInverse (inv)
  inv
}

