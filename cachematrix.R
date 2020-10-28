library(MASS) # utilizo esta libreria para calcular la inversa de una matriz
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL               #inicializa la variable inv como nula
    set <- function(y){
          x<<-y
        inv<<-NULL
    }
    get <- function()x
    setinv<-function (inverse)inv<<-inverse
    getinv<-function()  {inver<-ginv(x)
                        inver%*%x   #funcion que obtiene la inversa de la matriz
    }
    list(set = set, get = get,setinv = setinv, getinv = getinv)
    
}


# Write a short comment describing this function
#esta función obtiene el cache de datos

cacheSolve <- function(x, ...) #obtiene el cache de datos 
  {
  inv <-x$getinv()
  if(!is.null(inv)){   #comprueba si la inversa es nula
      message("obteniendo datos cache")
      return(inv)     #retorna valor inversa
  
      }
  }
