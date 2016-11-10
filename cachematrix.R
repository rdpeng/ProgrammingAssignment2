
## Andrea Morillo 2016 - Semana 3 Tarea Asignada.
## I want to ask for help because i'm not an english native speaker

## La función makeCacheMatrix crea un objeto especial denominado matriz
##que puede guardar la inversa en el caché, la cual, es una matriz invertible

makeCacheMatrix <- function(a = matrix()) { 
  #First we create this function that will help us through the environment.
  
  inversa <- NULL #Funcion inversa
  y <- function(n) { # We create the function
    a <<- d
    inversa <<- NULL
  }
  get <- function() a #The commander get, will save the function
  setinv <- function(inverse) inversa <<- inverse
  getinv <- function() inversa
  list(y = y, get = get, setinv = setinv, getinv = getinv)
}

## Write a short comment describing this function
##We create the function cachesolve that calculates 
## La función cacheSolve calcula la inversa de la matriz
## obtenida de la función makeCacheMatrix. Si la inversa ya
## ha sido calculada y la matriz no ha cambiado entonces devuelve
## la inversa guardada en el cache


cacheSolve <- function(a, ...) { ## The use of ... are the parameters that we want to change.
  # 
  
  inversa <- a$getinv()
  
  
  if(!is.null(inversa)) {
    print("Getting the result");
    return(inversa)
  }
  data <- a$get()

  inversa <- solve(data, ...)
  a$setinv(inversa)
  inversa 
}