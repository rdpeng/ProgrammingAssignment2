## Put comments here that give an overall description of what ## Put comments here that give an overall description of what your
## functions do

## crea una "matriz" especial, que en realidad es una lista que contiene una función para:
##establecer el valor de la matriz,  obtener el valor de la matriz,  establecer el valor de la inversa,  obtener el valor de la inversa

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


##Esta función calcula la inversa de la "matriz" especial devuelta por makeCacheMatrix anterior. Si la inversa ya se ha calculado (y la matriz no ha cambiado), cacheSolve debería recuperar la inversa de la caché
cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
