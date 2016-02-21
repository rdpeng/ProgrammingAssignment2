
## Programa da semana 3 R Programming
## Esta funcao armazena a inversa da matriz, com ela podemos armazenar
## no cache para usar repetidamente.
## A funcao abaixo cria um objeto especial "matriz" que pode armazenar
## em cache a sua inversa.

makeCacheMatrix <- function(x = matrix()) {
  inversa <- NULL
  set <- function(y) {
    x <<- y
    inversa <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inversa <<- inverse
  getInverse <- function() inversa
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## A segunda funcao calcula a inversa da "matriz" que foi retornada
## por makeCacheMatrix (a primeira funcao). Se a inversa foi calculada
## (e a segunda nao mudou), a funcao cacheSolve deve recuperar a
## inversa da cache.


cacheSolve <- function(x, ...) {
  inversa <- x$getInverse()
  if(!is.null(inversa)) {
    message("getting cached data")
    return(inversa)
  }
  data <- x$get()
  inversa <- solve(data, ...)
  x$setInverse(inversa)
  inversa
}


