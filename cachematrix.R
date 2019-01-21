## En general, vamos a escribir un código que en el que podamos encontrar la inversa de una matriz


#############################################################

## "makeCacheMatrix" almacenará el inverso del objeto que cree; en este caso una matriz

makeCacheMatrix <- function(x = matrix()) {
         M_inver <- NULL
  set <- function(y) {
    x <<- y
  M_inver <<- NULL
  }
  get <- function() x
  setInversa <- function(inversa) M_inver <<- inversa
  getInversa <- function() M_inver
  list(set = set, get = get,
       setInversa = setInversa,
       getInversa = getInversa)

}


## Esta función calcula la inversa de la matriz "makeCacheMatrix".
## si el inverso ya se ha calculado (y la matriz no ha cambiado), entonces la solución es la inversa

cacheSolve <- function(x, ...) {
         M_inver <- x$getInversa()
  if (!is.null(M_inver)) {
    message("dato en cache")
    return(M_inver)
  }
  mat <- x$get()
  M_inver <- solve(mat, ...)
  x$setInversa(M_inver)
  M_inver
       
}

