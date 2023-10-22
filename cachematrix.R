## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
          # variable pour stocker l'inverse de la matrice en cache
  inv <- NULL
  
  # getter pour la matrice
  get <- function() {
    x
  }
  
  # setter pour la matrice
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # getter pour l'inverse
  getInverse <- function() {
    inv
  }
  
  # setter pour l'inverse
  setInverse <- function(inverse) {
    inv <<- inverse
  }
  
  # retourne une liste des fonctions définies
  list(get = get,
       set = set,
       getInverse = getInverse,
       setInverse = setInverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
         # récupère la matrice de l'objet "matrice" passé en argument
  mat <- x$get()
  
  # vérifie si l'inverse est déjà en cache
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("Récupération de l'inverse depuis le cache")
    return(inv)
  }
  
  # sinon, calcule l'inverse de la matrice
  inv <- solve(mat, ...)
  
  # met l'inverse en cache
  x$setInverse(inv)
        ## Return a matrix that is the inverse of 'x'
        inv
}
