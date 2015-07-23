
makeCacheMatrix <- function(x = numeric()) {
			
		# creo cache
        cc <- NULL
        
        # inicalizacion de la matriz
        setMatrix <- function(newValue) {
                x <<- newValue                
                cc <<- NULL
        }

        # devuelve al resultado matriz
        getMatrix <- function() {
                x
        }

        # lo llama si es la cache
        cacheInverse <- function(solve) {
                cc <<- solve
        }

        # devuelve la cache
        getInverse <- function() {
                cc
        }
        
        # la lista reultaante
        list(setMatrix = setMatrix, getMatrix = getMatrix, cacheInverse = cacheInverse, getInverse = getInverse)
}


cacheSolve <- function(y, ...) {
        inverse <- y$getInverse()
        if(!is.null(inverse)) {
                return(inverse)
        }
        data <- y$getMatrix()
        inverse <- solve(data)
        y$cacheInverse(inverse)
        inverse
}
