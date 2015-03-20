
makeCacheMatrix <- function(x = matrix()) {
        matrizCacheInversa <- NULL 
        
        set <- function(matrizIntroducida = matrix()) {
                x <<- matrizIntroducida
                matrizCacheInversa <<- NULL
        }
        
        get <- function() x
        
        setInversa <- function(matrizInversa) {
                matrizCacheInversa <<- matrizInversa 
                return(matrizCacheInversa)
        }
        
        getInversa  <- function() matrizCacheInversa
        list(set = set, get = get, setInversa = setInversa, getInversa = getInversa)
}


cacheSolve <- function(x, ...) { 
        
        matrizInversaCalculada <- x$getInversa() 
        
        if(!is.null(matrizInversaCalculada) && is.matrix(matrizInversaCalculada)) { 
                return(matrizInversaCalculada)
        }
        
        matrizaInvertir <- x$get()  
        
        matrizInversaCalculada <-  solve(matrizaInvertir)
        
        x$setInversa(matrizInversaCalculada)
}
