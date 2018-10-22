## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## Create a list with methods to write and read a Matrix
## Ex. Input matrizA <- matrix(c(5,1,0,3,-1,2,4, 0, -1), nrow = 3, byrow = TRUE)
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL## Inversa
    ##// Getters y Setters de la matriz original
    get <- function(){
        x
    }
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    ##// Getters y Setters de la matriz Inversa
    setInverse <- function(inverseLocal){
        m <<- inverseLocal  
    } 
    getInverse <- function(){
        m  
    } 
    
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## asalazaru This function calculate the inverse of 'X', if it exists in cache,
## it'll return the value stored, but in other case it'll calculate the inverse
cacheSolve <- function(x, ...) {
    
    m <- x$getInverse()
    if(!is.null(m)) {
        message("Getting cached data ...")
        return(m)
    }
    
    message("Calculating inverse matrix ...")
    
    ## Return a matrix that is the inverse of 'x'
    dataLocal <- x$get()
    
    ## ** Recover the info **
    matrizAInv <- NULL
    # Obtiene el numero de columnas de la matriz original para obtener la matriz identidad
    numeroColumnas <- ncol(dataLocal)
    matrizI <- diag(numeroColumnas)
    # Comprobar que se puede obtener su inversa
    if(det(dataLocal)!=0){
        matrizAInv <- solve(dataLocal,matrizI)    
    }else{
        message("There isn't inverse of 'x'")
    }
    
    ## ** Set the info **
    x$setInverse(matrizAInv)
    matrizAInv
}
