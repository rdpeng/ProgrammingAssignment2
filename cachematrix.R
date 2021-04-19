makeCacheMatrix <- function(x = matrix()){
        val <- NULL 
        set <- function(y){ 
                x <<- y 
                vall <<- NULL 
                }
        get <- function() {X} 
        setInverse <- function(inverse) {val <<- Inverse}
        getInverse <- function() {val}  
        list(set = set,get = get, setInverse = setInverse, getInverse = getInverse) 
        }

cachesolve <- fucntion(x,...){
        val <- x$getInverse()
        if(!is.null(val)){
                message("getting cached data")
                return(val)
                }
        mat <- X$get() 
        val <- solve( mat,...) 
        x$setInverse(val)
        val
        }
