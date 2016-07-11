makeCacheMatrix <- function(x = matrix()) {
        inv <- Null
        set <- function(y) {
                x <<- y
                inv <<- Null
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set,get = get,setInverse = setInverse,getInverse = getInverse)
}