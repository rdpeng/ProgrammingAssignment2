## This program is meant to save computation time when some calculation is done repeatedly.
##Our example is a matrix ant its inverse.
## it is done in two functions makeCacheMatrix & cacheSolve

## ##First in makeCacheMatrix we create an object that we will use to cache
##a matrix named “x” and its inverse named “inv” (therefore they will work as global variables)

##We define set and get methods for both variables we will be able to “introduce” data
##with the set methods and “extract” them with the get methods
##Assignment: Caching the Inverse of a Matrix
##creating a matrix

makeCacheMatrix <- function(x =numeric() ) { 
        print("Introduce your square matrix with object$set(), where object is the matrix you just created")
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(matrixInverse) inv <<- matrixInverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
}


## Second in cacheSolve we check if the inverse matrix has been already calculated,
##if so we obtain it from the memory with the get method,
##otherwise we calculate the inverse and save it with the set method,
##so next time we will use it and none calculation will be need.


cacheSolve <- function(x, ...) {
#getting inverse of non singular square matrix from cache or calculating inverse
#we suppose a non singular square matrix created previously by makeCacheMatrix function
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
}
