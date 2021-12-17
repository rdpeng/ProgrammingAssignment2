## First I wrote the codes that can cache the inverse of matrix
## Then I generated 2 by 2 matrix and tried codes and they worked

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set<- function(y){
                x<<-y
                inv<<-NULL
        }
        get <- function() {x}
        setinverse <- function(inverse) {inv <<- inverse}
        getinverse <- function() {inv}
        list(set = set, get = get, setinverse = setinverse, 
             getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setinverse(inv)
        inv
}

mymatrix<-makeCacheMatrix(matrix(5:8, nrow=2, ncol=2)) # generating matrix
mymatrix$get() # to get my matrix
mymatrix$getinverse() # returns NULL because it is not cached yet
cacheSolve(mymatrix) # computes the inverse and caches it
cacheSolve(mymatrix) # "getting cached data"
mymatrix$getinverse() # Cached perfectly!
