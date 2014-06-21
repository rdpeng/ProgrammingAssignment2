
## makeCacheMatrix will create special matrix object that will cache matrix inverse and 
##cacheSolve will calculate the inverse of the matrix

makeCacheMatrix <- function(m = matrix()) {
        i<-NULL
                set <-function(matrix) {
                        m<<-matrix
                        i<<-NULL
}
        get <-function()  m
        setInverse <-function(inverse)  i<<- inverse
        getInverse <- function() i
        
        list(set = set, get = get,
                setInverse = setInverse,
                getInverse = getInverse)
}



## cacheSolve will return matrix that is inverse of x
## If cached inverse is available, cacheSolve retrieves it, if not will compute, cache
## and return it.

cacheSolve <- function(x, ...) {
        m<- x$getInverse()

                if(!is.null(m)) {
                        message("gettingcacheddata")
                                return(m)

}
        data<- x$get()
                m<-solve(data)
                x$setInverse(m)
                m
}
