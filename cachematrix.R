## Calculate the inverse of the matrix and caches it

## Create a matrix which is a list containing a function that sets the value of the matrix,
##gets the value of the matrix, and then sets and gets the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
        set<-function(y){
                x<<-y
                m<<-NULL
        }
        get<-function() x
        setmatinverse<-function(solve) m<<-solve
        getmatinverse<-function() m
        list(set = set, get = get, setmatinverse = setmatinverse, getmatinverse = getmatinverse)
        

}


## Calculate the inverse of the matrix if the inverse of the matrix has not already been calculated
## If it has been calcualated, grabs the inverse of the matrix from the cache, skips calculation

cacheSolve <- function(x, ...) {
        m<-x$getmatinverse()
        if(!is.null(m)){
                message ("getting cached data")
                return(m)
        }
        data<-x$get()
        m<-solve(data, ...)
        x$setmatinverse(m)
        m
}
