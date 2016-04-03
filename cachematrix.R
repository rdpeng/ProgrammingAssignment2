## The functions create a special object that stores a matrix and caches its inverse. The second part checks to see if the inverse has been calculated and if it has, it skips the computation and if it hasn't, then it performs the computation.

## set the value of the matrix, ge the value of the matrix, set the value of the inverse, get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    m<-NULL
    set<-function(y) {
        x<<-y
        m<<-NULL
    }
    get<-function()x
    setsolve<-function(solve) m<<-solve
    getsolve<-function() m
    list(set=set, get=get,
    setsolve=setsolve,
    getsolve=getsolve)

}


## Check to see if inverse has already been calculated and use that value, if it hasn't been calculated yet, then this will calculate it.

cacheSolve <- function(x, ...) {
    m<-x$getsolve()
    if(!is.null(m)){
      message("getting cached data")
        return(m)}
    
    data<-x$get()
    m<-solve(data,...)
    x$setsolve(m)
    m
        ## Return a matrix that is the inverse of 'x'
}
