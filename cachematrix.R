## A pair of vectors to create a matrix that can cache its inverse
## and then pull the cached inverse rather than re-calculating

## a function to create a matrix that can cache its inverse

makeCacheMatrix <- function(x = solve()) {
        m<-NULL
        set<-function(y){
                x<<-y
                m<<-NULL
        }
        get<-function()x
        setsolve<-function(solve) m <<-solve
        getsolve<-function()m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## a function to return the inverse of a matrix, either calculating or from cache
## if available

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m<-x$getsolve()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data<-x$get()
        m<-solve(data, ...)
        x$setsolve(m)
        m
}
