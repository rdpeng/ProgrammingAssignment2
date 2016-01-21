##Sometimes running a funtion may need lots of time, for example, solve funciton
##then we need to cache the result when we have the result. The following is an 
##example of how to keep the result fo the inverse of a matrix.

##First we need to do is to make the cache matrix. Cache the matrix and the result.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set=set,get=get,
             setinv=setinv,
             getinv=getinv)
        
}


##Next, we need to compute the inverse of the matrix

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {                     ##check the result in the environment
                message("getting cached data")
                return(inv)
        }
                                                ##if doesn't have, then compute 
                                                ##it and cache it
        data <- x$get()
        inv <- solve(data,...)
        x$setinv(inv)                           ##cache the result
        inv
}

##example of how to use it


x<-matrix(1:4,2,2)
##     [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5

s<-makeCacheMatrix(x)


##If we do it the first time, we need to calculate it.

cacheSolve(s)
##      [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5

##But if we do it later, then we can just get the result without computing it 
##one more time.

cacheSolve(s)
##getting cached data
##      [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5



