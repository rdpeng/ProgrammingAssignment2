## These two function caching the inverse of a matrix.
## makeCacheMatrix is a function that returns a list of functions
## Its puspose is to store a martix and a cached value of the inverse of the
## matrix. Contains the following functions:
## * set set the value of a matrix
## * get get the value of a matrix
## * setmatrix get the cahced value (inverse of the matrix)
## * getmatrix get the cahced value (inverse of the matrix)

## The first function, makeCacheMatrix, creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        
        # holds the cached value or NULL if nothing is cached
        # initially nothing is cached so set it to NULL
        m<-NULL
        
        
        # store a matrix
        set<-function(y){
                x<<-y
                # since the matrix is assigned a new value, flush the cache
                m<<-NULL
        }
        
        # returns the stored matrix
        get<-function() x
        
        # cache the given argument 
        setmatrix<-function(solve) m<<- solve
        
        # get the cached value
        getmatrix<-function() m
        
        # return a list. Each named element of the list is a function
        list(set=set, get=get,
             setmatrix=setmatrix,
             getmatrix=getmatrix)
}

## The cacheSolve function calculates the inverse of the special matrix 'mat' by makeCacheMatrix. 
## If the inverse has already been calculated, and the matrix is unchanged, then cacheSolve 
## retrieves the inverse from the cache.
cacheSolve <- function(x=matrix(), ...) {
        
        # get the cached value
        m<-x$getmatrix()
        
        # if a cached value exists return it
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        
        # otherwise get the matrix, caclulate the inverse and store it in
        # the cache
        matrix<-x$get()
        m<-solve(matrix, ...)
        x$setmatrix(m)
        
        # return the inverse
        m
}

## Here I create a 2x2  square matrix (because 'solve' only handles square matrices) during the call
## of makeCacheMatrix().
mat <- makeCacheMatrix( matrix(seq(1,4,1), nrow = 2, ncol = 2) )

## Summary of 'mat' shows it is a list of functions
summary(mat)
# Length Class  Mode    
#> set       1      -none- function
#> get       1      -none- function
#> setmatrix 1      -none- function
#> getmatrix 1      -none- function

## Using get() returns the called 2x2 matrix
mat$get()
#> [,1] [,2]
#> [1,]    1    3
#> [2,]    2    4

## Running cacheSolve() on 'mat' calculates the inverse of the matrix
cacheSolve(mat)
#> [,1] [,2]
#> [1,]   -2  1.5
#> [2,]    1 -0.5


## Call cacheSolve() again we get the cached value
cacheSolve(mat)
#> getting cached data
#> [,1] [,2]
#> [1,]   -2  1.5
#> [2,]    1 -0.5

