## Put comments here that give an overall description of what your
## functions do

##makeCacheMatrix() -> Used to set the value of matrix,calculate inverse and put it into cache,so that if the same computation has to be done again,it is not to be recomputed and we can directly get the already computed result.
##cacheSolve()->Checks whether the inverse of the matrix has already been calculated or not,if calculated,returns the computed result otherwise calculates the inverse of matrix and then returns the computed result.

## Write a short comment describing this function

##The first function, makeCacheMatrix creates a special matrix, which is really a list containing a function to

##1)set the value of the matrix
##2)get the value of the matrix
##3)set the value of the inverse of matrix
##4)get the value of the inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
    var<-NULL
    set<-function(y){
        x<<-y
        var<<-NULL
    }
    get<-function() x
    setinverse<-function(inverse) var<<- inverse
    getinverse<-function() var
    list(set=set,get=get,
         setinverse=setinverse,
         getinverse=getinverse)
}


## Write a short comment describing this function

##The following function calculates the inverse of the special matrix created with the above function. 
##However, it first checks to see if the inverse has already been calculated. 
##If so, it gets the inverse from the cache and skips the computation. Otherwise, it calculates the inverse of the matrix and sets the value of the inverse of matrix in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    var<-x$getinverse()
    if(!is.null(var)){
        message("getting cached data........")
        return(var)
    }
    data<-x$get
    var<-solve(data, ...)
    x$setinverse(var)
    var
}

