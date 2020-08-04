## The functions makeCachematrix() and cacheSolve() together create a matrix object and compute the inverse of the matrix that can be cached.

## The makeCachematrix() function takes as input an empty matrix object by default. The input provided is assumed to an invertible matrix and is assigned to the variable x.
## The function returns a list of named elements (each element is a function in this case) and this serves as an input for the cacheSolve() function.

makeCacheMatrix <- function(x = matrix()) {
        x_inverse<- NULL
        set<-function(y){
                x<<-y
                x_inverse<<-NULL
        }
  
        get<-function() x
        setinverse<-function(inverse) x_inverse<<-inverse
        getinverse<-function() x_inverse
        list(get = get,set = set, getinverse = getinverse, setinverse = setinverse)
  }

## The cacheSolve() function takes as input an object returned by the makeCachematrix() function. It first retrieves the inverse of the matrix and in case the value is NULL 
## the matrix object is retrieved and the inverse it computed using the solve() function and returned (the value is also cached). 
## If the inverse value has been previously computed/cached and the matrix input is the same, the inverse value is retrieved and returned.

cacheSolve <- function(x, ...) {
        x_inverse<-x$getinverse()
        if(!is.null(x_inverse)){
                print("getting cached inverse value")
                return(x_inverse)
        }
        z<-x$get()
        x_inverse<-solve(z, ...)
        x$setinverse(x_inverse)
        x_inverse
}
