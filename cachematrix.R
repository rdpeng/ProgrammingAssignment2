## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## The first function, makeVector takes matrix as argument
## Our assumption is: the matrix will be invertible. 
## The following will be done this function 
## set the content  of the matrix
## get the content of the matrix
## set the inverse of the matrix
## get the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
         inv <- NULL
         set <- function(y) {
##         print( "Inside set.") 
         x <<- y
         inv <<- NULL
        }
     get <- function() {
##         print( "Inside get.") 
         x
        }
     setinverse <- function(inverse){
##         print (" Inside setinverse. ")
         inv <<- inverse
        }
     getinverse <- function(){
##         print("inside getInverse")     
         inv
        }
     list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## The following function calculates the inverse of matrix created with the above function.
## However, it first checks to see if the inverse  has already been computed earlier.
## If so, it gets the inverse  from the cache and skips the computation. Otherwise, 
## it computes the inverse of the data and sets the value of the inverse matrix in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
            inv <- x$getinverse()
    if(!is.null(inv)) {
         message("getting cached data.")
         return(inv)
     }
     data <- x$get()
##     print ( "just before inversing")
     inv <- solve(data)
     x$setinverse(inv)
     inv        
}
