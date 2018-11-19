## Put comments here that give an overall description of what your
## functions do

## makecacheMatrix function creates special matric object which can cache its inverse
## it return list with set,get,setinv and getinv of matrix
makeCacheMatrix <- function(x = matrix()) {
    
    inv <- NULL
    set <- function(y) {
         x <<- y   ## ASSIGNING to be used in different environment
         inv <<- NULL ## ASSIGNING to be used in different environment
    }
    get <- function() x
    setinv <- function(invers) inv <<- invers
    getinv <- function() inv
    list(set=set, get=get, setinv=setinv, getinv=getinv)

}


## The Below function computes the Inverse of the Matrix which is returned by
## makecachematrix function, it also checks where Inverse is already calculated by checking for Null
## if not null , then it will take value from already cached Inv else will create an Inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        
        if (!is.null(inv)){
            ## inside the checkpoint of whether INV was not null
            message("getting cached data")
            return(inv)
        }
        ## calculating Inverse and populating it in the inv
        data <- x$get()
        inv <- solve(data)
        
        x$setinv(inv)
        
        inv
}
