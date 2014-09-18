## A pair of functions that cache the inverse of a matrix

## makecacheMatrix() creates a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        
        ## Method to set the matrix
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        ## Method the return the matrix
        get <- function() {
                x      
        } 
        
        ## Method to set the inverse of the matrix
        setinv <- function(m_inv) {
                inv<<-m_inv  
        } 
        
        ## Method to return the inverse of the matrix
        getinv <- function() {
                inv
        }
        
        ## Returning a special matrix object containing methods to cache inverse
        matrix(c(set,get,setinv,getinv),2,2)
        
}

## cacheSolve() computes the inverse of the special "matrix" returned by makeCacheMatrix()
## If the inverse has already been calculated, then cachesolve() retrieves
## the inverse from the cache
cacheSolve <- function(x,...){
       
        x<-as.list(x)
        inv <- x[[4]]()
        
        ## Return the inverse from cache if its already set
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        
        ## Get the matrix from our object
        data <- x[[2]]()
        
        ## Calculate the inverse using solve()
        inv <-solve(data,...)
        
        ## Set the inverse to the object
        x[[3]](inv)
        
        ## Return the inverse matrix
        inv
}
