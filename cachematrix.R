## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

#create a matrix and cache the inverse
makeCacheMatrix <- function(x = matrix()) {
    #initialize inverse inverse
    i <- NULL
    set <- function(y) {
    #set matrix
        x <<- y
        i <<- NULL
    }
    get <- function() x
  
    #set matrix inverse 
    setinv <- function(inv) 
        i <<- inv
    
    #get matrix inverse
    getinv <- function() i
    
    #return list
    list(set = set, 
         get = get,
         setinv = setinv,
         getinv = getinv)
    
}



## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
    inv<-x$getinv()
    
    #return inv if already set
    if (!is.null(inv)) {
        print("getting cached data")
        return (inv)
    }
    #get the matrix
    mat<-x$get()
    
    #calculate inverse 
    inv<-solve(mat) 
     
    x$setinv(inv) 
    
   inv
    
}

