## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
### First step is to create a function "makeCacheMatrix ### this function will keep inverse of the matrix
makeCacheMatrix <- function(mtx = matrix()) {
  invs <- NULL    
  ## define set function when a new matrix is created  
       set <- function(mty) {    
               mtx <<- mty        ## assigning value to parent variable     
               invs <<- NULL      ## to reset parent variable to NULL to get inserve calulated  
       }    
  ## define get function   
        get <- function() mtx    
  ## setinverse function will prepare inverse of the matrix   
        setinverse <- function(inverse) invs <<- inverse    
  ## getinverse function will return inverse from cache    
        getinverse <- function() invs    
  ## return list to calling function   
        list(set = set,       get = get,       setinverse = setinverse,       getinverse = getinverse)
}

## Write a short comment describing this function
cacheSolve <- function(mtx, ...) {
        ## Return a matrix that is the inverse of 'x'
          invs <- mtx$getinverse()  
               if (!is.null(invs)) {    
                     message("Since parent environment has inverse already cached so fetching it from the cache")
                       return(invs)  
               }  
        mt_invs <- mtx$get()  
        invs <- solve(mt_invs, ...)    ## this calculate inverse of matrix using solve function of R  
        mtx$setinverse(invs)           ## set inverse in parent environment  
        invs                           ## retrun inverse of the matrix
}
