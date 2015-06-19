## Programming Assignment #2
## Author : Madhu Balaji
## Function returns Inverse of the Matrix
## Enhanced feature of this function is the cached dataset for faster retrieval
## Data is calculated the first time and stored in the cache 
## Any subsequent retrieval will be from cache hence it will take very less time to fetch


## MakecacheMatrix - This function creates the Inverse of the passed Matrix

makeCacheMatrix <- function(x = matrix()) {
  m = NULL
  set = function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInv <- function(inverse) m <<- inverse
  getInv <- function() m
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
  
}


## CacheSolve - This function invokes inverse function the first time 
## If the Cached value is not null then it will return from the cache for subsequent requests

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      
        m <- x$getInv()
        if(!is.null(m)) {
          print("From cached dataset")
          return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setInv(m)
        
        return(m)
  }

## UNIT Testing
##Test Function invoking Cache Matrix and return the cached value on the Second Call
testf <- function(matVal) {
  temp = makeCacheMatrix(matVal)
  print("FIRST CALL - CALC VALUE")
  print(cacheSolve(temp))
  print("SECOND CALL FROM CACHE")
  cacheSolve(temp)
 
}

## Use below Sample to run the test
## mat <- matrix(1:4, nrow=2, ncol = 2)
## testf(matVal = mat)

## OUTPUT
## > testf(matVal = mat)
## [1] "FIRST CALL - CALC VALUE"
##       [,1] [,2]
## [1,]   -3  2.5
## [2,]    2 -1.5
## [1] "SECOND CALL FROM CACHE"
## [1] "From cached dataset"
##      [,1] [,2]
## [1,]   -3  2.5
## [2,]    2 -1.5
