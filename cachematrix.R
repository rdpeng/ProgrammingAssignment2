## My fuctions first make a cache based matrix 
## an then it calculates its inverse.
## although its not very precise but when multipled with 
## original matrix (a true matrix multiplication %*%) yield an
## identity matrix of same size after roundup function.


## this function creates a cache based matrix,
## it sets and Gets values of Matrix and its inverse matrix

makeCacheMatrix <- function(x = matrix()) {
          invMat <- NULL
        set <- function(y) {
             x <<- y
        invMat <<- NULL
         }
       get <- function() x
       setinverse <- function(inverse) invMat <<- inverse
       getinverse <- function() invMat
       list(set = set, get = get,
                       setinverse = setinverse,
                       getinverse = getinverse)
}


## this function assumes that the supplied matrix 
## is a square matrix and solves its 
## inverse in cache

cacheSolve <- function(x, ...) {
  invMat <- x$getinverse()
  if(!is.null(invMat)) {
    message("getting cached data")
    return(invMat)
  }
  data <- x$get()
  invMat <- solve(data, ...)
  x$setinverse(invMat)
## Return a matrix that is the inverse of 'x'
  invMat
}        

## above functions are checked by following Procedure
## assume a Matrix as follows
a <- matrix(rnorm(20), 4, 4)
b <- makeCacheMatrix(a)  ## this creates a cache based matrix
c <- cacheSolve(b) ## this calculates the matrix up to 7 decimal digits

## to verify its accuracy
## any matrix when multiplied with its invers yields an 
## identity matrix of same size

d <- c %*% a
e <- round(d)

## Result (e) would be an identity matrix of same size (here say 4x4)
