# Function will compute the inverse of a matrix
makeCacheMatrix <- function(x = matrix()) {
     # Inverse matrix in main env is set to NULL 
     Inversematrix <- NULL
     set <- function(y) {
          # set new matrix and clear an old inverse
          x <<- y
          Inversematrix <<- NULL
     }
     # Function above return getters and setters for the matrix
     get <- function() x
     setInverse <- function(inverse) Inversematrix <<- inverse
     getInverse <- function() Inversematrix 
     # Contain named list access the function by name rather than using [[
     list(set = set, get = get,
          setInverse = setInverse,
          getInverse = getInverse)
}


# Function that solve the cache matrix

cacheSolve <- function(x,...){
     # get an Inverse of matrix 
     Inversematrix <- x$getInverse()
     # check matrix if Inverse exist return message
     if(!is.null(Inversematrix)) {
          message("getting cached data")
          return(Inversematrix)
     }
     # get matrix from makeCacheMatrix
     data <- x$get()
     Inversematrix <- solve(data) %*% datas
     x$setInverse(Inversematrix)
     Inversematrix
}
