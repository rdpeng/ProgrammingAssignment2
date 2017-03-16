## write a function to cache the computation of inverse of a matrix

## creat a list contain the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setInverse <- function(Inverse) m <<- Inverse
    getInverse <- function() m
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## check whether inverse of x exists, if yes, return the cashed value, otherwise, calculate the inverse of the matrix and return it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    x_list<-makeCacheMatrix(x)#if i assign the list into x instead of another name x_list, R always told me that a is not a numeric matrix in the test
    m <- x_list$getInverse()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x_list$get()
    m <- solve(data)
    x_list$setInverse(m)
    m
}


##test the function by creating a random matrix and return its inverse
set.seed(1)
a<-matrix(rnorm(25),5,5)
c<-cacheSolve(a)
c
