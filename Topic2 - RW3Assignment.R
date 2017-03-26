## This function creates a special "matrix" object that can cache its inverse.x = rbind(c(1, -1/4), c(-1/4, 1))
#### Course 2 - R Programming - W3 Assignment
makeCacheMatrix <- function(x = matrix()) {
  
  i <- NULL
  n <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  calinv <- function(solve) i <<- solve
  retriveinv <- function() i
  list(n = n, get = get, calinv = calinv, retriveinv = retriveinv)
  
}

## OUTPUT TRAIL -- 
#> x=matrix(1:4,2,2)
#> x
#[,1] [,2]
#[1,]    1    3
#[2,]    2    4

#> y<-makeCacheMatrix(x)
#[,1] [,2]
#[1,]    1    3
#[2,]    2    4
##********************************************************************************************************

## `cachecomp` function computes the inverse of the special "matrix" returned by `makeCacheMatrix` above. 
## Course 2 - R Programming - W3 Assignment

cachesolve <- function(x, ...) { 
  i<- x$retriveinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i<- solve(data, ...)
  x$calinv(i)
  i
  
}
## OUTPUT TRAIL
#> s<-cachesolve(y)
#> s
[,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5


