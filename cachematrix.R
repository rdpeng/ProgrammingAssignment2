> # makeCacheMatrix creates a list containing a function to
> # 1. set the value of the matrix
> # 2. get the value of the matrix
> # 3. set the value of inverse of the matrix
> # 4. get the value of inverse of the matrix
> ## Write a short comment describing this function
> 
> makeCacheMatrix <- function(x = matrix()) {
+   inv <- NULL
+   set <- function(y) {
+     x <<- y
+     inv <<- NULL
+   }
+   get <- function() x
+   setinverse <- function(inverse) inv <<- inverse
+   getinverse <- function() inv
+   list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
+ }
> ## Write a short comment describing this function
> 
> cacheSolve <- function(x, ...) {
+   inv <- x$getinverse()
+   if(!is.null(inv)) {
+     message("getting cached data.")
+     return(inv)
+   }
+   data <- x$get()
+   inv <- solve(data)
+   x$setinverse(inv)
+   inv
+ }
> # Sample run:
> x = rbind(c(7, -4), c(-8, 5))
> m = makeCacheMatrix(x)
> m$get()
     [,1] [,2]
[1,]    7   -4
[2,]   -8    5
> ## No cache in the first run
> cacheSolve(m)
         [,1]     [,2]
[1,] 1.666667 1.333333
[2,] 2.666667 2.333333
> 
