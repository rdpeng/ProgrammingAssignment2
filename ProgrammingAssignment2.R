
> makeCacheMatrix <- function(x = matrix()) {
+     inv <- NULL
+     set <- function(y) {
+         x <<- y
+         inv <<- NULL
+     }
+     get <- function() x
+     setinverse <- function(inverse) inv <<- inverse
+     getinverse <- function() inv
+     list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
+ }
> cacheSolve <- function(x, ...) {
+     inv <- x$getinverse()
+     if(!is.null(inv)) {
+         message("getting cached data.")
+         return(inv)
+     }
+     data <- x$get()
+     inv <- solve(data)
+     x$setinverse(inv)
+     inv
+ }
> m <- matrix(1:4, 2, 2)
> x <- makeCacheMatrix(m)
> x$get()
     [,1] [,2]
[1,]    1    3
[2,]    2    4
> cacheSolve(x)
     [,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5
> cacheSolve(x)
getting cached data.
     [,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5
