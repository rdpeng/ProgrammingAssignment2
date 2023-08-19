> # x is matrix, v is mean value NULL
> makeCacheMatrix <- function(x = matrix()) {
+   v <- NULL
+   set <- function(y){
+     x <<- y
+     v <<- NULL
+   }
+   get <- function()x
+   setInverse <- function(inverse) v <<- inverse
+   getInverse <- function() v 
+   list(set = set, get = get, 
+   setInverse = setInverse, 
+   getInverse = getInverse)
+ }
> cacheSolve <- function(x, ...) {
+   v <- x$getInverse()
+   if(!is.null(v)){
+     message("getting cached data")
+     return(v)
+   }
+   mat <- x$get()
+   v <- solve(mat,...)
+   x$setInverse(v)
+   v
+ }
> #Check calculation
> x<- makeCacheMatrix(matrix(1:8, 2, 2))
> x$get()
     [,1] [,2]
[1,]    1    3
[2,]    2    4
> x$getInverse()
NULL
> cacheSolve(x)
     [,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5
> x$getInverse()
     [,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5
