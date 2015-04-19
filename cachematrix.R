
Square Matrix

A matrix with equal numbers of rows and columns.

http://www.emathhelp.net/calculators/linear-algebra/inverse-of-matrix-calculator/?i=%5B%5B1%2C-0.25%2C1%2C2%5D%2C%5B-0.25%2C1%2C2%2C1%5D%2C%5B2%2C3%2C4%2C3%5D%2C%5B2%2C1%2C3%2C1%5D%5D&steps=on


makeCacheMatrix <- function(x = matrix()) {
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
> 
> x = rbind(c(1, -1/4), c(-1/4, 1))
> m = makeCacheMatrix(x)
> m$get()
      [,1]  [,2]
[1,]  1.00 -0.25
[2,] -0.25  1.00
> cacheSolve(m)
          [,1]      [,2]
[1,] 1.0666667 0.2666667
[2,] 0.2666667 1.0666667


> x = rbind(c(1, -1/4, 1, 2), c(-1/4, 1, 2, 1), c(2, 3, 4, 3),c(2, 1, 3, 1))
> m = makeCacheMatrix(x)
> m$get()
      [,1]  [,2] [,3] [,4]
[1,]  1.00 -0.25    1    2
[2,] -0.25  1.00    2    1
[3,]  2.00  3.00    4    3
[4,]  2.00  1.00    3    1


> cacheSolve(m)
           [,1]        [,2]       [,3]       [,4]
[1,]  0.0000000 -0.68965517  0.1379310  0.2758621
[2,] -0.4444444 -0.30651341  0.5057471 -0.3218391
[3,]  0.0000000  0.55172414 -0.3103448  0.3793103
[4,]  0.4444444  0.03065134  0.1494253 -0.3678161


> x = rbind(c(1, -1/4), c(-1/4, 1))
> m = makeCacheMatrix(x)
> m$get()
      [,1]  [,2]
[1,]  1.00 -0.25
[2,] -0.25  1.00
> cacheSolve(m)
          [,1]      [,2]
[1,] 1.0666667 0.2666667
[2,] 0.2666667 1.0666667
> 
