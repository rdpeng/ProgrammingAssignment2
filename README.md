### Introduction

### Part 1: Caching the Mean of a Matrix



1.  set the value of the matrix
2.  get the value of the matrix
3.  set the value of the inverse
4.  get the value of the inverse

<!-- -->

 makeCacheMatrix <- function(x = matrix()) {
+       m <- NULL
+       set <- function(y) {
+             x <<- y
+             m <<- NULL
+       }
+       get <- function() x
+       setinverse <- function(solve) m <<- solve
+       getinverse <- function() m
+       list(set = set, get = get,
+            setinverse = setinverse,
+            getinverse = getinverse)
+ }

### Part 2: The second function calculates the mean of the special matrix created with the above function. It first checks to see if the inverse has been calvulated. If so, it get the inverse from the cache and skips the computation. Otherwise, it calcultes the inverse of the matrix and sets the value of the inverse in the cache via the setinverse function. 


> cacheSolve <- function(x, ...) {
+       m <- x$getinverse()
+       if(!is.null(m)) {
+             message("getting cached data")
+             return(m)
+       }
+       data <- x$get()
+       m <- solve(data, ...)
+       x$setinverse(m)
+       m
+ }

### Part 3: Solve answer
a<-diag(9,4)
> a
     [,1] [,2] [,3] [,4]
[1,]    9    0    0    0
[2,]    0    9    0    0
[3,]    0    0    9    0
[4,]    0    0    0    9
CachedMarix <- makeCacheMatrix(a)
> 
> cacheSolve(CachedMarix)
          [,1]      [,2]      [,3]      [,4]
[1,] 0.1111111 0.0000000 0.0000000 0.0000000
[2,] 0.0000000 0.1111111 0.0000000 0.0000000
[3,] 0.0000000 0.0000000 0.1111111 0.0000000
[4,] 0.0000000 0.0000000 0.0000000 0.1111111
 

