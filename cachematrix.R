## Put comments here that give an overall description of what your
## functions do
## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## The two functions, makeCacheMatrix and cacheSolve create a special object that 
## stores a matrix and caches its inverse.


## Write a short comment describing this function
## makeCacheMatrix creates a function thatcreates a matrix object that can cache its inverse.



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


## Write a short comment describing this function
## cacheSolve creates a function that computes the inverse of makeCacheMatrix.

cacheSolve <- function(x, ...) {
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

## "m" is a stored matrix 
## x is assigned to set and get the matrix through the function makeCacheMatrix

> m <- matrix(1:4, 2, 2)
> x <- makeCacheMatrix(m)
> x$get()
     [,1] [,2]
[1,]    1    3
[2,]    2    4

## cacheSolve didn't cache the inverse the first time, but on the second time 

> cacheSolve(x)
     [,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5
> cacheSolve(x)
getting cached data.
     [,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5
}
