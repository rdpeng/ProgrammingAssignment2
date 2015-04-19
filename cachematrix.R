Description of the makeCacheMatrix and cacheSolve functions

The two functions together allow to create first a square matrix 
and later calculate automatically its inverse.

A square matrix is a matrix with equal numbers of rows and columns.

A web based calculator for creating inverse matrices can be found here:
http://www.emathhelp.net/calculators/linear-algebra/inverse-of-matrix-calculator/

Instructions on how to test my functions:
1. 	Run in R the makeCacheMatrix first in the console.

2. 	Run in R the cacheSolve function in the console.

3. 	Enter the data for a square matrix and test data entry.
	For example a 2 by 2 matrix can be entered as follows
	> x = rbind(c(1, -1/4), c(-1/4, 1))

4. 	Launch the makeCacheMatrix function:
	m = makeCacheMatrix(x)

5. 	Test that the result is a matrix with:
	m$get()

6.  Result for the 2 by 2 matrix data above is as follows
	      [,1]  [,2]
	[1,]  1.00 -0.25
	[2,] -0.25  1.00

7. 	Launch the cacheSolve function:
	cacheSolve(m)	

8. 	Check result:
	For the example above the inverse matrix is:
          [,1]      [,2]
[1,] 1.0666667 0.2666667
[2,] 0.2666667 1.0666667

9. 	Test everything in the web based inverted matrix calculator above
	to check if the results from R are correct.

10. Redo everything with a 4 by 4 matrix:
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
	
11. Or redo everything again with the 2 by 2 matrix again:
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

12. Bingo.



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
