# Matrix inversion is usually a costly computation and there may be some benefit 
2 # to caching the inverse of a matrix rather than compute it repeatedly. The 
3 # following two functions are used to cache the inverse of a matrix. 
4 

5 # makeCacheMatrix creates a list containing a function to 
6 # 1. set the value of the matrix 
7 # 2. get the value of the matrix 
8 # 3. set the value of inverse of the matrix 
9 # 4. get the value of inverse of the matrix 
10 makeCacheMatrix <- function(x = matrix()) { 
  11     inv <- NULL 
  12     set <- function(y) { 
    13         x <<- y 
    14         inv <<- NULL 
    15     } 
  16     get <- function() x 
  17     setinverse <- function(inverse) inv <<- inverse 
  18     getinverse <- function() inv 
  19     list(set=set, get=get, setinverse=setinverse, getinverse=getinverse) 
  20 } 
21 

22 

23 # The following function returns the inverse of the matrix. It first checks if 
24 # the inverse has already been computed. If so, it gets the result and skips the 
25 # computation. If not, it computes the inverse, sets the value in the cache via 
26 # setinverse function. 
27 

28 # This function assumes that the matrix is always invertible. 
29 cacheSolve <- function(x, ...) { 
  30     inv <- x$getinverse() 
  31     if(!is.null(inv)) { 
    32         message("getting cached data.") 
    33         return(inv) 
    34     } 
  35     data <- x$get() 
  36     inv <- solve(data) 
  37     x$setinverse(inv) 
  38     inv 
  39 } 
40 

41 ## Sample run: 
42 ## > x = rbind(c(1, -1/4), c(-1/4, 1)) 
43 ## > m = makeCacheMatrix(x) 
44 ## > m$get() 
45 ##       [,1]  [,2] 
46 ## [1,]  1.00 -0.25 
47 ## [2,] -0.25  1.00 
48 

49 ## No cache in the first run 
50 ## > cacheSolve(m) 
51 ##           [,1]      [,2] 
52 ## [1,] 1.0666667 0.2666667 
53 ## [2,] 0.2666667 1.0666667 
54 

55 ## Retrieving from the cache in the second run 
56 ## > cacheSolve(m) 
57 ## getting cached data. 
58 ##           [,1]      [,2] 
59 ## [1,] 1.0666667 0.2666667 
60 ## [2,] 0.2666667 1.0666667 
61 ## >  

