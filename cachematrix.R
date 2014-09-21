## Put comments here that give an overall description of what your
## functions do : Function 1 -- makeCacheMatrix()----it creates a 
+## special “matrix” object that can cache its inverse.
+## Function 2 -- cacheSolve()----it computes the inverse of the “matrix”
+## returned by makeCacheMatrix(). If the inverse has already been calculated
+## and the matrix has not changed, it’ll retrieve the inverse from the cache
+## directly.
+## Function 3 -- test()----takes in any invertible matrix, calculates its inverse
+## twice using the above functions, and prints out the times it takes for both runs.
+## The first run should take longer than the second because it actually calculates 
+## the inverse while the second run only does a look-up from the cache.
 
-## Write a short comment describing this function
+## FUNCTION 1
 
 makeCacheMatrix <- function(x = matrix()) {
+inv = NULL
+set = function(y) {
+x <<- y
+inv <<- NULL
+}
+get = function() x
+setinv = function(inverse) inv <<- inv
+getinv = function() inv
+list(set = set, get = get, setinv = setinv, getinv = getinv)
 
 }
 
 
-## Write a short comment describing this function
+## FUNCTION 2
 
 cacheSolve <- function(x, ...) {
-        ## Return a matrix that is the inverse of 'x'
+inv = x$getinv()
+if(!is.null(inv)){
+message("getting cached data")
+return(inv)
+}
+mat.data = x$get()
+inv = solve(mat.data,...)
+x$setinv(inv)
+return(inv)
 }
+
+## FUNCTION 3
+
+test = function(mat){
+temp = makeCacheMatrix(mat)
+start.time = Sys.time()
+cacheSolve(temp)
+dur = Sys.time() - start.time
+print(dur)
+start.time = Sys.time()
+cacheSolve(temp)
+dur = Sys.time() - start.time
+print(dur)
+}
+
+### Sample output as an example after sourcing the above file into R console ::
+### Input lines typed by us in the R console :-
+##  source("xyz.R")     ('xyz' being the placeholder for the R file name in the working directory)
+##  set.seed(1110201)
+##  r = rnorm(1000000)
+##  mat1 = matrix(r, nrow=1000, ncol=1000)
+##  test(mat1)
+
+### Output that we get :-
+##  Time difference of 't1' secs
+##  Time difference of 't2' secs
+## We notice that t2<t1 i.e.; the first run takes longer than the second 
+## because it actually calculates the inverse while the second run only does a simple look up from the cache.
