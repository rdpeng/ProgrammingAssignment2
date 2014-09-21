
+## The first function, makeCacheMatrix, creates a list containing a function to:
+ # set the the matrix
+ # get the the matrix
+ # set the the inverse
+ # get the the inverse
+ # It caches the result (via the "<<-" operator) in the global environment for use in the upcoming cacheSolve function.  
 
+makeCacheMatrix <- function(x = matrix()) {
+   m <- NULL  #matrix inversion results are stored in this variable
+   set <- function(y) {
+     x <<- y  #set result to global environment
+     m <<- NULL #reset the results
+   }
+   
+   # get the original matrix
+   get <- function() x
+ 
+   setinverse <- function(solve) m <<- solve
+   
+   getinverse <- function() m 
+   
+   list(set = set, get = get,
+        setinverse = setinverse,
+        getinverse = getinverse)  
 }
 

+## Function cacheSolve calculates the inverse of a computable matrix.
+ # It first checks if the inverse has been calculated and if so, it directly gets that result.If not 
+ # it will then solve the matrix via setinverse. 
 
 cacheSolve <- function(x, ...) {
+    m <- x$getinverse()
+   # check if m has been calculated from above
+   if(!is.null(m)) {
+     message("getting cached data")
+     return(m)
+   }
+   data <- x$get()
+   m <- solve(data, ...)   
+   x$setinverse(m)
+   m    
 }
