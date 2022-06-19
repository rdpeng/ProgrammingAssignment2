# > makeCacheMatrix() will create a special object (sobj) based on input matrix, necessary to cache its inverse. The inverse matrix is the result of R's solve() function;  
# > sobj is actually a list containing a function to:
#    set the value of the matrix
#    get the value of the matrix
#    set the value of the inverse
#    get the value of the mean

makeCacheMatrix <- function(x = matrix()) {
  s=NULL #store empty solved obj
  set <- function(y) { #initialize and define
    x <<- y
    s <<- NULL
  }
  get <- function() x # x is undefined, so R gets is in parent env
  setsolve <- function(solve) s <<- solve #based on cache mean examples
  getsolve <- function() s
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


# > cacheSolve() calculates the inverse of matrices created with makeCacheMatrix().
# > First it checks if the inverse was calculated before,
# If so, it gets the inverse from the cache, without computation needed.
# If there is no cached inverse, it calculates the inverse of the matrix and then,
# sets the value of the inverse in the cache via the setsolve function

cacheSolve <- function(x, ...) {
  #x should be an object returned by makeCacheMatrix()
  s <- x$getsolve()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s
  ## Return a matrix that is the inverse of 'x'
}

# >Dummy matrices:
# mymatrix<-matrix(1:4, 2,2)
# myothermatrix<-matrix(4:7, 2,2)

# >Test solve function on dummies:
# solve(mymatrix)
# solve(myothermatrix)

# >create matrix cache:
#cacheme<-makeCacheMatrix(mymatrix)
#othercacheme<-makeCacheMatrix(myothermatrix)

# >Run cacheSolve:
#cacheSolve(cacheme)
#cacheSolve(othercacheme)
# 1st run should print the solved matrices
# 2nd run should print the solved matrices BELOW the message
# "getting cached data", AND WITHOUT COMPUTATION, as the inverse matrix should be cached already.



### MY FEELING ABOUT THIS LESSON ###

#This was a bit confusing, but I could understand the makeVector() cachemean() examples very well after reading "Demystifying makeVector" (https://github.com/lgreski/datasciencectacontent/blob/master/markdown/rprog-breakingDownMakeVector.md), from Alan E. Berger's answer in the discussion forum.
#Now I see how inportant this lesson was to understand R's scoping. Thank you all.
