# A few things to note before we write the code
# Computing the inverse of a Matrix is usually a costly computation 
# It therefore suffices to cache the inverse of a matrix rather than computing it repeatedly
# Below are a pair of functions that cache the inverse of a matrix

makeCacheMatrix <- function (x = matrix()) {
         inverse.temp <- NULL
         set <- function(y) {
                x <<- y
                inverse.temp <<- NULL
  }
         get <- function() x
         setinverse <- function (inverse) inverse.temp <<- inverse
         getinverse <- function () inverse.temp
         list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

# We now need a function to return the inverse of the matrix 
# However, we first check if the inverse has been computed already
# If that is the case, it only returs the result. No need for any calculation
# If not, it finds the inverse and sets the inverse in the cache

cacheSolve <- function(x, ...) {
         inverse.temp <- x$getinverse()
         # If inverse has been computed already,  
         # it prints the message "getting cached data"  
         # and returns the inverse . If not, inverse is computed  
         if (!is.null(inverse.temp)) {
         message ("getting cached data.")
         return (inverse.temp)
  }
         data <- x$get()
         inverse.temp<- solve (data)
         x$setinverse (inverse.temp)
         inverse.temp
}

# (NB)The assignment requires that we assume that the matrix is always invertible.
# My function there assumes that the matrix is always invertible

# Now we try the code to see if it works
# First we set a matrix x and cache its inverse
# We then try compute the inverse of the same matrix a second time.
# We would expect it to retrieve the inverse from the cache

x <- matrix(rnorm(100), 10)
m  <-  makeCacheMatrix(x)
m$get()
cacheSolve(m)
cacheSolve(m) # We observe that it retrieves the inverse from the cache the second time


#################### References ######################
# In this code I have used help from the exmaple on assigment webpage for caching mean of a vector
# https://github.com/rdpeng/ProgrammingAssignment2 
