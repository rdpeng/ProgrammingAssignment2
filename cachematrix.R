# R code for coursera, Programming Assignment 2 
# Created by Zoltan Kovacs on 2015-09-24, Budapest, Hungary

# -------------------------------------------------------
# # How to use this R code 
# -------------------------------------------------------
# # 1. Firstly create a matrix e.g. 'zMat'
# # assuming that the matrix is invertible
# zMat = matrix( c(1,2,3,4),nrow=2,ncol=2,byrow = TRUE)
# 
# # 2. Create the environment as a new variable
# zCachedMat = makeCacheMatrix(zMat)
# 
# # 3. Calculate inverse matrix first time, so 
# #   "getting cached data" won't appear
# cacheSolve(zCachedMat)
# 
# # 4. Calculate inverse matrix any time after this, the 
# #   "getting cached data" will appear, because it is
# #   already in memory as a part of the environment variable
# cacheSolve(zCachedMat)


#-------------------------------------------------------
## Create an environment variable for 
## calculating the inverse of a matrix
#-------------------------------------------------------
makeCacheMatrix <- function(mat = matrix()) {
  # Reset inverse matrix
  inv <- NULL
  
  # Set new matrix for the environment
  set <- function(newMat) {
    mat <- newMat
    inv <<- NULL
    }
  
  # Return the matrix of environment
  get <- function() mat
  
  # Solve matrix
  setSolve <- function(solve) inv <<- solve
  
  # Return the inverse matrix of environment
  getSolve <- function() inv
  
  # Return the inner functions (methods) of env. as list 
  list(set = set, get = get,
       setSolve = setSolve,
       getSolve = getSolve)
  }


#-------------------------------------------------------
## Return inverse matrix of 'mat'. It calculates the
## inverse matrix, if it has not been calculated so far
#-------------------------------------------------------
cacheSolve <- function(mat, ...) {
  
  # Get the inverse of matrix
  inv <- mat$getSolve()
  
  # If it is not null, then return it 
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  # Get the matrix of environment as a variable
  data <- mat$get()
  
  # Calculate inverse matrix
  inv <- solve(data, ...)
  
  # Set inverse in environment
  mat$setSolve(inv)
  
  # Return value
  inv
}
