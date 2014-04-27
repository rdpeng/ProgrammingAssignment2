####################################################################################
# cacheMatrix and cacheSolve Functions
# --------------------------------------
# Author: James B Taylor
# Date : 26/04/2014
#
####################################################################################


# FUNCTION: makeCacheMatrix
# ================
# This function creates a special "matrix" object that can cache its inverse.
# It contains the following nested functions:
# set(x)     - sets a new matrix
# get()      - returns the current matrix
# invert()   - inverts the current matrix (NOT exposed in the list but used internally)
#              NOTE: This function checks to see if the matrix is non-square or 
#              non-singular in which case there is no inverse and in these cases it
#              returns NULL.  Pseudo inverses for non-square matrices are out of scope.
# getInv()   - returns the inverse of the current matrix from cache if previously called
#             for this matrix or calculates on first call.
# checkInv() - returns true if getInv() has been called - the inverse has been cached
#
# Note: inversion happens inside this object as is best practice - see discussion on 
# coursera forums here: https://class.coursera.org/rprog-002/forum/thread?thread_id=576

makeCacheMatrix <- function(y = numeric()) {
  
  inverted <- FALSE
  
  set <- function(x) {
    y <<- x
    inverted <<- FALSE     
  }
  
  get <- function() y
  
  invert <- function(){
    if (nrow(y)!= ncol(y))
    {
      #this isn't a square matrix and has no true inverse - return NULL
      #Note - could call Pseudo inverse (Moore-Penrose) via implementation
      #in corpcor package but not stated as required for the assessment
      #see tinyurl.com/modf6vj
      yInv<<-NULL
      
    }else if (det(y)==0)
    {
      #this is a singular square matrix and hence has no inverse - return NULL
      #see tinyurl.com/modf6vj
      yInv<<-NULL
      
    }else
    {
      #This is a square non-singular matrix - i.e. has an inverse
      #Note - need to apply rounding to account for rounding errors in
      #IEEE floating point implementation 
      yInv <<- round(solve(y),3)
      inverted <<- TRUE
    }
  }
  
  checkInv <- function(){
    inverted 
  }
  
  getInv <- function(){
    if(!inverted)
    {
      invert()
    }
    yInv
  }
  
  list(set = set,
       get = get,
       checkInv = checkInv,
       getInv = getInv)
}

# END OF FUNCTION: makeCacheMatrix()



# FUNCTION: cacheSolve
# ================
# This function computes the inverse of the special "matrix" returned by makeCacheMatrix
# above. If the inverse has already been calculated (and the matrix has not changed), 
# then the cachesolve should retrieve the inverse from the cache.
#
# This function simply calls a function within makeCacheMatrix, which calculates the
# inverse and caches it WITHIN the object - which is best practice
# See this discussion on the coursera forum including posts by Lecturer Roger d Peng:
# https://class.coursera.org/rprog-002/forum/thread?thread_id=576

cacheSolve <- function(x, ...) {
  
  return(x$getInv())
  
}