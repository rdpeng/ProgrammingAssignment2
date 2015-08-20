## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(mat.val = matrix()) {

  # initialization, there is no inversed matrix and it will have to be built.
  #
  inverse.mat <- NULL
  inverse.built <- FALSE
  
  # set a new initial matrix
  #
  set <- function(new.mat.val)
  {
    mat.val <<- new.mat.val
    inverse.mat <<- NULL
    inverse.built <<- FALSE
  }
  
  # get the initial matrix
  #
  get <- function() mat.val
  
  # get the flag which indicates if the inverse matrix has already been built.
  #
  get.inverse.built.flag <- function() inverse.built
  
  # if the above flag indicates a build has alread occurred, return the stored
  # inverse
  #
  get.inverse <- function() inverse.mat
  
  # set the inverse that has been calculated outside the makeCacheMatrix object
  #
  set.inverse <- function( inverted.matrix )
  {
    inverse.mat <<- inverted.matrix
    inverse.built <<- TRUE
  }
  
  list( set = set, get = get, get.inverse.built.flag = get.inverse.built.flag,
        get.inverse = get.inverse, set.inverse = set.inverse )
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  resp.mat <- NULL
  
  if( x$get.inverse.built.flag() == FALSE )
  {
    # we need to build the inverse
    # 1. grab the matrix
    # 2. run the solve
    # 3. store it
    #
    x$set.inverse( solve( x$get() ) )
    resp.mat <- x$get.inverse()
  }
  
  ## Return a matrix that is the inverse of 'x'
  resp.mat <- x$get.inverse()
}
