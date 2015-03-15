## We define two functions: makeCacheMatrix and cacheSolve
## 1) makeCacheMatrix(x):
##    creates a special matrix as a list from the matrix x object with methods:
##    get, set, get_inverse,and set_inverse
##
## 2) cacheSolve(x,...):
##    returns the inverse of a matrix stored in the environment of the special
##    list x create by calling makeCacheMatrix, and stores the inverse in
##    the environment of x for future retrieval if x has not changed.



##---------------------------- makeCacheMatrix(x) ---------------------------------
## This function creates a special "matrix" that can cache its inverse.
## It returns a list containing methods: get, set, get_inverse,and set_inverse
##          get(): Extract the value of the matrix x
##  get_inverse(): Extract the inverse of x stored in the environment of the
##                 special matrix created by calling makeCacheMatrix
##          set(): Changes the value of the cached matrix and reset the cached
##                 inverse to NULL
##  set_inverse(): Caches the value of the inverse of x by direct assignment

makeCacheMatrix <- function(x = matrix()) {
      # Create an variable that holds the cached inverse of the matrix x. Its is
      # initialized as NULL on creation of the new matrix object
      x_inv <- NULL

      # Extracts the value of a matrix from which the special matrix was created
      get <- function(){
            x
      }

      # Define the value of a special matrix object add set its inverse to NULL
      set <- function(y){
            x <<- y
            x_inv <<- NULL
            }

      # Adds the inverse of X to the special matrix object created from X
      set_inverse <- function(xinv){
            x_inv <<- xinv
      }

      # Retrieves the inverse X from the special matrix created from X
      get_inverse <- function(){
            x_inv
      }

      # Return list of methods that can be called on the special matrix
      list(set = set, set_inverse = set_inverse,
           get = get, get_inverse = get_inverse)
}

##------------------------------ cacheSolve ------------------------------------
## This function takes a special matrix, checks if its inverse is available in
## the environment of the special matrix. Otherwise, it computes the inverse of
## the matrix attached to the special matrix and attaches it to its environment
##
## ------ THOUGH I THINK THE NAME OF THE FUNCTION IS MISLEADING ----------------
## -------- cacheInverse is a better name based of the instructions ------------

cacheSolve <- function(x=makeCacheMatrix(), ...) {
      ## Define special matrix names
      sm_names = c("get", "get_inverse","set", "set_inverse")

      # Check that the argument x is a list with names as above
      if ( !is.list(x) ){
            stop("The mandatory input x must be a list with names:
                 get, get_names, set, set_names")
      }else if( !all(names(x) %in% sm_names) ){
            stop("The mandatory input x must be a list with names:
                 get, get_names, set, set_names")
      }
      # Check that the inverse has not been cached
      x_inv <- x$get_inverse()
      if ( is.null(x_inv) ){
            # Get the matrix stored in the environment of x
            x_mat <- x$get()

            # Cache matrix inverse if values are defined otherwise stop and
            # display a message notifying the user of the missing values issue
            if ( any(is.na(x_mat)) ){
                  stop("Please set all values of x. There were missing values!")
            }

            # Compute inverse and store it in the environment of x
            x_inv <-solve(x_mat)
            x$set_inverse(x_inv)
      }else{
            message("Returning inverse from cache")
      }
      ## Return a matrix that is the inverse of 'x'
      return(x_inv)
}
