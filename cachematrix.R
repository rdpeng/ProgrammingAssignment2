## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

}

# Data = a matrix which was created by manipulating data which is 
  # supplied by the user.
  Matrix <- NULL
  
  # ----------------------------------------------
  # ----------------------------------------------
  ##########
  ##PUBLIC##
  ##########
  ##This function allows the user to cache the data and a matrix which can be 
  ##to store one manipulated form of the data.
  set <- function(y){
    x      <<- y
    Matrix <<- NULL
  }
  
  ##This function allows the user to view the data indirectly.
  get       <- function() {return(x)}
  
  ##These functions are used to manipulate the data and to allow the user 
  ##to view that manipulated data indirectly.
  set_Matrix <- function(InvM){Matrix <<- InvM}
  get_Matrix <- function(    ){return(Matrix) }
  
  # ----------------------------------------------
  # ----------------------------------------------
  return(list(set = set, get = get, 
       set_Matrix = set_Matrix, 
       get_Matrix = get_Matrix))
}
## Write a short comment describing this function

cacheSolve <- function(x, ...) {
# This function makes use of the functions and storing abilities defined
  # by the class (makeCachMatrix).
  ##INPUT:
  #       x   = The name of the function with the dataset included. 
  #       ... = Other variables which can be used in the solve() function. 
  ##OUTPUT:
  #       Return InvM = a matrix that is the inverse of 'x'
  
  ##Check to see whether the inverted matrix for the chosen matrix has 
  ##already been calculated. If it has then return it and do not bother 
  ##recalculating the inverted matrix again.
  InvM <- x$get_Matrix()
  if(!is.null(InvM)) {
    message("Returning the cached inverted matrix as is has already been calculated.")
    return(InvM)
  }
  
  ##If the inverted matrix has not been calculated then calculate it from the user
  ##supplied data <<solve>> function. Then cache this inverted matrix for further 
  ##potential use.
  data <- x$get()
  InvM <- solve(data, ...)
  x$set_Matrix (InvM)
  
  return(InvM)
}

        ## Return a matrix that is the inverse of 'x'
}
