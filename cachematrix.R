## This R script contains 2 functions, used to create an inverted matrix and cache it
## Use: Set a variable using makeCacheMatrix with the original matrix as parameter
## then use cacheSolve with the variable set above as a parameter to return the
## inverted matrix (from cache, if stored there)

## The functions assume that the input (parameter) received is valid
## i.e., an invertible, square matrix
## An improved version of this script could explicity check for this
## and return feedback accordingly


## makeCacheMatrix
## makeCacheMatrix creates a list of functions called by cacheSolve

makeCacheMatrix <- function(x = matrix()) {

## The <<- assigment operator is behind the "magic",
## causing the search to run though parent enviroment.
## See ?assignOps for details.
   
   invertedMatrix <- NULL

   set <- function(y) {
      x <<- y
      invertedMatrix <<- NULL
   }

   get = function() x

   setInvertedMatrix <- function(inverse) invertedMatrix <<- inverse 

   getInvertedMatrix <- function() invertedMatrix

   ## Return a list containing the functions defined above
   list(
      set = set,
      get = get,
      setInvertedMatrix = setInvertedMatrix,
      getInvertedMatrix = getInvertedMatrix
   )
   
}


## cacheSolve
## cacheSolve returns an inverted matrix,
## which is either created or obtained from cache 
cacheSolve <- function(x, ...) {

   invertedMatrix = x$getInvertedMatrix()
   
   ## Test whether the variable inverseMatrix has a value
   if (!is.null(invertedMatrix)){

      ## If there is a value, get it from cache (prepending a message)
      ## return exits and skips the execution of the rest of the function 
      message("Getting cached matrix data:")
      return(invertedMatrix)
   }
   
   ## If there is not already a value, calculate it using the solve function 
   matrix.data = x$get()
   invertedMatrix = solve(matrix.data, ...)
   
   ## Set the value of the inverse matrix in the cache via the setInvertedMatrix function.
   x$setInvertedMatrix(invertedMatrix)
   
   return(invertedMatrix)
}