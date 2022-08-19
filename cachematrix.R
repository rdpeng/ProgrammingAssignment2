##I have created this function that takes a valid input matrix from the user and creates/computes a 
##special matrix.
##(glossary:-

##U_input= valid input matrix entered by user
##invU_input= inverse of said matrix)

makeCacheMatrix <- function(U_input = matrix()) {
  invU_input <- NULL
  set <- function(x) {
    U_input <<- x
    invU_input <<- NULL
  }
  get <- function() U_input
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() invU_input
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

##The second function i created basically caters to what is asked in the question.
##It takes the special matrix and computes its inverse. The cache comes into play,  if the inverse 
## has already been calculated and is unchanged. If said conditions are met, it retrieves required matrix 
##from the cache.


cacheSolve <- function(U_input, ...) {
  invU_input <- U_input$getInverse()
  if(!is.null(invU_input)) {
    message("getting cached data")
    return(invU_input)
  }
  mat_to_invert <- U_input$get()
  invU_input <- solve(mat_to_invert,...)
  U_input$setInverse(invU_input)
  invU_input
}


