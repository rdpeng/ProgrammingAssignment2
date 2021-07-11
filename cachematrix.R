##Creating the matrix
##Setting up own variables for personel reference
##creating a function "transpre" to solved value as null
createCacheMatrix <- function(x = matrix(sample(1:100,9),3,3)){
  transpre <- NULL
  crea <- function(y) {
    x <<- y
    transpre <<- NULL ## If the function is empty if the calculations has not cast
  }
  obtai <- function() x
  setthetransposed <- function(transposed) {transpre <<- transposed} ##As you can see the "mean" is changed to "inverse"
  gettheretroverted <- function() transpre
  list(crea = crea, obtai = obtai, setthetransposed = setthetransposed, gettheretroverted = gettheretroverted)
}
##As i said earlier all "mean" function are changed to "inverse" which is "m" to "transpre"
cacheTransposed <- function(x, ...) {
  transpre <- x$gettheretroverted()
  if(!is.null(transpre)) {
    message("calculating the inversed numeric") ##This message will appear when it tries to load the retroverted number and returns to the statement
    return(transpre)
  }
  data <- x$obtai()
  transpre <- solve(data, ...)
  x$setthetransposed(transpre)
  transpre
}
##End of statement