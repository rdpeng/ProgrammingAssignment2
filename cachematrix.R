##Creating the matrix
##Setting up own variables for personal reference
##creating a function "transpre" to solved value as null
createapattern <- function(x = matrix(sample(1:100,9),3,3)){
  transpre <- NULL
  crea <- function(y) {
    x <<- y
    transpre <<- NULL ## If the function is empty if the calculations has not cast
  }
  obtai <- function() x ## I used "obtai" variable to get the following calculations
  setthetransposed <- function(transposed) {transpre <<- transposed} ##As you can see the "mean" is changed to "transposed"
  gettheretroverted <- function() transpre
  list(crea = crea, obtai = obtai, setthetransposed = setthetransposed, gettheretroverted = gettheretroverted) ##List of my own variables created
}
##As i said earlier all "mean" function are changed to "transposed" which is "m" to "transpre"
cacheTransposed <- function(x, ...) { ##Now to calculate the reversed retroverted numeric patterns
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
##End of statement and repeats to the top
##That's all