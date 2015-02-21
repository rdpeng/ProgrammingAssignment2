## These two functions gives the inverse of a matrix and cache it so it can be retrived later on.
## This saves time as R doesn't have to recalculate the inverse matrix again.
## I have further comments inside the function that provides information on what the different
## parts does. 


makeCacheMatrix <- function(x = matrix())  ## makeCacheMatrix is defined as a function with the formal argument x, and the default of x is a matrix.
{
  xin <- NULL ## defines xin as null
  set <- function(y) {
    x <<- y ## x is defines as y In the global enviroment
    xin <<- NULL ## xin is defined as null in the global enviroment
  }
  get <- function() x ## get is defined as a function that gives x
  setinverse <- function(solve) xin <<- solve ## setinverse is defined as a function that defines xin as "solve" in the global enviroment.
  getinverse <- function() xin ## getinverse defined as a function that gets xin
  list(set = set, get = get,
       getinverse = getinverse,
       setinverse = setinverse) ## creates a list with the above function. This is also the result that will be displayed when running the function
}

cacheSolve <- function(x, ...) {
  xin <- x$getinverse() ## xin is defined with the function getinverse defined in makeCacheMatrix function
  if(!is.null(xin)) {
    message("getting cached data")
    return(xin)
  } ## shows the message ("getting cached data" if xin is NOT null.
  data <- x$get() ## if xin is not null data is defined with the function get from the makeCacheMatrix lidt og functions
  xin<- solve(data, ...) ## xin is defined as the inverse matrix of data ising the solve function
  x$setin(xin) ## x is cached using the setin function from makeCacheMatrix function and Can be uses later on.
  xin ## prints xin
}




