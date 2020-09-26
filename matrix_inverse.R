makecacheMatrix <- function(x = matrix()){
  inv <- null
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function()x
  setInverse <- function(solveMatrix)inv <<-solveMatrix
  getInverse <- function()inv
  list(set = set, get = get, 
       setInverse = setInverse,
       getInverse = getInverse )
}

cacheSolve <- function(x,...)
{
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("Getting Cached Data")
    return (inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setInverse(inv)
  inv
  
}