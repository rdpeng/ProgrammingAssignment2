makecachematrix <- function(x = matrix()) ## Assign a matrix to funtion makecachematrix
  { 
  m <- NULL
  set <- function(y) 
    {
    x <<- y
    m <<- NULL
  } ## Two functions(set and get) were used to assign the values of matrix  and two other functions (setInverse and Getinverse were assinged to calculate inverse of thr matrix
  get <- function() x
  setInverse <- function(Inverse) m <<- Inverse
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)}
  ##list is created to assign all the elements of the matrix
  
cachesolve <- function(x, ...) ## cachesolve returns the value ie inverse of the matrix
  {
    m <- x$getInverse()
    if(!is.null(m)) {
      print("cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInverse(m)
    m
  }
