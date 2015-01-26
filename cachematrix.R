makeCacheMatrix <- function(x = matrix()) {
      #was getting this error
      #Error in m$getmatrix : $ operator is invalid for atomic vectors
      #changed to x=matrix
  m <- NULL 
    # setting to NULL as a placeholder for a future value
  set <- function(y) { 
    x <<- y 
    m <<- NULL
  } 
    #this function sets x to y and sets m to NULL
  get <- function() x #this function doesn't need arguments (), 
                      #it returns x from the parent environment
  setmatrix <- function(solve) m <<- solve #sets solve, m, to solve
  getmatrix <- function() m #returns the inverse matrix, m
  list(set = set, get = get, setmatrix = setmatrix,getmatrix = getmatrix) #returns the functions just defined
}

cacheSolve <- function(x=matrix, ...) {
  m <- x$getmatrix() 
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get() #gets matrix from x and puts it in data
  m <- solve(data) #run solve on data
  x$setmatrix(m) #update m
  m #returns m
    #got a lot of guidance here: https://class.coursera.org/rprog-010/forum/thread?thread_id=1096
    #https://asitarrives.wordpress.com/2014/10/18/understanding-lexical-scoping-in-r-great-guidance-for-community-ta-in-coursera/
}
