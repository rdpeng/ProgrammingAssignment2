#this function creates a list that contains 4 functions within it. 
#These are set, get, setInv, getInv. 
#These functions are not exposed to the environment outside the main function 
#because the <<- operator is used. 
makeCacheMatrix <- function(x = matrix()) {
  xinv <- NULL #inversion result is stored here
  set <- function(y) {
    x <<- y
    xinv <<- NULL #initializes xinv to null
  }
  
  get <- function() x  #input matrix gets returned
  setInv <- function(inv) xinv <<- inv  #inverse matrix set
  getInv <- function() xinv    #inverse matrix returned
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}

cacheSolve <- function(x, ...) {
  m <- x$getInv() #obtaining inversed matrix from object x
  if(!is.null(m)) {  #if inversion result calculated/exists
    message("getting cached data")
    return(m) #returns the calculated inversion
  }
  data <- x$get() #gets matrix object if 
  m <- solve(data) 
  x$setInv(m) #set it to the object that is solved #(m)
  m #this is where the result is printed
}

test <- matrix(runif(6,1,100),2,2) #generates random square matrix
testCached <- makeCacheMatrix(test) 
cacheSolve(testCached)  # tests the matrix!