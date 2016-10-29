## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) { #initializing  matrix called x
  m <- matrix(nrow=2, ncol=2) #initializing null matrix m
  #m<- list()
  #length(m) <-2^2
  # dim(m) <- c(2,2)
  set <- function(y) {
    x <<- y #assigns value of x in makecachematrix environment to be y
    m <<- matrix(nrow=2,ncol=2) #assigns value of NULL to m in makecachematrix environment - this clears cache if value of x is reset
  }
  get <- function() x #gets x from makecacematrix environment
  setinverse <- function(solve) m <<- solve #assigns solve function to m in Makecachematrix environment
  getinverse <- function() m #retrieves value of m in Makecacematrix environment
  list(set = set, get = get, #outputs a list of functions to parent environment
       setinverse = setinverse, #names each function within list so can access using $ extract operator
       getinverse = getinverse) 
}

## Write a short comment describing this function

cachesolve <- function(x, ...) {#calls matrix x created in makecachematrix
  m <- x$getinverse() #assigns m to be the inverse matrix in getinverse from above
  if(!is.na(m)) { #if m is not NULL then retrieve cached inverse
    message("getting cached data") #output this note
    return(m) #return m from Makecachematrix above
  }
  data <- x$get() #creates a matrix from x which retrieves the numbers in x
  m <- solve(data, ...) #calculates the inverse of data
  x$setinverse(m) #sets the inverse of m
  m #prints m
}
