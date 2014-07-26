#create a function that will create a complex matrix object that can cache its inverse
makeCacheMatrix<-function(x=matrix())
{
  #outer environment variable that cache's inverse of matrix
  inv<-NULL
  #function to set the value of the matrix and the variable of outer environment
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  #function to get the value of the matrix
  get <- function() x
  
  #function to set the inverse of the matrix to the outer environment variable
  setinv <- function(inverse) inv <<- inverse
  
  #function to get the inverse of the matrix from the outer
  getinv <- function() inv
  
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

#function definition that computes inverse of the matrix returned by the function above
#It checks whether the cache is not empty and also checks if the matrix stord in cache
#is the inverse of the matrix input by checking whether their multiplication returns an identity matrix
#if it returns an identity matrix, it returns the matrix inverse from the cache otherwise
#it computes inverse using solve function

cacheSolve <- function(x, ...) 
{
  #check whether cache has the right inverse matrix by multiplying with inverse matrix and checking for identity matrix
  inv <- x$getinv()
  if(!is.null(inv) & det(inv%*%x)==1) 
  {
    message("getting cached data")
    return(inv)
  }
  else
  {
    #compute inverse if cache is outdated or empty and set it in cache
    data <- x$get()
    inv <- solve(data)
    x$setinv(inv)
    m

  #compute inverse if cache is outdated or empty and set it in cache
  data <- x$get()
  inv <- solve(data)
  x$setinv(inv)
  m

  }
}
