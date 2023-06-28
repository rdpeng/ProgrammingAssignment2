## the function below takes a matrix x that we create, and assigns its inverse i to NULL 
## the set function allows to annull the inverse i and replace the matrix x with a new one (y) 
## if y is different from x.
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
        
  get <- function() x    ## the get function retrieves the matrix x from cache 
  setinverse <- function(inverse) i <<- inverse  ## sets the inverse to a new value to be stored in cache 
  getinverse <- function() i      ##obtains the inverse from cache 
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)   ##creates a list of set, get, setinverse and getinverse variables
  
}

## the funcion below takes a matrix as the first argument, and other arguments are passed to it via the ...
cacheSolve <- function(mymatrix, ...) {
  i <- mymatrix$getinverse()  ## i (the inverse) is first assigned the previous inverse value sored in the cache 
  if(!is.null(i)) {
    message("getting cached data")
    return(i)                 ## if i is not NULL the inverse from the cache is retrieved 
  }
  data <- mymatrix$get()      ## the matrix is retrieved from the 'get' part of the list we created before 
  i <- solve(data, ...)       ## i is assigned the inverse value of the matrix that is calculated using the solve function 
  mymatrix$setinverse(i)      ## new value is set to the setinverse part of the list 
  i
}

#TESTING THE PROGRAM 
my_matrix1 <- makeCacheMatrix(matrix(c(1,2,3,4), nrow=2, ncol=2)) ##we create an invertible 2x2 matrix 
class(my_matrix1)              ## class it to check that it is a list and we can use the $ operator on it 
## [1] "list"

my_matrix1$get()               ## check that the matrix is in our list and the output is correct 
##     [,1] [,2]
##[1,]    1    3
##[2,]    2    4

my_matrix1$getinverse()        ## attempt to get the inverse but it is NULL since we haven't calculated it yet 
## NULL

cacheSolve(my_matrix1)         ## now we apply the cacheSolve function and obtain the inverse 
##     [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5

my_matrix1$getinverse()        ## the inverse is now appeared in the list 
##     [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5

my_matrix1$set(matrix(c(5,6,7,8), nrow=2,ncol=2))    ## we create a different matrix 
my_matrix1$get()   ## check the matrix in the list has changed 
##     [,1] [,2]
##[1,]    5    7
##[2,]    6    8

my_matrix1$getinverse()  ## the inverse has been annulled because the new matrix is different 
## NULL

cacheSolve(my_matrix1)   ## apply the cachesolve function again 
##     [,1] [,2]
##[1,]   -4  3.5
##[2,]    3 -2.5

my_matrix$getinverse()
##    [,1] [,2]
##[1,]   -4  3.5
##[2,]    3 -2.5

cacheSolve(my_matrix1) #this time the inverse is retrieved from cache 
#getting cached data 
##    [,1] [,2]
##[1,]   -4  3.5
##[2,]    3 -2.5




