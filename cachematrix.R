## Put comments here that give an overall description of what your
## functions do

#####
## Below are 2 functions, it's to get the inverse value of a matrix from a cache
## if possible

## the first function is called makeCacheMatrix, used to prepare the matrix
## and the functions inside

## the second function is called cacheSolve, used to get the inverse value if possible
## or calculate the inverse value if it's not exist yet

### To make you more understand about what actually happen here, i suggest read this url
### https://stackoverflow.com/questions/24904683/caching-the-mean-of-a-vector-in-r

#####

## Write a short comment describing this function
## makeCacheMatrix function is a setter and getter function for a matrix and it's inverse
## you have to create the matrix using this function to be able to use the cacheSolve function

## example : 
## x <- matrix(1:4,2,2)
## my_matrix <- makeCacheMatrix(x)

## or, you can directly do it this way
## y <- makeCacheMatrix(matrix(1:4,2,2))

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL # intialize the inverse, so it won't return error as long as the input is invertible
  set <- function(y) { # define the set function.
    x <<- y # setting y value into the parent variable x
    i <<- NULL # setting null value into the parent variable i. It's initialize the i value
  }
  get <- function() x # get the x value
  setinverse <- function(inverse) i <<- inverse # set the inverse value into the parent variable i
  getinverse <- function() i # get the i (inverse) value
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse) # return all of the function, so it can be called using the '$' sign
}


## Write a short comment describing this function
## This function will get the inverse of the matrix that's defined using makeCacheMatrix
## If the inverse is not cached yet, it'll calculate it first before return the value

## Note that, the inverse cache is actually done in this function,
## rather than at the makeCacheMatrix function.

## So, if this function is never called,
## the inverse value that's got from the $getinverse() will return NULL

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
