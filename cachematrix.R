## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

#This function is created to a super matrix that can keep
#the content of the matrix and the inverse.Hence it has two variables 
#the matrix stored as x and the inv variable that is the inverse of matrix
#Each of them with two functions, set(Creation and manipulation of sets).
#and get(Return The Value Of A Named Object).

#the matrix has to be a square matrix


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) inv <<- solve
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

##testing 


matriz <- makeCacheMatrix()
matriz$set(matrix(1:25, 5, 5))
matriz$get()


## Write a short comment describing this function

##### the next function determine the inverse matrix returned by the previous function


cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setInverse(inv)
  inv      
}

#testing 

mtx1 <- matrix(1:25, 5, 5) ##creating the matrix 
mtx2 <- makeCacheMatrix(m)## storing the  matrix 

cacheSolve(mtx2) ##getting matrix inverse  














