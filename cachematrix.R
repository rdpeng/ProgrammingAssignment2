## Week 3 Programming Assignment, Caching the Inverse of a Matrix.
##write an R function is able to cache potentially time-consuming computations
## I used/adapted the makevector/cachemean example in assignment (replaced 
#mean with inverse)


#The first function, makeCacheMatrix creates a special "matrix" object, 
#which is really a list containing a function to:

#1. set the value of the matrix
#2. get the value of the matrix
#3. set the value of the inverse
#4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(solveMatrix) inv <<- solveMatrix
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}


## The following function calculates the inverse of the special "matrix" 
#created with the above function. However, it first checks to see if the 
#inverse has already been calculated. If so, it gets the inverse from the 
#cache and skips the computation. Otherwise, it calculates the inverse of 
#the data and sets the value of the inverse in the cache via the setInverse
#function.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("Getting cached data")
    return(inv)
  }
  data <-x$get()
  inv <- solve(data)
  x$setInverse(inv)
  inv
}
#The following is used to test the functions, from the course discussion forum
#code provided by Course Mentor, Alan E. Berger (many thanks!!).

# # Matrices for testing the R functions 
#  # makeCacheMatrix and cacheSolve
#   > # in the Coursera R Programming Course
#   > #
#   > # First, 
#   > # If you haven't read Leonard Greski's invaluable
#   > # [TIPS] Demystifying makeVector()  Post
#   > # be sure to do so.
#   > #
#   > # A simple matrix m1 with a simple matrix inverse n1
#   > # Define
m1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)
# > m1
# [,1]  [,2]
# [1,]  0.50 -1.00
# [2,] -0.25  0.75
# > 
#   > # You can use m1 to test your 
#   > # makeCacheMatrix and cacheSolve functions.
#   > # Since the grading is done on the correctness of your 
#   > # makeCacheMatrix and cacheSolve functions and your 
#   > # comments on how they work, using this (or some other)   
#   > # test matrix to check your code 
#   > # before submitting it 
#   > # is OK relative to the Coursera Honor Code.
#   > # (Checking code with test cases is always a good idea.)
#m1 was constructed (using very simple linear algebra, so
# > # no references are given, almost surely the examples
#   > # in this post have been given many times before) 
#   > # to have a simple matrix inverse, call it n1.
#   > # This means  m1 %*% n1 (%*% is matrix multiply in R) 
#   > # is the 2 row by 2 column Identity matrix I2
I2 <- matrix(c(1,0,0,1), nrow = 2, ncol = 2)
# > I2
# [,1] [,2]
# [1,]    1    0
# [2,]    0    1
# > 
#   > # And so (by linear algebra) n1 %*% m1 is also equal I2.
#   > # (If n1 is the inverse of m1 then m1 is the inverse of n1.)
#   > # With m1 defined as above, n1 ( the inverse of m1) is
n1 <- matrix(c(6,2,8,4), nrow = 2, ncol = 2)
# > n1
# [,1] [,2]
# [1,]    6    8
# [2,]    2    4
# > 
#   > # Checks:
m1 %*% n1
# [,1] [,2]
# [1,]    1    0
# [2,]    0    1
# > 
#   > n1 %*% m1
# [,1] [,2]
# [1,]    1    0
# [2,]    0    1
# > 
solve(m1)
# [,1] [,2]
# [1,]    6    8
# [2,]    2    4
# > 
#   > solve(n1)
# [,1]  [,2]
# [1,]  0.50 -1.00
# [2,] -0.25  0.75
# > 
#   > # So if you have programmed your functions 
#   > # correctly (in the file cachematrix.R),
#   > # (that, and your comments-explanation of how they work
#   > # are what you are graded on)
#   > # and sourced cachematrix.R so they are 
#   > # available in your R session workspace, then doing 
#   > #
myMatrix_object <- makeCacheMatrix(m1)
# > 
#   > # and then
cacheSolve(myMatrix_object)
#   > 
#   > # should return exactly the matrix n1
#   > cacheSolve(myMatrix_object)
# [,1] [,2]
# [1,]    6    8
# [2,]    2    4
# > 
#   > # calling cacheSolve again should retrieve (not recalculate)
#   > # n1
cacheSolve(myMatrix_object)
# getting cached data
# [,1] [,2]
# [1,]    6    8
# [2,]    2    4
# > 
#   > # you can use the set function to "put in" a new matrix.
#   > # For example n2
n2 <- matrix(c(5/8, -1/8, -7/8, 3/8), nrow = 2, ncol = 2)
myMatrix_object$set(n2)
# > # and obtain its matrix inverse by
cacheSolve(myMatrix_object)
# [,1] [,2]
# [1,]    3    7
# [2,]    1    5
# > 
cacheSolve(myMatrix_object)
# getting cached data
# [,1] [,2]
# [1,]    3    7
# [2,]    1    5
# > 