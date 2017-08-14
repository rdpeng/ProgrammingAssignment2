My testscript:

# In the readme file I did the runcode and the test results.

# The first function, makecacheMatrix creates a special "matrix", which is really a list containing a function to

# set the value of the matrix
# get the value of the matrix
# set the value of the inverse
# get the value of the inverse
# the new function is structured according the MakeVector function in the 
# assignment example

makeCacheMatrix <- function(Input_Matrix = numeric()) {
  
  # holds the cached inversed matrix or NULL if nothing is cached yet
  # initially nothing is cached that is why the variable Inverse_Matrix it is set to NULL
  Inverse_Matrix <- NULL
  
  # store a matrix
  
  setMatrix <- function(newMatrix) {
    Input_Matrix <<- newMatrix
    # since the matrix is assigned a new matrix, drop the Inverse_Matrix
    Inverse_Matrix <<- NULL
  }
  
  
  # returns the input matrix
  getMatrix <- function() {
    Input_Matrix
  }
  
  
  # cache inverse matrix 
  cacheInverse <- function(solve) {
    Inverse_Matrix <<- solve
  }
  
  
  # get the cached inverse matrix
  getInverse <- function() {
    Inverse_Matrix
  }
  
  # return a list. Each named element of the list is a function
  list(setMatrix = setMatrix, getMatrix = getMatrix, cacheInverse = cacheInverse, getInverse = getInverse)
}




# The following cacheSolve function gets the inverse out of cache or calculates the inverse if not available

cacheSolve <- function(Inverse_Matrix, ...) {
  # get the cached matrix
  inverse <- Inverse_Matrix$getInverse()
  # if a cached matrix exists return it
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  # otherwise get the matrix, caclulate the inverse and store it in
  # the cache
  data <- Inverse_Matrix$getMatrix()
  inverse <- solve(data)
  Inverse_Matrix$cacheInverse(inverse)
  
  # return the calculated matrix
  inverse
}

The result in the console after running the script:

> # create a 4 by 4 matrix and store it in my_matrix
> # print my_matrix for test reasons 
> # calculate and print the inverse for test reasons
> my_matrix <- matrix(c(1,2,8,6,5,4,9,12,11,45,16,13,15,1,3,4), nrow = 4, ncol = 4)
> my_matrix
     [,1] [,2] [,3] [,4]
[1,]    1    5   11   15
[2,]    2    4   45    1
[3,]    8    9   16    3
[4,]    6   12   13    4
> # the result should be:
> #      [,1] [,2] [,3] [,4]
> # [1,]    1    5   11   15
> # [2,]    2    4   45    1
> # [3,]    8    9   16    3
> # [4,]    6   12   13    4
> 
> my_inverse <- solve(my_matrix)
> my_inverse
              [,1]         [,2]          [,3]         [,4]
[1,] -0.0010922277 -0.043689108  0.2852461881 -0.198916510
[2,] -0.0249901700  0.000393202 -0.1535672157  0.208790249
[3,]  0.0006116475  0.024465901  0.0002621346 -0.008606754
[4,]  0.0746209970 -0.015160121  0.0319804273 -0.050024029
> 
> # call function makeCacheMatrix and place the result in my_result
> my_result <- makeCacheMatrix(my_matrix);
> 
> summary(my_result);
             Length Class  Mode    
setMatrix    1      -none- function
getMatrix    1      -none- function
cacheInverse 1      -none- function
getInverse   1      -none- function
> #>              Length Class  Mode    
> #> setMatrix    1      -none- function
> #> getMatrix    1      -none- function
> #> cacheInverse 1      -none- function
> #> getInverse   1      -none- function
> 
> my_result$getMatrix();
     [,1] [,2] [,3] [,4]
[1,]    1    5   11   15
[2,]    2    4   45    1
[3,]    8    9   16    3
[4,]    6   12   13    4
> # the result should be:
> #      [,1] [,2] [,3] [,4]
> # [1,]    1    5   11   15
> # [2,]    2    4   45    1
> # [3,]    8    9   16    3
> # [4,]    6   12   13    4
> cacheSolve(my_result)
              [,1]         [,2]          [,3]         [,4]
[1,] -0.0010922277 -0.043689108  0.2852461881 -0.198916510
[2,] -0.0249901700  0.000393202 -0.1535672157  0.208790249
[3,]  0.0006116475  0.024465901  0.0002621346 -0.008606754
[4,]  0.0746209970 -0.015160121  0.0319804273 -0.050024029
> # the result should be the inverse matrix:
> #              [,1]         [,2]          [,3]         [,4]
> # [1,] -0.0010922277 -0.043689108  0.2852461881 -0.198916510
> # [2,] -0.0249901700  0.000393202 -0.1535672157  0.208790249
> # [3,]  0.0006116475  0.024465901  0.0002621346 -0.008606754
> # [4,]  0.0746209970 -0.015160121  0.0319804273 -0.050024029
> 
> # the 2nd time we run the function,we get the cached value
> cacheSolve(my_result)
getting cached data
              [,1]         [,2]          [,3]         [,4]
[1,] -0.0010922277 -0.043689108  0.2852461881 -0.198916510
[2,] -0.0249901700  0.000393202 -0.1535672157  0.208790249
[3,]  0.0006116475  0.024465901  0.0002621346 -0.008606754
[4,]  0.0746209970 -0.015160121  0.0319804273 -0.050024029
> # the result should be:
> #> getting cached data (extra print line)
> #              [,1]         [,2]          [,3]         [,4]
> # [1,] -0.0010922277 -0.043689108  0.2852461881 -0.198916510
> # [2,] -0.0249901700  0.000393202 -0.1535672157  0.208790249
> # [3,]  0.0006116475  0.024465901  0.0002621346 -0.008606754
> # [4,]  0.0746209970 -0.015160121  0.0319804273 -0.050024029
> 

