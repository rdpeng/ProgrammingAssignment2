## This program is about Caching a Inverse of a non-singular square matrix.
## The results will be stored as cache and can be restored if needed. So, we don't have to compute the inverse of matri if we 
#previously computed it. 

makeCacheMatrix <- function(x = matrix()) {
# This function creates a matrix objects that can cache it's inverse.
   I <- NULL   # Here we have initialize I(inverse of matrix) to be NULL.
     set <- function(z) {
        x <<- z
           I <<- NULL }
#############################################################
   get <- function() x
       setInverse <- function(Inverse) I <<- Inverse
          getInverse <- function() I
  list(set = set, get = get,
         setInverse = setInverse,
             getInverse = getInverse)
}
#################################################################
# So far, we have created a function that creates a matrix. Now next group of codes will
# first check whether the inverse of matrix already computed or not. if it is, the program 
# will skip the execution and show the result. But, if the inverse of matrix not already computed
# it will compute the inverse and show the result.
              


cacheSolve <- function(x, ...) {  
#cacheSolve will compute the inverse of matrix.
      I <- x$getInverse()
        if(!is.null(I)){
           message("getting cached Data")
               return(I)
}
      matrix <- x$get()
          I <- solve(matrix, ...)
             x$setInverse(I)
                   I
} 
 
#Post Processing
#my_matrix <- makeCacheMatrix(matrix(1:4, 3,3)), This will create 3 by 3 matrix whose element is 1 to 4 and stored in my_matrix.
#my_matrix$get(), This will return your matrix and you can see how it looks like.
#cacheSolve(my_matrix), This will compute inverse of matrix. If we repeat this again, we will see that program will load cached result.


