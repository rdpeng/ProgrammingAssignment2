## The functions written below cache the inverse of a matrix.

## The function makeCacheMatrix creates a matrix object that caches its inverse.

makeCacheMatrix <- function(x = matrix()){ ## defines argument as a matrix
inv <- NULL                                ## initialize variable inv as NULL value for inverse of the matrix
set <- function(t){                        ## assigns new value of matrix in parent environment
  x<<-t
  inv <<-NULL                              ## if there is any value, reset its value to NULL
}
get<- function()x                          ## returns the set value of the new matrix
setsolve <- function(solve)inv <<- solve   ## assigns value of inverse matrix to inv 
getsolve <- function()inv                  ## gets the value of inv when called
list(set = set, get = get,                 ## specifies names to function to call with its name using $ operator
     setsolve = setsolve,
     getsolve = getsolve)
}

#created, set, noticed
## The function cacheSolve computes the inverse of the created matrix by the above function. 
## It retrieves the inverse value from cache, if already calculated.  

cacheSolve <- function(x, ...) { ## returns an inverse matrix 
  inv <- x$getsolve()
       if(!is.null(inv)){
         message("getting cached or calculated inversed matrix")
         return(inv)
       }
       mat <- x$get()
       inv <- solve(mat,...)
       x$setsolve(inv)
       inv
}

#Conduct test of the functions
my_matrix <- makeCacheMatrix(matrix(1:4, 2, 2)) #create matrix
my_matrix$get() #display matrix

cacheSolve(my_matrix)
my_matrix$getsolve() #fetch matrix

my_matrix1$set(matrix(c(4,3,2,1),2,2))
my_matrix1$get()
my_matrix1$getsolve()
cacheSolve(my_matrix1)
my_matrix1$getsolve()
