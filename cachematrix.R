## The first function makeCacheMatrix(x = matrix()) returns a list containing 4 functions 
## The 4 functions are:
## 1. "set_mat(y)" : defines a function to set the matrix "x" to a new matrix "y" and
## resets the inverse of the matrix "inv_mat" to an empty (NA's) matrix 
## 2. "get_mat()" : defines a function to return the matrix "x"
## 3. "set_inv_mat(any_mat)" : defines a function to set the inverse of the matrix "inv_mat" to "any_mat"
## 4. "get_inv_mat()" : defines a function to return the inverse of the matrix "inv_mat"  
## list(set_mat = set_mat, get_mat = get_mat,set_inv_mat = set_inv_mat, get_inv_mat = get_inv_mat) 
## returns the list containing all these functions

makeCacheMatrix <- function(x = matrix()){
        inv_mat <- matrix()
        set_mat <- function(y) { 
                
                x <<- y
                inv_mat <- matrix()
        }
        ## When we use double arrow assignment ( i.e. <<- )in x<<-y, 
        ## the values of y gets assigned to x only for the closure ( the list v1 resturend by "v1 <- makeCacheMatrix()" ) 
        ## under which the set_mat(y) function was used
        ## for other closures if set_mat(y) is not used then get_mat() will result in an empty matrix 
        ## as "x" is not initialized                  
        
        get_mat <- function()x 
        
        ## the get_mat() function will return a non-empty matrix only is set_mat(y) has been called 
        ## before it with a non-empty "y" matrix  
        
        set_inv_mat <- function(any_mat) inv_mat <<- any_mat
        
        ## the set_inv_mat(any_mat) function will take any matrix as its argument and set the inverse of the matrix "inv_mat" to any_mat
        ## if any_mat is not a valid inverse of the matrix ( which is returned by solve(matrix) function) but some other arbitrary matrix   
        ## then the inv_mat is set to any_mat and any call to cacheSolve(for the given closure) will return that arbitrary matrix only
        
        get_inv_mat <-  function() inv_mat
        
        list(set_mat = set_mat, get_mat = get_mat,
             set_inv_mat = set_inv_mat,
             get_inv_mat = get_inv_mat)
        
}

## The cacheSolve (x,...) function returns the inverse of the matrix set in the closure
## First it checks for the existing value of get_inv_mat for the closure and if it exists it return
## the inverse inv_mat printing the message "getting cached value of inverse matrix"   
## if the inverse inv_mat does not exist, it computes the inverse using the solve(matrix) function
## and assigns it to  set_inv_mat(y) function of the makeCacheMatrix function and returns the inverse inv_mat    

cacheSolve <- function(x, ...) {
        
        inv_mat <- x$get_inv_mat()
        if(!all(is.na(inv_mat))) {
                message("getting cached value of inverse matrix")
                return(inv_mat)
        }
        new_mat <- x$get_mat()
        inv_mat <- solve(new_mat, ...)
        x$set_inv_mat(inv_mat)
        inv_mat
}

v1 <- makeCacheMatrix()
y <- matrix(c(4,2,7,6),2,2)
v1$set_mat(y)
v1$get_mat()
v1$set_inv_mat(solve(y))
v1$get_inv_mat()
cacheSolve(v1)


v2 <- makeCacheMatrix()
v2$get_mat()
y <- matrix(c(3,4,1,2),2,2)
v2$set_mat(y)
v2$get_mat()
v2$get_inv_mat()
cacheSolve(v2)

## Smaple Runs        

##      > v1 <- makeCacheMatrix()
##      > y <- matrix(c(4,2,7,6),2,2)
##      > v1$set_mat(y)
##      > v1$get_mat()
##      [,1] [,2]
##      [1,]    4    7
##      [2,]    2    6
##      > v1$set_inv_mat(solve(y))
##      > v1$get_inv_mat()
##      [,1] [,2]
##      [1,]  0.6 -0.7
##      [2,] -0.2  0.4
##      > cacheSolve(v1)
##      getting cached value of inverse matrix
##      [,1] [,2]
##      [1,]  0.6 -0.7
##      [2,] -0.2  0.4
##      > v2 <- makeCacheMatrix()
##      > v2$get_mat()
##      [,1]
##      [1,]   NA
##      > y <- matrix(c(3,4,1,2),2,2)
##      > v2$set_mat(y)
##      > v2$get_mat()
##      [,1] [,2]
##      [1,]    3    1
##      [2,]    4    2
##      > v2$get_inv_mat()
##      [,1]
##      [1,]   NA
##      > cacheSolve(v2)
##      [,1] [,2]
##      [1,]    1 -0.5
##      [2,]   -2  1.5 
