## The porpose of this code is obtain the inverse of a square matrix x. 
#First, we put a matrix in cache with the function "makeCacheMatix".
# Second, we run the function "cacheSolve". "CacheSolve" return de inverse of x.
# To do that, "CacheSolve" first check if the inverse of x was already been calculated.
#If the answer is "yes", the function return the inverse matrix stored in the cache.
#If the answer is "no", the function calculated the inverse of A.

# This function put the matrix x in cache and make a "list" of function to set Matrix, get the Matrix, set the inverse of the Matrix and get the inverse of the Matrix.
makeCacheMatrix <- function(x = matrix()) {
    x_inv<-NULL
    setMatrix<-function(mdata){       ## Set the matrix X (put a data into the matrix x)
        x<<-mdata
        x_inv<<-NULL
    }
    
    getMatrix<-function(){x}   ##call the original matrix
    
    setInverse<- function(aux){     ## Set the inverse of X (put aux into de x_inv)
    x_inv <<-aux
    } 
    
    getInverse<-function() x_inv ##call de inverse matrix 
return(list(getMatrix=getMatrix,getInverse=getInverse,setMatrix=setMatrix,setInverse=setInverse))
} 


## This function return a matrix that is the inverse of 'x'. 
#If this inverse was already calculated, the function will return the cached data along with a message.

cacheSolve <- function(x, ...) {
    M_inv <- x$getInverse()
    if(!is.null(M_inv)) {   # check if the inverse of 'x' is in cache.
        message("getting cached data")
        return(M_inv)
    }
    data<-x$getMatrix()  #If the inverse of 'x' is not in cache, the inverse is calculated in this block.
    M_inv<-solve(as.matrix(data)) 
    x$setInverse(M_inv)
    return(x$getInverse())
}
