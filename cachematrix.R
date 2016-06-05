## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##This function creates a "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
        set<-function(y){       ##Set the values of the matrix
              x<<-y
              m<<-NULL
        }
        get<-function()x        ## Get the values of the matrix
        setmatrix<-function(solve) m<<-solve  ##Set the values of the matrix inverse
        getmatrix<-function() m    ##Get the values of the matrix inverse
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)
}


## Write a short comment describing this function
##Computes the inverse of the matrix returned by the previous function. cacheSove retrives the inverse from the makeCacheMatrix if it has already been calculated

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
         m<-x$getmatrix()
         if(!is.null(m)){              ##checks if the inverse matrix has already been calculated 
                 message("getting cached data")
                 return(m)            ##If yes returns the value calculated in the previous function, skiping the computation.
         }
         matrix<-x$get()
         m<-solve(matrix, ...)  ##If no gets the data from the matrix and calculates its inverse
         x$setmatrix(m)
         m
}
