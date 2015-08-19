## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

    invertedMatrix <- NULL
    set <- function(y = matrix()){    #set matrix
        x <<- y
        invertedMatrix <<- NULL
    }
    get <- function() x               #return matrix
    
    setInverted <- function(invertedValue) invertedMatrix <<- invertedValue
    getInverted <- function() invertedMatrix
    list(set = set,get = get, 
         setInverted = setInverted,
         getInverted= getInverted)
    
}


## Write a short comment describing this function


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    #x<-as.data.frame(t(x))
    
    invertedMatrix <- x$getInverted()   #get Cashed inverted matrix
    if(!is.null(invertedMatrix)){       #check if found
        message("getting cashed inverted matrix")
        return(invertedMatrix)
    }
    data<- x$get()                      #get matrix
    invertedMatrix <- solve(data)       #invert matrix
    x$setInverted(invertedMatrix)       #set inverted matrix
    invertedMatrix
    }
