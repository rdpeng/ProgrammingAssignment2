## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#makeChacheMatrix function creates a  special "matrix" object, which is a list containing a function to
#set the value of the matrix,get the value of the matrix, set the value of the inverse ,
#get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
        i<- NULL
        set <- function(y){
                x<<-y
                i<<-NULL
        }
        get <- function() x
        setinverse <- function(solve) i<<- solve
        getinverse <- function() i
        
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## Write a short comment describing this function
#return a matrix inverse of x, by computing if not present earlier and storing it
# in the object list after computing
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        i <-x$getinverse()
        if(!is.null(i)){
                return(i)
        }
        matx <- x$get()
        i<-solve(matx)
        x$setinverse(i)
        i
        
}
