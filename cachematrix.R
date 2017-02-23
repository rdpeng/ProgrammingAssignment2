## Put comments here that give an overall description of what your
## functions do

##makeCacheMatrix constructs the matrix object and has the handler
##functions necessary to manipulate its state.

makeCacheMatrix <- function(x = matrix()) {
    #establish a variable for assessing/returning the matrix inverse
    m<-NULL
    #Set function which sets the value of the matrix and resets the matrix inverse
    set<- function(y){
        x<<-y
        m<<-NULL
    }
    #get function which returns the matrix
    get<-function() x
     #Set inverse variable value
    setInverse<-function(xInverse){
        m<-xInverse
    }
    #gets matrix inverse and returns it
    getInverse<-function() m
    
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    #Test to see if the matrix has a cached inverse
    #extract inverse and set to testable variable
    m<-x$getInverse()
    #test if variable is empty or holds the matrix inverse
    if(is.null(m)){
        m<<-solve(x)
        return(m)
    }
    else{
        print("Getting cached matrix inverse...")
        return(m)
    }
}
