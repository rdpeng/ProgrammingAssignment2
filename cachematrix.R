## Put comments here that give an overall description of what your
## functions do

## The following function creates a special "matrix" that can cahce its inverse. 
## The function consists of four other functions. These are:
## 1. "set" = set the value of the special matrix object;
## 2. "get" = get the value of this matrix;
## 3. "setinverse" = set the value of the inverse matrix;
## 4. "getinverse" = get the value of the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
        inv<-NULL                    #set the value of inv to NULL
        set<-function(y){            #set the value of the (squared) matrix  
                x<<-y                #the matrix is chached <- would be used by the function cacheSolve
                inv<<-NULL
        }
        get <-function() x                   #create three other functions (get, setinverse, getinverse)
        setinverse <- function(inverse) inv<<-inverse
        getinverse <- function() inv
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)     #create a list of four functions
}


## The function cacheSolve calculates the inverse matrix of the "special matrix"
## created by the previos function. 
## In the case if the inverse has already been calculated (other things equal), the function 
## would restore the inverse from the cache and, therefore, would reduce time for computation.

cacheSolve <- function(x, ...) { 
        ## Return a matrix that is the inverse of 'x'
        inv<- x$getinverse()      #get the data if the inverse matrix has already been genereted
        if (!is.null(inv)) {      #check if the function has already been used 
                message ("getting cached data")  #show the message if the function has been run before
                return(inv)
        }         
        data<-x$get()            #if the function hasn't been run before; run the function get in order 
        #to get the value of the initial inputted matrix
        inv<-solve(data, ...)    #the value of the inverse is computed  
        x$setinverse(inv)        #set the value of the inverse matrix and cache the value 
        inv
}

#Test of the functions
sq.matrix <- matrix(data=c(5,3,7,9,1,2,3,1,3), nrow=3, ncol=3)
solve(sq.matrix)  #the inverse of the inputted matrix to compare with the functions' results
sq.matrix2<- makeCacheMatrix(sq.matrix)
cacheSolve(sq.matrix2)

#run one more time to check whether the cache function works or not
cacheSolve(sq.matrix2)
