## Put comments here that give an overall description of what your
## functions do
##this function makes a special matrix and stores the 
##and cach the solution to prevent redoing the calculation

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        inv<-NULL
        set<-function(y){
        x<<-y
        inv<<-NULL
        }
        get<-function()x
        setinverse<-function(inverse) inv<<-inverse
        getinverse<-function() inv
        list(set=set,
             get=get, 
             setinverse=setinvers,
             getinverse=getinvers)
}


## Write a short comment describing this function
##this function returns the value of cahc if the inverse of matrix is already calculated
## and stored in the cach, and if not it solves the inverse of the matrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv<-x$getinverse
        if (!is.null(inv)){
                messge("getting chached data")
                return(inv)
        }
        matrix<-x$getinverse()
        inv<-solve(matrix,...)
        x$setinverse(inv)
        inv
}	
