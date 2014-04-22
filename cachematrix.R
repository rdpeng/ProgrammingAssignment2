## This function creates a list of four functions - set, get, setinverse and getinverse.
##Input x must be a square, invertible matrix.
##To call and use this function, simply input the matrix and assign the function output to a variable,
##eg. a <- makeCacheMatrix(matrix(c(2,4,3,1), nrow=2, ncol=2)).
makeCacheMatrix <- function(x = matrix()) {
        ##Assigns NULL to inv variabble (in fact it simply creates an object);
        ##inv stores the inverted matrix.
        inv <- NULL
        
        ##The set() function changes the matrix x to a given y (globally)
        ##and sets globally inv to NULL (as the matrix changed, the inverse is unknown at first).
        ##Call by a$set(y),
        ##where a is the variable you've assigned makeCacheMatrix(x) to.
        set <- function(y) {
          x <<- y
          inv <<- NULL
        }
        
        ##The get() function returns the current value of matrix x.
        ##Call by a$get(y),
        ##where a is the variable you've assigned makeCacheMatrix(x) to.
        get <- function() x
        
        ##The setinverse() function sets the value of inv to inverse (given on function call).
        ##Call by a$setinverse(inverse),
        ##where a is the variable you've assigned makeCacheMatrix(x) to.
        setinverse <- function(inverse) inv <<- inverse
        
        ##The getinverse() function returns the current value of inv.
        ##Call by a$getinverse(),
        ##where a is the variable you've assigned makeCacheMatrix(x) to.
        getinverse <- function() inv
        
        ##It is the object function makeCacheMatrix(x) returns
        ##- a list of four functions.
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}
	
##This function returns the inverse of a matrix given on input in makeCacheMatrix(x).
##If a value is assigned to inverse, it returns it.
##Otherwise it calculates it.
##To call it, give the name of the list you've assigned makeCacheMatrix(x) output to,
##eg. cacheSolve(a)
cacheSolve <- function(a, ...) {
        ##Gets the inverse value from the object a.
        inverse <- a$getinverse()
        
        ##Checks the condition - if inv value is not NULL, it returns it
        ##with a message "getting cached data" and ends the function cacheSolve.
        if (!is.null(inverse)){ 
          message("getting cached data")
          return(inverse)
        }
        
        ##If the condition is not met,
        ##it assigns input of makeCacheMatrix(x) to a variable data, using the get() function.
        ##In other words - it assigns the matrix x to data.
        data <- a$get()
        
        ##Calculates the inverse of the matrix data, using built-in function solve().
        inverse <- solve(data, ...)
        
        ##Sets inv of object a (to which the output of makeCacheMatrix(x) was assigned) 
        ##to calculated inverted matrix.
        x$setinverse(inverse)
        
        ##Returns the inverted matrix.
        inverse
}