## Put comments here that give an overall description of what your
## functions do


## Make Matrix
z <- matrix(rnorm(16),nrow = 4, ncol = 4)

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
              i <- NULL            #Reset value to null
              set <- function(y) {
              x <<- y              #Fills x with y and renulls i
              i <<- NULL
              }
              get <- function() {x}
              setInv <- function(inv) {i <<- inv}    #Fills i with new values
                getInv <- function() i
                list(set = set, get = get,
              setInv = setInv,
              getInv = getInv)                 #Names function to be called later
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
                i <- x$getInv()
                if(!is.null(i)) {
                message("getting cached data")
                return(i)      #Makes sure inverse has not yet been calculated
                }
                data <- x$get()
                i <- solve(data)  #Calculates Inverse of Matrix
                x$setInv(i)       #Puts Inverse into setInv variable to be printed after function runs
                i
}

#Run using test Matrix
cacheSolve(makeCacheMatrix(z))



#Test using inv function

