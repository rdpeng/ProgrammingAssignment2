#creating function to create a special 'matrix' object that can caches its inverse
# Note: <<- used below to assign value to object in an environment different from current

#Make Cache Matrix: a list containing the below function (# in code show where
#                    and what is taking place)
#Notes for below Code:    
#0. Creates makeCacheMatrix function where all the below occurs
#1. sets the value of the vector
#2. gets/calls the value of the vector
#3. sets the value of the inverse 
#4. gets/calls the value of the inverse 
makeCacheMatrix <- function(x = matrix()) { #0
     a <- NULL
     set <- function(y){
          x <<- y 
          a <<- NULL 
     } # 1
     get <- function() x  #2
     setInv <- function(inv) a <<- inv #3 
     getInv <- function() a 
     list(set = set, get = get, 
          setInv = setInv, 
          getInv = getInv) #4
} 

# Cache Solve, calculates the inverse from the vector created above. Before running
#    this function checks to see if the inverse has already been calculated. If so, 
#    it will call the inverse from the cache instead of running through 
#    the rest of its function. If not it doesn't see/can't find it, will calculate
#    the inverse of the data and sets the value in the cache via 'setinv'
#    (set inverse) function.
#Notes for below Code:    
#0. Creates cacheSolve function where all the below occurs
#1.Checks to see if inverse has already been calculated 
#2a. If not NULL, returns messaged "getting cached data!"
#2b. If NULL, proceeds to calculate the inverse 
#3. Calculates the inverse of the data
#4. sets the value of the inverse in the cache with the 'setinv' (set inverse) 
#    function.
cacheSolve <- function(x, ...) { #0
     inv <- x$getInv() #1
     if(!is.null(inv)) {
          message("getting cached data!")
          return(inv)
     } #2a
     data <- x$get() #2b
     inv <- solve(data) #3
     x$setInv(inv) #4
     inv
}

#Testing 
test <- makeCacheMatrix(matrix(1:4, 2, 2))
test$get() #see the matrix
test$getInv() #look for inverse, yields null result 
cacheSolve(test) #runs second function to look for or calculate inverse! 

