## this code demonstrates the concept of lexical scoping by making use
## of a set of functions that illustrate caching of a mean from a vector.
## The makeCacheMatrix creates an R object that stores a vector and its mean.

makeCacheMatrix <- function(x = matrix()) {               ## 'x' is the function argument.         
 inv <- NULL                          ## assign value NULL to 'inv' in parent environment.
  set <- function(y) {                                     ## defining the 'set' function.
   x <<- y                                   ## assign value from right side to left side.
    inv <<- NULL                      ## assign value NULL to 'inv' in parent environment.
}
    get <- function() x                                                 ## x is retrieved.
   setInverse <- function()                                          ## define setInverse.
  inv <- solve(x)                                                ## calculate the inverse.
 getInverse <- function() inv                                        ## define getInverse.
list(set = set,            
 get = get,
   setInverse = setInverse,
     getInverse = getInverse) ##list assigns as elements and returns to parent environment.
}
     dat <- makeCacheMatrix()                                                ## assign dat.
   dat$set(matrix(1:4, 2))                                             ## build the matrix.
 dat$get()                                                             ## print the matrix.
  
##        [,1] [,2]
##  [1,]    1    3
##  [2,]    2    4
  
## Return a matrix that is the inverse of 'x'  
  
dat$setInverse()                                                         ## prep the matrix.
dat$getInverse()                                         ## print the inverse of the matrix.
 
##         [,1] [,2]
## [1,]     -2  1.5
## [2,]      1 -0.5

ls(environment(dat$set))                            ## list the functions in the environment.
    
[1] "get"        "getInverse" "inv"        "set"        "setInverse" "x"
