makeCacheMatrix <- function(x = matrix()) {
  +     inv <- NULL
  +     set <- function(y) {
    +         x <<- y
    +         inv <<- NULL
    +     }
  +     get <- function() x
  +     setInverse <- function() inv <<- solve(x) #calculate the inverse
  +     getInverse <- function() inv
  +     list(set = set,
             +          get = get,
             +          setInverse = setInverse,
             +          getInverse = getInverse)
  + }

