MatrixCache <- function(x = matrix()) {
  inverse <- NULL
  setop <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  getop <- function() x
  
  
  setInverse <- function() inverse <<- solve(x) #calculate the inverse
  getInverse <- function() inverse
  list(setop = setop,
       getop = getop,
       setInverse = setInverse,
       getInverse = getInverse)
}



funs <- MatrixCache()#regular matrix
funs$setop(matrix(1:4, 2))
funs$getop()




funs$setInverse()
funs$getInverse()#getting output







