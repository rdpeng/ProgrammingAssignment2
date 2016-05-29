
# Hi!  Honestly, I really tried to write a program that
# would be fundamentally different from the posted example.
# I wanted to just have:
# makeCacheMatrix <- function(x) {return(c(x,NULL) )}
# cacheSolve <-  function(x) {if (!is.NULL) {x[[2]] <- solve(x)}
#                              return(x[[2]])}
# However, the second function isn't able to change the values
# of the objects made in the first, since its environment is
# different.  Thus, the CacheMatrix has to consist of functions
# (which don't change) in a single environment (which can change).
# And thus, I have resorted to being boring and largely copying
# the example.  Sorry, but it's the only way I can find that works!

# Our function takes in a matrix that might want its inverse cached
makeCacheMatrix <- function(mainmatrix = matrix()){

        # we start with the inverse undefined
        inverse <- NULL

        # setmatrix changes the given matrix, and resets the inverse
        setmatrix <- function(newmatrix){
                mainmatrix <<- newmatrix
                inverse <<- NULL
        }

        # getmatrix tells us the current matrix        
        getmatrix <- function() {return(mainmatrix)}

        # setinverse computes the inverse and stores it in the environment
        calcinverse <- function() {inverse <<- solve(mainmatrix)}

        # getinverse tells us the inverse (or says NULL if uncalculated)
        getinverse <- function() {return(inverse)}

        # the output of this function will be a list of functions
        # called a CacheMatrix that live and work in this environment 
        return(list(setmatrix = setmatrix, getmatrix = getmatrix, 
                    calcinverse = calcinverse, getinverse = getinverse))
}

# I love being able to re-use variable names, since everything is 
# different from environment to environment anyway!
# This function will output the inverse of the matrix in a CacheMatrix,
# but will not recalculate if this has already been done
cacheSolve <- function(x) {

        # get the value of the inverse from the CacheMatrix environoment:
        inverse <- x$getinverse()
        
        # if it is null, compute the inverse...
        if (is.null(inverse)) { x$calcinverse() }
               
        # in any event, return the inverse
        return(x$getinverse())
}

# And that's that!  Thanks for watching!
