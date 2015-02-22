##==============================================================================##
## his source allows you to cache the inverse matrix without extra calculations.##
##   First function helps you to create a specific matrix object with attribute ##
## inverse matrix.                                                              ##
##   Second function helps you to get the inverse matrix from created specific  ##
## matrix object.                                                               ##
##==============================================================================##



##==============================================================================##
## Function 1.                                                                  ##
##                                                                              ##
## Input: standart R-matrix m                                                    ##
## Output: specific matrix object that can cache its inverse                    ##
## - with methods: set, get, set.inv, get.inv.                                  ##
##==============================================================================##

makeCacheMatrix <- function(m = matrix()) {
# the inverse of matrix m
        m_inv <- NULL 

# set new matrix value
	  set <- function(x,...) {
                m <<- matrix(x,...)
                m_inv <<- NULL
        }

# get old matrix value	
        get <- function() m

# set new inverse matrix value
        set.inv <- function(solve) m_inv <<- solve

# get olf inverse matrix value
        get.inv <- function() m_inv

# create a specific matrix object
        list(set = set, get = get,
             set.inv = set.inv,
             get.inv = get.inv)
}



##==============================================================================##
## Function 2.                                                                  ##
##                                                                              ##
## Input: specific matrix object                                                ##
## Output: inverse of this matrix                                               ##
##==============================================================================##

cacheSolve <- function(m, ...) {
# get known inverse matrix value
	  m_inv <- m$get.inv() 

# try to return known inverse matrix value (if it's known)
        if(!is.null(m_inv)) {
                message("Yippee! Getting cached data!")
                return(m_inv)
        }

# calculate inverse matrix value (if it's unknown)
        data <- m$get()
        m_inv <- solve(data, ...)
        m$set.inv(m_inv)

# return calculated inverse matrix value
        m_inv 
}
