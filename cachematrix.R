## Create 2 functions that cache the inverse of a matrix


## 1stfunction: makeVector creates a special "vector"
makeVector <- function( m = numeric() ) {

	   m <- NULL

    ## Method to set the value of the vector
    set <- function(y) {
            x <<- y
            m <<- NULL
    }

    ## Method the get the value of the vector
    get <- function() {
    	    	m
    }

    ## Method to set the value of the mean
    setmean <- function(mean) m <<-mean

    ## Method to get the value of the mean
    getmean <- function() m

    ## Return a list of the methods
    list(set = set, get = get,
         setmean = setmean,
         getmean = getmean)
}

## 2nd function: calculates the mean of the special "vector"

cacheSolve <- function(x, ...) {

    ## Return a matrix that is the mean of 'x'
    m <- x$getmean()

        if( !is.null(m) ) {
            message("getting cached data")
            return(m)
    }

    ## Get the matrix 
    data <- x$get()

    ## Calculate the inverse 
    m <- solve(data) %*% data

    ## Set the inverse 
    x$setmean(m)

     m
}
