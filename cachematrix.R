# makeVector is a function that returns a list of functions
# Its puspose is to store a vector and a cached value of the mean of the 
# vector. Contains the following functions:
# * set      set the value of a vector
# * get      get the value of a vector
# * setmean   set the cached value (mean)
# * getmean     get the cached value (mean)
makeVector <- function(x = numeric()) {
        
        # holds the cached value or NULL if nothing is cached
        # initially nothing is cached so set it to NULL
        m <- NULL
        
        # store a vector
        set <- function(newValue) {
                x <<- newValue
                # since the vector is assigned a new value, flush the mean
                m <<- NULL
        }

        # returns the stored vector
        get <- function() {
                x
        }

        # m the given argument 
        setmean <- function(mean) {
                m <<- mean
        }

        # get the cached value
        getmean <- function() {
                m
        }
        
        # return a list. Each named element of the list is a function
        list(set = set, get = get, setmean = setmean, getmean = getmean)
}


# The following function calculates the mean of a "special" vector created with 
# makeVector
cachemean <- function(y, ...) {
        # get the cached value
        mean <- y$getmean()
        # if a cached value exists return it
        if(!is.null(mean)) {
                message("getting cached data")
                return(mean)
        }
        # otherwise get the vector, calculate the mean and store it in
        # the cache
        data <- y$get()
        mean <- mean(data)
        y$setmean(mean)
        
        # return the mean
        mean
}