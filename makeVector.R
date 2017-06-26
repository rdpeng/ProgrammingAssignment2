makeVector <- function(x = numeric()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmean <- function(mean) m <<- mean
        getmean <- function() m
        list(set = set, get = get,
             setmean = setmean,
             getmean = getmean)
}

#In this example we introduce the `<<-` operator 
#which can be used to assign a value to an object 
#in an environment that is different from the
#current environment. Below are two functions that
#are used to create a special object that stores a 
#numeric vector and caches its mean.

#The first function, `makeVector` creates a special 
#"vector", which is really a list containing a 
#function to

#1.  set the value of the vector
#2.  get the value of the vector
#3.  set the value of the mean
#4.  get the value of the mean
