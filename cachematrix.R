## Coursera R Language course: Programming Assignment 2
## functions to compute, cache, fetch the inverse of a matrix

## makeCacheMatrix does the checking if a value already exists. If so, returns the answer. If not, computes it

makeCacheMatrix <- function(x = matrix()) {
	   if(!squarematrix(x)){
	   	  message("argument not square numeric matrix")
	   	  return(NULL)
	   }
		m<-NULL
		set <-function(y){
			x<<-y
			m<<-NULL
		}
		get <-function()x
		setinverse<-function(inv) m<<-inv
		getinverse<-function() m
		list(set = set, get=get, 
			 setinverse=setinverse,
			 getinverse=getinverse)
}


## Computes the inverse of a matrix x. Used by makeCacheMatrix

cacheSolve <- function(x) {
        ## Return a matrix that is the inverse of 'x'
        m<- x$getinverse()
        if (!is.null(m)) {
        	message("getting cached data")
        	return(m)
        }
        data <-x$get()
        m<-solve(data)
        x$setinverse(m)
        m
}

##checks if argument is a square numeric matrix

squarematrix <- function(x) { 
	    if (!is.numeric(x)) return(FALSE)
	    
	    if (!is.matrix(x))   return(FALSE)
	    
	    d<-dim(x)
	    if (length(d)!=2) return(FALSE)
	    	
	    if(d[1]!=d[2])  return(FALSE)
	    
	    return(TRUE)
	    		
}

## models for doing the mean

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

cachemean <- function(x, ...) {
        m <- x$getmean()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- mean(data, ...)
        x$setmean(m)
        m
}
