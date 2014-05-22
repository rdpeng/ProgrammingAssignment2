## Put comments here that give an overall description of what your
## functions do
## 
##	Sample use of 'makeCacheMatrix' and 'cacheSolve':
##         > data_mtx <- matrix(c(10, 20, 30, 40), nrow=2, ncol=2)      # sample input numeric matrix
##         > make_this_mtx <- makeCacheMatrix        # instantiate instance of special "matrix" object
##         > mtx01 <- make_this_mtx(data_mtx)        # create special "matrix" 'mtx01' with 'data_mtx' as input  
##         > inverse_of_mtx01 <- cacheSolve(mtx01)   # calc inverse of VALUE (i.e., 'data_mtx') in object 'mtx01', 
##                                                   #   and get from cache if already there
## 
##	The special "matrix" object built/instantiated by function 'makeCacheMatrix' is tailored to hold the  
##		input matrix, the inverse, and the 4 functions.  This object is designed to be the input for   
##  	function 'cacheSolve', which can access any of the 4 functions by indexing into the instantiated object:  
##
##		makeCacheMatrix creates a special "matrix" object (a list):
##           input: VALUE     # numeric square, invertible data matrix
##           list: (inv,                 # cached inverse of VALUE
##                  list( set,           # the 4 functions
##                        get,           #
##                        set_inverse,   #
##                        get_inverse )  #
##                 )                     #
## 
## 
##                    Write a short comment describing this function
##
##	makeCacheMatrix is a constructor function that builds a special "matrix" object based on a supplied 
##		argument:  a square, invertible matrix.  The special "matrix" object contains the input matrix,    
##		a cache variable for an inverse ('inv'), plus 4 functions.  See diagram above.
##		Functions 'set' and 'set_inverse' access variables in the their containing environment 
##		by using the '<<-' assignment operator (lexical scoping).
##
##
##                # constructor function that builds/instantiates the special "matrix" object
makeCacheMatrix <- function(x = matrix()) {    # 'x' is data input numeric matrix VALUE
        inv <- NULL             # inv is cache
		# define 4 functions:
        # set makes the data input matrix the VALUE of the special "matrix"; puts it into 'x'
		set <- function(y) {  
		        x   <<- y       # the VALUE is assigned to 'x' in the enclosing environment using '<<-'
				inv <<- NULL    # 'inv' is cache - is NULL because the VALUE is new (inverse not yet calc'd)
		}                       #                       'inv' is in the enclosing environment: use '<<-'
		
		# get returns the data input matrix VALUE (stored in 'x') of the special "matrix"
		get <- function() x   
		
		# set_inverse assigns freshly calc'd matrix inverse from caller to 'inv' (cache)
		set_inverse <- function(inverse) inv <<- inverse  # 'inv' is in the enclosing environment: use '<<-'
		
		# get_inverse returns inverse (of VALUE matrix) cached in 'inv'
		get_inverse <- function() inv 
		
		# keep 4 functions in this inner list
        list(set = set, get = get, 
             set_inverse = set_inverse,
             get_inverse = get_inverse)
}

##
##                    Write a short comment describing this function
##
##  cacheSolve returns the inverse of the input invertible matrix found in the input 
##		special "matrix" object (here named x) instantiated by function makeCacheMatrix.  cacheSolve  
##		either returns the cached inverse, or returns a freshly calculated inverse, which gets cached.
##		This function accesses the functions in the input object by indexing them by name 
##		using the '$' oprerator.  
##
##            # caller function that uses the functions in 'x' to manage inverse caching and inverse calc'ing
cacheSolve <- function(x, ...) {  # 'x' is special "matrix" object with VALUE, 'inv' and 4 functions
                                  # 'x' was built by the function makeCacheMatrix
        ## Return a matrix that is the inverse of 'x'
		# get_inverse returns cached inverse or NULL using special, inner function get_inverse in 'x'
		inv <- x$get_inverse()
		# test on value of 'inv': an inverse or NULL
        if(!is.null(inv)) {    
				# 'inv' is not NULL, so must be the cached inverse
                message("getting cached data")
				# return cached inverse
                return(inv)
        }  # else 'inv' is NULL, so calc a fresh inverse and cache it
		# get returns the VALUE in the special "matrix" x and assigns it to 'data' for inverse calculation
		data <- x$get()
		# calculate a fresh inverse using 'data' (from the VALUE in the special "matrix" 'x')
		inv <- solve(data) 
		# set_inverse caches fresh inverse into 'inv' of the special "matrix" 'x'
		x$set_inverse(inv)
		# return cached inverse 'inv' as value of function 'cacheSolve'
		inv
}

##	the end  ##
