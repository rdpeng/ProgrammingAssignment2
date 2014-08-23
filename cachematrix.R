
#############################################################################################
##
## R Programming July 2014 - Coursera/Johns Hopkins Univ.
## Assignment 2 - bobisthedataguy
##
## Set of functions for caching a matrix and it's inverse. Includes the following modules 
## specified in the assignment:  
## 
##		makeCacheMatrix: Sets up a series of functions matrix cache manipulation.
##		cacheSolve: Computes the inverse of the matrix that was saved to cache.
##
## The following additional modules are not part of the requirements but were created to 
## facilitate testing:
##
## 		nullSolve: nulls the cached inverse matrix
##		cacheMatrix: loads the non-inverted matrix to the cache location
##		nullMatrix: nulls the non-inverted matrix
##
#############################################################################################
## makeCacheMatrix
## usage: makeCacheMatrix()
## 
## This function intansiates the following functions:
##
##	set - saves a matrix to cache
##	get - retrieves a cached matrix
##	setInverse - saves the inverse of a matrix to cache
##	getInverse - retrieves a cached inverse matrix
##
#############################################################################################

makeCacheMatrix <- function(x = matrix()) {

	mtx <<- NULL									## NULL matrix cache
	mtxInverted <<- NULL							## NULL inverted cache

	set <- function(x) {							## load matrix to cache function
			mtx <<- x
			mtxInverted <<- NULL
			}

	get <- function() mtx							## get matrix from cache function

	setInverse <- function(inverse) mtxInverted <<- inverse	## load inverse matrix to cache function

	getInverse <- function() mtxInverted			## get inverse matrix from cache function
	
	mFuncs<<-list (set = set, get = get, 			## load the function into a vector
			setInverse = setInverse, 
			getInverse = getInverse)

}

#############################################################################################
## cacheSolve
## usage: cacheSolve()
## 
## This function calculates the inverse of a matrix passed to it and stores it into cache.
## If the inverse matrix cache is already populated it just returns what is in cache.
##
#############################################################################################

cacheSolve <- function() {					        ## Return a matrix that is the inverse of what matrix passed

	 	m <- mFuncs[['getInverse']]()				## get the inverse matrix from cache

      	if(!is.null(m)) {							## if data already in cached inverse matrix return it
                message("getting cached data")		## (inverse matrix is only null if the non-inverted matrix is loaded)
                return(m)							## (or if it hasn't been loaded at all)
        }

		data <- mFuncs[['get']]()					## Get matrix data previously cached

        if(is.null(data)) {							## if Matrix not cached then display error 
                message("ERROR: No cached matrix data found to invert. Use cacheMatrix function to load matrix to cache prior to using this function.")
				message("Returning NULL")
                return(data)
        }

        m <- solve(data)							## compute inverse of matrix

        mFuncs[['setInverse']](m)					## load the invese matrix into the inverse matrix cache

        m											## display the inverse matrix										

}

#############################################################################################
## nullSolve
## usage: nullSolve()
## 
## This function NULLs the cached inverse matrix and then displays what is in the cache.
## Not required per assignment requirement - created to facilitate testing
##
#############################################################################################

nullSolve <- function() {							## NULL the cached inverse matrix
	
	    mFuncs[['setInverse']](NULL)				## set cache inverse matrix to NULL
		m <- mFuncs[['getInverse']]()				## get the inverse matrix from cache

        m											## display what was returned from cache

}

#############################################################################################
## cacheMatrix
## usage: cacheMatrix(<matrix>)
## 
## This function caches the matrix passed to it.
## If something is passed to this function then it is loaded to the cache matrix.
## If nothing is passed to this function, it returns what is in the cache matix
## Not required per assignment requirement - created to facilitate testing
##
#############################################################################################

cacheMatrix <- function(z=NULL) {

	 	m <- mFuncs[['get']]()						## get the cache matrixb

		if(is.null(z)) {							## if noting passed 
			if(!is.null(m)) {						## if data already in matrix, return it
				message("getting cached data.")
				return(m)
        		}
			else {									## if NULL returned display a message
				message("Warning: Cache matrix is NULL.")
				return (m)
				}
			}

		mFuncs[['set']](z)							## load matrix to cache matrix 
		m <- mFuncs[['get']]()						## retrieve what was just cached

        m											## display what was returned from cache

}

#############################################################################################
## nullMatrix
## usage: nullMatrix()
## 
## This function NULLs the cached matrix.
## It then displays what is in the cached matrix to provide verification.
## Not required per assignment requirement - created to facilitate testing
##
#############################################################################################

nullMatrix <- function() {							## NULL the cached matrix

        mFuncs[['set']](NULL)						## set cache matrix to null

		m <- mFuncs[['get']]()						## retrieve what was just cached

        m											## display what was returned from cache
			
}

#############################################################################################
## END
#############################################################################################
