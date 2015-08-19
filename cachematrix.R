     ## makeCacheMatrix specifies a matrix which will be stored at parent level.
     ## It contains a solve() function to find the inverse of the matrix passed to it.
     ## cacheSolve checks for the presence of the inverse in parent cache.  If the
     ## inverse was not cached, it uses the solve method in makeCacheMatrix to find it.
     
     ## Creates a special "matrix" object that can cache its inverse
     
     makeCacheMatrix <- function(x = matrix()) {
       mInv<-matrix() ##initialize a local, empty inverse
       mOrig <- x
       
       setInv<- function(x2) { # calculate the inverse of the original matrix
         mInv<<- solve(x2) # cache the inverse
       }
       getInv<-function() mInv   # return the inverse
     
         setmOrig <- function(x3)  { # cache the original matrix
         mOrig <<- x3
       }
     	
       getmOrig <- function() mOrig  # return the original matrix
       
       # list functions to be constructed
       list(getInv = getInv, setInv = setInv,
            getmOrig = getmOrig, setmOrig = setmOrig)
       
     }
     
     ##*************** END OF FUNCTION
     
     
     cacheSolve <- function(MCM, ...) {  # makeCachematrix will be passed to this function
       ## Return a matrix that is the inverse of the original matrix passed to it
     
       orig <- MCM$getmOrig  # store local copy of original matrix
       
       # check for original matrix change
       orig <- MCM$getmOrig()  # store local copy of original matrix
       
       if(orig == MCM$mOrig) { # if a match is found
         # check for cached inverse
         inv <- MCM$getmInv()
         if(!is.null(inv)) { # if a value is stored
           message("Returning cached inverse...")
           inv   # print the inverse
         }
       } else  { # calculate inverse
         #first, cache original matrix
         MCM$setmOrig(orig)
         message("Calculating inverse...")
         MCM$setmInv(orig)   # pass local copy of original matrix for calculation
         MCM$getmInv()   # print inverse
       }
     
     }
     
