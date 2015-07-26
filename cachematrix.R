cachematrix.R - Programming Assignment 2
## Caching the inverse of matrix
## WARNING: My English is weak... My language native is portuguese.
#########################################################################################
###     SAMPLE RUN:                                                                     #
### 1)  >source("cachematrix.R")   ## load the program R                                #                                                    #
### 2)  >m1 <- matrix(1:4,2,2)     ## put matrix (1:4,2,2) in variable 'm1'             #
###  follow 3.1 or 3.2                                                                  #
### 3.1)>a  <- makeCacheMatrix()   ## load only functions in variable 'a'               #
### 3.2)>a$setm(m1)                ## load matrix "m1" in cache                         #
### 3.2)>a  <- makeCacheMatrix(m1) ## load functions and matrix 'm1'                    #
### 4)  >a$seti()          ## compute inverse, if it has not been calculated            #
### 5)  >a$geti()          ## get inverse cache (if already has calculated)             #
### 6)  >cacheSolve(m1,a)  ## return inverse matrix 'm1', cache in variable 'a'         #
#########################################################################################

####################################################################
# makeCacheMatrix(): criate cached matrix (cria  matriz em cache)  #
####################################################################
makeCacheMatrix <- function(x=matrix()) {
                m <<- NULL
                setm <- function(x=matrix()) {
                    if((nrow(x)==ncol(x)) & (!is.null(x)) ){ # test is matrix is square and not null
                            m  <<- NULL                      # m  = inverse matrix cache
                            xm <<- x                         # xm = matrix cache
                    } else {
                            message("Is not square matrix or null!")
                    }
                }
                getm <- function() {xm}                           # returns matrix cache
                seti <- function(x=matrix()) {                    # computes the inverse of matrix 'x'  
                        if(nrow(x)==ncol(x) & !is.null(x)){       # test if matrix is square and not null
                                if(is.null(m)) {                  # if the inverse is not cached
                                    message("inverse calculate!")
                                    xm <<- x                      # stores the matrix in cache 'xm'
                                    m  <<- solve(x)               # and calculates the inverse
                                    return(m)
                                } else {                          # inverse has already been calculated
                                    message("in cache!")
                                    return(m)                     # returns the inverse cached
                                }
                        } else {
                            message("Is not square matrix or null!")
                        }
                }
                geti <- function() {m}                            # returns the the inverse
                
				list(setm=setm, getm=getm,                        # loads the list of functions
                     seti=seti, geti=geti)
}

############################################################################################################
# cacheSolve(): return inverse matrix in cache or calculate inverse (retorna a inversa da matriz em cache) #
############################################################################################################
cacheSolve <- function(y = matrix(),z) { # return matrix that is inverse of 'y' with cache in 'z'
                    m  <- z$geti()
                    xm <- z$getm()
                    if(identical(xm,y)) { 
                       if(!is.null(m)) {
                            message("inverse read in cache!")
                            return(m)                                     # displays the inverse cached
                        } else {
                            message("call for calculate inverse!")
                            z$seti(xm)                                    # to calculate the inverse
                            return(m)
                        }
                    } else { 
                        message("matrix is not the same the cache")
                    }
}
