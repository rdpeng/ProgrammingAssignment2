cachematrix.R - Programming Assignment 2
## Caching the inverse of matrix

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
                    if((nrow(x)==ncol(x)) & (!is.null(x)) ){ # testa se quadrada e não nula
                            m  <<- NULL                      # m  = cache da inversa
                            xm <<- x                         # xm = cache da matriz
                    } else {
                            message("Is not square matrix or null!")
                    }
                }
                getm <- function() {xm}                           # retorna a matriz em cache
                seti <- function(x=matrix()) {                    # calcula a inversa da matriz no arg x  
                        if(nrow(x)==ncol(x) & !is.null(x)){       # testa se quadrada e não nula
                                if(is.null(m)) {                  # se a inversa não estiver na cache
                                    message("inverse calculate!")
                                    xm <<- x                      # guarda a matriz no cache
                                    m  <<- solve(x)               # e calcula a inversa
                                    return(m)
                                } else {                          # a inversa já foi calculada
                                    message("in cache!")
                                    return(m)                     # retorna a inversa em cache
                                }
                        } else {
                            message("Is not square matrix or null!")
                        }
                }
                geti <- function() {m}                            # retorna a matriz inversa
                list(setm=setm, getm=getm,                        # carrega a lista de funções
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
                            return(m)                                     # exibe a inversa em cache
                        } else {
                            message("call for calculate inverse!")
                            z$seti(xm)                                    # calcular a inversa
                            return(m)
                        }
                    } else { 
                        message("matrix is not the same the cache")
                    }
}
