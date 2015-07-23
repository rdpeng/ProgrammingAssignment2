# pa2.R - Programming Assignment 2
# Caching the inverse of matrix
# makeCacheMatrix(): criates cached matrix (cria  matriz em cache)
# cacheSolve() :   obtains (obtem a matriz inversa do cache ou inverte-a)
###     EXEMPLO DE EXECUÇÂO:
### 1)  >a <- makeCacheMatrix()  =>carrega as funções em a
### 2)  >mat <- matrix(1:4,2,2)
### 3)  >a$setm(mat) =>carrega a matriz no cache, testa se é quadrada
### 4)  >a$seti() => calcula a inversa se não tiver sido calculada
### 5)  >a$geti() => obtém a inversa do cache (já está calculada)
### 6)  >cacheSolve(a)
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
                getm <- function() {xm}                           # exibe a matriz em cache
                seti <- function(x=matrix()) {                    # calcula a inversa da matriz no arg x  
                        if(nrow(x)==ncol(x) & !is.null(x)){       # testa se quadrada e não nula
                                if(is.null(m)) {                  # se a inversa não estiver na cache
                                    message("inverse calc!")
                                    ### m  <<- NULL
                                    xm <<- x                      # guarda a matriz no cache
                                    m  <<- solve(x)               # e calcula a inversa
                                    return(m)
                                } else {                          # a inversa já foi calculada
                                    message("in cache!")
                                    return(m)                     # exibe a inversa em cache
                                }
                        } else {
                            message("Is not square matrix or null!")
                        }
                }
                geti <- function() {m}
                list(setm=setm, getm=getm,
                     seti=seti, geti=geti)
}
cacheSolve <- function(y) {
                m <- y$geti()
                if(!is.null(m)) {
                    message("inversa read in cache!")
                    return(m)                                     # exibe a inversa em cache
                } else {
                    message("chamada para calcular inversa!")
                    y$seti(xm)                                    # calcular a inversa
                    return(m)
                    
                }
}