#Feito a partir dos dados de makeVector e cachemean

makeCacheMatrix <- function(x = matrix()){
    #Cria uma matriz, que deve ser inserida como argumento da funcao
    inv <- NULL
    set <- function(novaMatriz){
        #Permite realimentar com nova matriz, zerando a cache e redefinindo a matriz
        x <<- novaMatriz
        inv <<- NULL
    }
    get <- function(){
        #Busca a matriz armazenada
        x
    }
    setInverse <- function(matrizInversa){
        #Guarda a matriz inversa calculada
        inv <<- matrizInversa
    }
    getInverse <- function(){
        #Busca a matriz inversa armazenada
        inv
    }
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

cacheSolve <- function(x, ...){
    #A funcao checa se a matriz inversa ja foi calculada ou armazenada.
    #Se sim, busca o valor desta e retorna.
    #Se nao, calcula (funcao solve) e armazena empregando o metodo da funcao makeCacheMatrix.
    inv <- x$getInverse()
    if(!is.null(inv)){
        message("Matriz ja criada - Buscando dados da memoria")
        return(inv)
    }
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setInverse(inv)
    inv
}
