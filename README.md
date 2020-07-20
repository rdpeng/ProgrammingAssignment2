# ProgrammingAssignment2
Repository for Programming Assignment 2 for R Programming on Coursera
makeCacheMatrix = function(x = matrix()){  #Cria um obejto Matriz especial, 
                                           #que seu inverso pode ser armazenado em cache
  inv = NULL
  set = function(y){
    x <<- y
    inv <<- NULL
  }
    get = function() {x}
    setInverse = function(inverse){inv <<- inverse}
    getInverse = function() {inv}
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}
cachesolve = function(x, ...){      #Cálculo da matriz inversa especial
                                    #Retorna uma matriz que é o inverso de "x"
  inv = x$getInverse()
  if (!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  mat = x$get()
  inv = solve(mat, ...)
  x$setInverse(inv)
  inv
}
