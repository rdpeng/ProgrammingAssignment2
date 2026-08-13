## Esta función crea un objeto especial tipo "matriz" que puede almacenar en caché su inversa.
## Devuelve una lista de funciones para interactuar con la matriz y su caché.
makeCacheMatrix <- function(x = matrix()) {
  # 'inv' almacenará la inversa de la matriz. Se inicializa como NULL.
  inv <- NULL
  
  # Función para cambiar el valor de la matriz
  set <- function(y) {
    x <<- y        # Asigna el nuevo valor 'y' a 'x' en el entorno superior (parent environment)
    inv <<- NULL   # Si la matriz cambia, limpia el caché de la inversa
  }
  
  # Función para obtener la matriz original
  get <- function() x
  
  # Función para establecer (guardar) la inversa en el caché
  setInverse <- function(inverse) inv <<- inverse
  
  # Función para obtener la inversa desde el caché
  getInverse <- function() inv
  
  # Devuelve la lista con las cuatro funciones creadas
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## Esta función calcula la inversa de la "matriz" especial creada con 'makeCacheMatrix'.
## Si la inversa ya ha sido calculada, la recupera directamente del caché.
cacheSolve <- function(x, ...) {
  # Intenta obtener la inversa almacenada en el objeto especial 'x'
  inv <- x$getInverse()
  
  # Si la inversa ya está en caché (no es NULL), la devuelve inmediatamente
  if(!is.is.null(inv)) {
    message("Obteniendo datos en caché (getting cached data)...")
    return(inv)
  }
  
  # Si no está en caché, obtiene la matriz original
  data <- x$get()
  
  # Calcula la inversa utilizando la función 'solve' de R
  inv <- solve(data, ...)
  
  # Almacena el resultado calculado en el caché del objeto 'x'
  x$setInverse(inv)
  
  # Devuelve la matriz inversa
  inv
}