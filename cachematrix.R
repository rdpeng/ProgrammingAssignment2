    ## Actividad corespondiente a la semana 3: Léxico
    ## El código representa una funcion que utiliza los objetos 'datos', 'nrow' y 'ncol'
    ## la función genera una matriz 'x' usando información de los objetos y genera
    ## 'inv' como el inverso de 'x'
    
    makeCacheMatrix <- function (x = matrix (), data, nrow, ncol) {
      x <- matriz (datos, nrow, ncol)
      inv <- NULL
      establecer <- función (y) {
        x << - y
        inv << - NULL
      }
      obtener <- función () x
      setmean <- function (solve) inv << - resolver
      getmean <- función () inv
      lista (set = set, get = get,
             setmean = setmean,
             getmean = getmean)
    }
  
  ## La funcion cacheSolve regresa la inversa si esta no falta, o 
  ## calcula y devuelve la inversa si es que esta falta, 
  ## cuyo insumo es la función anterior makeCacheMatrix
  
  cacheSolve <- función (x, ...) {
    inv <- x $ getmean ()
    if (! is.null (inv)) {
      mensaje ("obtener datos en caché")
      volver (inv)
    }
    datos <- x $ get ()
    inv <- resolver (datos)
    x $ setmean (inv)
    inv
  }
