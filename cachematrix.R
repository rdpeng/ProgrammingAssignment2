## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix2<-function(x = matrix()) {
   mrows <- nrow(in_m)
   mcols <- ncol(in_m)
   im_vec <- c(x)
   vlen <- length(im_vec)
#create inverse of input matrix
   inverse_v <- im_vec * t(im_vec)
   i_matrix <- matrix(data = inverse_v, nrow = mrows, ncol = mcols)
#cache input matrix
  o_matrix <<- i_matrix
  return(o_matrix)
}


## computes the inverse of the special "matrix" returned by makeCacheMatrix above
##  If the inverse has not already been calculated, caluate the inverse
## get the cached matrix
cacheSolve <- function(x, ...) {
  mrows <- nrow(x)
  mcols <- ncol(x)
  i_matrix<- matrix(data = NA, nrow = mrows, ncol = mcols)
  i_matrix <- NULL

  if (is.matrix(x) ) {
   cm <- makeCacheMatrix2(x)
   v_cm <- c(cm)
   v_x <- c(x)

   if (!isTRUE(all.equal(v_x,v_cm))) {    
     i_matrix <- makeCacheMatrix2(v_x)
   }
   else 
     i_matrix<- as.matrix(x)   
  }
## Return cached matrix
   return(o_matrix)
}
