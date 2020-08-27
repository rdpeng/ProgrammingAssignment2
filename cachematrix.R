Here get and set the values of the matrix which are containing in a list
makeCacheMatrix <- function(x = matrix()) {
    i<-NULL
    s<-function(y){
        i<<-NULL
        x<<-y}
    g<-function(){x}
    sinverse<-function(inverse){i<<-inverse}
    ginverse<-function(){i}
    list(s=s,g=g,sinverse=sinverse,ginverse=ginverse)}


If the matrix isn´t null the function calculate the inverse

cacheSolve <- function(x, ...) {
    i<-x$ginverse()
    if(!is.null(i)){
        message("Obteniendo datos")
        return(i)}
    resolve<-x$g()
    i<-solve(resolve, ...)
    x$sinverse(i)
    i
}
