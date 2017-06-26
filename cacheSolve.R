cacheSolve<- function(x){
        m<- x$getSolve()
        if(!is.null(m)){
                message("printing cache value")
                m
        }
        data<- x$get()
        m<- solve(data)
        x$setSolve(m)
        m
}