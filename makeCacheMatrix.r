makeCacheMatrix<- function(x= matrix()){
        m<- NULL
        set<- function(y){
                x<<- y
                m<<- NULL
        }
        get<- function() x
        setSolve<- function(solve) m<<- solve
        getSolve<- function() m
        list(get= get, set= set, getSolve= getSolve, setSolve= setSolve)
} 