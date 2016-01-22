## makeCacheMatrix creates a special object to store matrix and more importantly stores 4 functions within
##including "set", "get", "setInverse","getInverse"  


makeCacheMatrix <- function(x = matrix()) {

        m<-NULL
        set<-function(y) {
                x<<-y
                m<<-NULL
        }
        get<-function()x
        setInverse<-function(inverse) m<<-inverse
        getInverse<-function() m
        list(set=set,get=get,
             setInverse=setInverse,
             getInverse=getInverse)
}


## The function below takes the inverse of the special object created previously
## Only creates an inverse if the variable housing the matrix "m" is NULL. If already created
## it will retrieve from cache.

cacheSolve <- function(x, ...) {
        m<-x$getInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        dat<-x$get()
        m<-solve(dat, ...)
        x$setInverse(m)
        m
}
