## Put comments here that give an overall description of what your
## functions do

## This function creates a special "special matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    m<-NULL
    set<-function(y){
        x<<-y
        m<<-NULL
    }
    get<-function() x
    setInverse<-function(inverse) m<<-inverse
    getInverse<-function() m
    list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
}

## This unction computes the inverse of the special "matrix returned by makeCacheMean function.

cacheSolve <- function(x, ...) {
    m<-x$getInverse()
    if(!is.null(m)){
        message("getting cachet data")
        return(m)
    }
    data<-x$get()
    m<-solve(data,...)
    x$setInverse(m)
    m
}