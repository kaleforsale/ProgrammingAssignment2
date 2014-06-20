
## Create a CacheMatrix object in R

makeCacheMatrix <- function(a=matrix()) {  
    origMatrix <- NULL
    
    set <- function(x) {
        a <<- x
        origMatrix <<- NULL
    }
    
    get <- function() {
        a
    }
    
    setinverse <- function(b) {
        origMatrix <<- b  
    }
    
    getinverse <- function() {
        origMatrix
    }
    
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}



## pull inverse of matrix if stored; else, generate inverse matrix and store in cached object

cacheSolve<- function(x, ...) {  # create, ie 'set', the matrix and input as 'x'
    m <- x$getinverse()
    if (!is.null(m)) {
        message("returning the cached inverse")
        return(m)
    }
    
    data <- x$get()
    m <- solve(data)
    x$setinverse(m)
    m
    
}
