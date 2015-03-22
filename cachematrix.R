## Matrix inversion

## Cache inverse of a matrix, return list of functions

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y){
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(x) m <<- solve(x)
    getinverse <- function() m
    list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## Returns cached inverse or computes inverse then returns

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    x$setinverse(data)
    x$getinverse()
}
