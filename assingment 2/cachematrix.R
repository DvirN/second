## these function take a matrix and return its inverse.
## however, if the inverse was already compued for that specific matrix, 
## the computation will not be executed again. the cache inverse will be returned



## Write a short comment describing this function
## this is a function which contatins a list of 4 functions, to deal with getting and returning 
## a matrix and its inverse.
makeCacheMatrix <- function(x = numeric()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(mean) m <<- mean
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## Write a short comment describing this function
## this function checks if the inverse matrix was alreday computed.
## if yes -> return the cache inverse value
## if not -> solves for the inverse, and returns it

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
