## These functions will take in a matrix, cache it produce the inverse.
## It is going to cache the computed inverse so that if it is called a second time
## it doesn't have to waste time re-computing the inverse.

## This function will take in a matrix and cache it.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(inv) m <<- inv
        getinv <- function() m
        list(set = set, get = get, 
             setinv = setinv, 
             getinv = getinv)
}


## This function will first check if the inverse of the matrix from makeCacheMatrix has already been computed.
## If not, it will re-calculate the inverse and return it. If it's already been computed, it will return the
## cached version.

cacheSolve <- function(x, ...) {
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data.")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setinv(m)
        m
}
