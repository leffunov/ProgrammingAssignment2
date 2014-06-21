
## makeCacheMatrix creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
                }
        get <- function() x
        setInv <- function(mean) m <<- mean
        getInv <- function() m
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
}

## cacheSolve computes the inverse of the special matrix. if the inverse has already been computed,
## then the function will retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInv(m)
        m
}
