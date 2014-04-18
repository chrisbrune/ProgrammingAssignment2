## Caches the matrix and sets the inverse mean
makeCacheMatrix <- function(x = matrix()) {
            m <- NULL
            set <- function(y) {
                    x <<- y
                    m <<- NULL
            }
            get <- function() x
            setinversemean <- function(inversemean) m <<- inversemean
            getinversemean <- function() m
            list(set = set, get = get,
                 setinversemean = setinversemean,
                 getinversemean = getinversemean)
    
}


## Checks to see if inverse mean is cached, if not than it caches the inveser mean using the solve function


cacheSolve <- function(x) {
        ## Return a matrix that is the inverse of 'x'
            
            m <- x$getinversemean()
            if(!is.null(m)) {
                    message("getting cached data")
                    return(m)
            }
            data <- x$get()
            m <- solve(data)
            x$setinversemean(m)
            return (m);

}
