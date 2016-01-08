## Create a special object that stores a special matrix and
## caches its mean.

## Create a matrix (a list containing functions to set/get values, and set/get
## inverses)

makeCacheMatrix <- function(x = matrix()) {
 		m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Calculate inverse of matrix created with makeCacheMatrix 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) { ##checks to see if inverse is calculated
                message("getting cached data")
                return(m) ## gets inverse and skips computation
        }
        data <- x$get()
        m <- solve(data, ...) ##inverse of matrix
        x$setinverse(m)       ##sets value of inverse in the cache
        m
}
