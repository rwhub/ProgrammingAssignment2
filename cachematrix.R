makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


cacheSolve <- function(x, ...) {
    inverse <- x$getinverse()
    
    if(!is.null(inverse)) {
        message("getting cached data.")
        return(inverse)
    }
    data <- x$get()
    # matrix needs to be square for solve
    inverse <- solve(data)
    x$setinverse(inverse)
    inverse
}

