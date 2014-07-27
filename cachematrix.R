# cachematrix.R: assignment 2 homework
# 


makeCacheMatrix <- function(x = matrix()) {
    #instantiate inv
    inv <- NULL

    set <- function(y) {
        x <<- y
        inv <<- NULL
    }

    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    
    #return function list
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


cacheSolve <- function(x, ...) {
    #leverage makeCacheMatrix()
    i <- x$getinverse()
    
    #if we have a cache get it and let the user know
    if(!is.null(i)) {
        message("getting cached data.")
        return(i)
    }
    #otherwise we get the data for solve()
    data <- x$get()
    
    # matrix needs to be square for solve()
    i <- solve(data)
    x$setinverse(i)

    #return matrix
    i
}

