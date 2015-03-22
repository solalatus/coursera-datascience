makeCacheMatrix <- function(x = matrix()) {
	inv <- NaN
  	set <- function(y) {
                x <<- y
                inv <<- NaN
        }
        get <- function() x
        setinverse <- function(sol) inv <<- sol
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}

cacheSolve <- function(x, ...) {
        inve <- x$getinverse()
        if(!is.nan(inve)) {
                message("getting cached data")
                return(inve)
        }
        data <- x$get()
        inve <- solve(data, ...)
        x$setinverse(inve)
        inve
}
