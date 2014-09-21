## makeCacheMatrix creates an object with four methods - get, set, getinverse and setinverse
## Calls to set will invalidate the cache

makeCacheMatrix <- function(x = matrix()) {

    inv <- NULL
    
    ## implements the set method
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    ## implements the get method
    get <- function() x
    
    ## implements the set inverse method
    setinverse <- function(y) inv <<- y
    
    ## implements the get inverse method
    getinverse <- function() inv
    
    ## returns the completed object
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve will return the inverse of a given cacheMatrix object.
## if the cacheMatrix object does not have a result for the inverse cached, it will be calculated and stored

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

    ## Attempts to get the cached inverse of the matrix
    inv <- x$getinverse()
    
    ## Checks if the cache had a value, if so, returns it
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    ## Solves for the inverse of the matrix, and store it in the cache
    inv <- solve(x$get())
    x$setinverse(inv)
    inv
}
