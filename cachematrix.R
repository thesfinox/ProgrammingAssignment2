## These functions cache the inverse of a matrix to avoid repeatedly computing
## them.

## This function creates a matrix with the cached value of its inverse by
## saving it in an internally stored value accessible through setters and
## getters.

makeCacheMatrix <- function(x = matrix()) {

    # start by instantiating the object (the inverse of the matrix)
    inv <- NULL

    # create a function to set the value of the matrix (i.e. setter)
    set <- function(y) {
        x <<- y #--------------- set x to equal an outside value
        inv <<- NULL #---------- reset the inverse of the matrix
    }

    # create a function to return the value of the matrix (i.e. getter)
    get <- function() { x }

    # create a function to compute the inverse of the matrix
    setinverse <- function(inverse) { inv <<- inverse }

    # create a function to recover the value of the inverse
    getinverse <- function() { inv }

    # return a list of the two setters and the two getters
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function then computes the inverse of the matrix or retrieves the
## previously computed value.

cacheSolve <- function(x, ...) {
    inv <- x$getinverse() #------------------------ get the value of the inverse
    if(!is.null(inv)) { #-------------------------- if it is not NULL
        message("Retrieving cached data...") #----- debug message
        return(inv) #------------------------------ return the cached result
    }

    # if the value is not cached
    inv <- solve(x$get(), ...) #------------------- compute the inverse
    x$setinverse(inv) #---------------------------- cache the value of the inverse
    return(inv) #---------------------------------- return the result
}
