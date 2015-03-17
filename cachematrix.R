## This script contains a pair of functions that invert a matrix and,
## cache it in a parent environment. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cacheSolve should retrieve the inverse from the cache.


## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) 
        {
        inv <- NULL
        set <- function(y) 
                {
                x <<- y
                inv <<- NULL
                }
        get <- function() x
        setinverse <- function(solve) inv <<- solve
        getinverse <- function() inv
        list(set = set, get = get,
                setinverse = setinverse,
                getinverse = getinverse)
        }

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
cacheSolve <- function(x, ...) 
        {
        inv <- x$getinverse()
        if(!is.null(inv)) 
                {
                message("The inverse was already calculated. Getting cached data")
                return(inv)
                }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv		
        ## Return a matrix that is the inverse of 'x'
        }
