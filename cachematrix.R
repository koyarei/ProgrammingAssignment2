## this program calculates the inverse
## of a matrix. To minimize computing power needed, previously calculated
## inverse will be cached and retrieved when calculation is called upon.

## makeCacheMatrix function calculates the inverse of a matrix and stores it 
## in the cache; it has 4 sub-functions
makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    ## set the initial matrix
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    ## retrieve the matrix set by previous function
    get <- function() x
    ## force store the result of the solve, no calculation invovled here
    setSolve <- function(solve) s <<- solve
    ## retrieve the result of the solve
    getSolve <- function() s
    list(set = set, get = get,
         setSolve = setSolve,
         getSolve = getSolve)
    
}

## cacheSolve first checks if the solve exists
## if it exists, retrieve it without calcuation;
## if not, calcuates the solve using makeCacheMatrix function

cacheSolve <- function(x, ...) {
    ## first, attempts to retrieve result from getSolve from makeCacheMatrix
    s <- x$getSolve()
    ## if result does exist, immediately return without further calculation
    if(!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    ## if result does not exist, proceed to calculate the solve
        message("calculating")
        data <- x$get()
        s <- solve(data, ...)
        x$setSolve(s)
        s
    
}
