## The following two functions are able to create a special matrix
## able to cache the inverse of the matrix in an object for later
## use.

## This function will create a matrix and make a list containing
## the functions to set and get the values of the matrix and the
## inverse of the matrix in order to cache the inverse.

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setinv <- function(solve) m <<- solve
	getinv <- function() m
	list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## This function computes the inverse of the special matrix, returned
## by the makeCacheMatrix function above. If the inverse has already
## been calculated and hasn't changed, it will retrieve the inverse
## from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	m <- x$getinv()
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	data <- x$get()
	m <- solve(data, ...)
	x$setinv(m)
	m
}