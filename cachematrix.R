## These two functions are used to calculate the inverse of a matrix an store
## the result in a cache, in order to avoid making the same operation repeatedly.

################################################################################

## Creates a closure composed of variables that store the matrix and its inverse,
## and a set of functions used to access and modify the values of the matrix
## and its inverse.
##
## Argument: the matrix to be inverted.
## Returns: a list containing the functions that manipulate the matrix and its
## inverse.
makeCacheMatrix <- function(x = matrix()) {
	i <- NULL

	set <- function(y) {
		x <<- y
		i <<- NULL
	}

	get <- function() x

	setinverse <- function(inverse) i <<- inverse

	getinverse <- function() i

	list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)
}


################################################################################

## Given a list created using the 'makeCacheMatrix' function, this function
## calculates the inverse of a matrix, or returns a previously cached value.
##
## Argument: a list created using the function 'makeCacheMatrix'
## Returns: the inverse of the matrix 
cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
	i <- x$getinverse()

	if(!is.null(i)) {
		message("getting cached data")
		return(i)
	}

	data <- x$get()
	i <- solve(data, ...)
	x$setinverse(i)

	i
}
