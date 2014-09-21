
# creates a special matrix which can be cached in memory
makeCacheMatrix <- function(x = matrix()) {

	i <- NULL

	set <- function (y) {
		x <<- y
		i <<- NULL
	}

	get <- function () x 

	setInverse <- function(inverse) i <<- inverse
	getInverse <- function () i

	list ( set = set, get = get, setInverse = setInverse, getInverse = getInverse )
}

#it computes the inverse of matrix returned by the "makeCacheMatrix". It returned the inverse if it already computed, else it computes, stores and return
cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'

	m <- x$getInverse()

	if (!is.null(m) ) {

		message("getiing cached data")
		return(m)
	}

	data <- x$get()
	m <- solve(data) %*% data

	x$setInverse(m)
	m
}
