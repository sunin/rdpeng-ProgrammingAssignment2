## this function will create a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x=matrix()) {
	inverse <- NULL
	if(NROW(x) == NCOL(x)) {
		set <- function(y) {
			x <<- y
			inverse <- NULL
		}
		get <- function() x
		setinverse <- function(solve) inverse <<- solve
		getinverse <- function() inverse
		list(set = set, get = get, setinverse = setinverse, 
             getinverse = getinverse )
	}
	else {
		print("[*] matrix is not square, please give square matrix")
	}
}


## this function will compute the inverse of a matrix returned by "makeCacheMatrix" function 


cacheSolve <- function(x,...) {
	inverse <- x$getinverse()
	if(!is.null(inverse)) {
		message("getting cached data ")
		return (inverse)
	}
	data <- x$get()
	inverse <- solve(data, ...)
	x$setinverse(inverse)
	inverse
	}

