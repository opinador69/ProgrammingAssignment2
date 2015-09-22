## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
	inv<-NULL # Initializes inverted matrix

	# set: function changes the matrix
	set<-function (y=matrix()) {
		x<<-y
		inv<<-NULL
	}

	# get: function returns the matrix contents
	get<- function () x

	# setinvert: function calculates the inverted matrix
	setinvert<- function(inverted) inv<<-inverted

	# getinvert: function returns the inverted matrix
	getinvert<- function() inv

	list(set = set, get = get,
             setinvert = setinvert,
             getinvert = getinvert)	
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
	inv<-x$getinvert()
	if(!is.null(inv)){
		message("cache data")
		return(inv)
	}
	data<-x$get()
	inv<-solve(data,...)
	x$setinvert(inv)
	inv
}


