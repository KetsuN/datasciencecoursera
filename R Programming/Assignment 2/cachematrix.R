##  File Name:
##		cachematrix.R
##	
##	File Description:
##		This file contains two functions that create and use a cached matrix 
##		object.  A cached matrix object contains an square, invertible matrix 
##		and a potential inverse of the original.   
##
##	Author:
##		Tim "KetsuN" Butler


##  Function Name:
##		makeCacheMatrix
##	
##	Function Description:
##		Create a cached matrix. A cached matrix object contains a normal matrix
##		(assumed to be square and invertible) and provides four functions:
##			- set: sets the matrix stored in the cached matrix and nullifies 
##				   the inverse
##			- get: returns the matrix stored in the cached matrix
##			- setinverse: sets the inverse variable in the cached matrix to the
##						  provided
##			- getinverse: returns the inverse variable
##
##	Author:
##		Tim "KetsuN" Butler
makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL
	
	##  Function: Set the matrix to the provided matrix
	##  This new matrix is assumed to be invertible
	set <- function(y) {
			x <<- y
			inverse <<- NULL
	}
	
	##  Function: Return the cached matrix
	get <- function() {
		x
	}
	
	##  Function: Find the inverse of the matrix
	setinverse <- function(newInverse) {
		inverse <<- newInverse
	}
	
	##  Function: Return the inverse of the matrix
	getinverse <- function() {
		inverse
	}
	
	##  Define functions can can be performed with this data
	##  Return the functions
	list(set = set, 
		 get = get,
		 setinverse = setinverse,
		 getinverse = getinverse)
}


##  Function Name:
##		cacheSolve
##	
##	Function Description:
##		Solve for the inverse of a cached matrix.  
##		When solving for the inverse of a cached matrix, the solveCache 
##		function determines if the inverse has already been solved.  If
##		it has, use the cached inverse.  If not, calculate the inverse 
##		and store it in the cached matrix object.	
##
##	Author:
##		Tim "KetsuN" Butler
cacheSolve <- function(x, ...) {
    ##  Retrieve the cached inverse of the matrix 
	inverse <- x$getinverse()
	
	##  Determine if a cached inverse matrix exists (is not null)
	if(!is.null(inverse)) {
		##  Cached inverse exists, return it
		message("getting cached data")
		return(inverse)
	}
	
	##  Retrieve the original matrix
	inverseData <- x$get()
	
	##  Calculate the inverse of the original matrix 
	inverse <- solve(inverseData, ...)
	
	##  Provide an inverse value to the matrix
	x$setinverse(inverse)
	
	##  Report the inverse
	inverse
}
