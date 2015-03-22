#########################################################################
##cachematrix.R:							#
##	Created for the Programming Assignment #2 in			#
## 	the 'R Programming Course' on Coursera.  This file contains	#
## 	two functions: makeCacheMatrix & cacheSolve. Both are based	#
## 	on code examples provided by the instructors of the course  	#
#########################################################################


## makeCacheMatrix:  This function creates a special "matrix"
##		     object that can cache its inverse
##		     The function returns a list of the methods
##                   available:  set, get, setinverse, getinverse


makeCacheMatrix <- function(x = matrix()) {

## Initialize the 'inverse' matrix	
	i <- NULL
	
## Create 'set' method. To be used, if needed, to reset the
## cached 'inverse' matrix
	set <- function(y) {
		x <<- y
		i <<- NULL    
	}

## Create 'get' method.
        get <- function() x

## Create 'setinverse' method, used to assign cached inverse matrix
        setinverse <- function(inverse) i <<- inverse

## Create 'getinverse' method
        getinverse <- function() i

## Return list of methods available for this function
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)		

}


## cacheSolve:	This function computes the inverse of the special
##		"matrix" returned by makeCacheMatrix.  If the 
##     		inverse has been calculated previously then it will
##		not be recacalculated and the cached value is retrieved

cacheSolve <- function(x, ...) {
	
## Gets the 'i' value from the x environment and assigns to local the local 'i'
	i <- x$getinverse()
	
## If the inverse has been calculated previously, a message is printed 
## and the cached value is returned		
	if (!is.null(i)) {
		message("getting cached data")
		return(i)
	}

## If inverse not calculated previously, pull the x matrix into the
## local variable: 'data'
 	data <- x$get()
	
## Calculate the inverse, set, the cache value and return the result
	i <- solve(data, ...)
	x$setinverse(i)
	i
        
}
