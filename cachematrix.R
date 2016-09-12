## Program for week 3 - coding assignment #2

## 2016-09-11 - CMA - adapted from sample cacheMean program by Roger Peng

## makeCacheMatrix:  Function creates a special "matrix" object with a cache of its inverse.  Input will always be invertible square matrix.

## As in "cachemean" example, create a function to create a special "vector" - really a list containing a function to do 4 things:
## 1.set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {

  ## 1. Create function set for matrix to be replaced and cached on 1st run.
  InvMatrix <- NULL
  set <- function(y) {
	x <<- y
	InvMatrix <<- NULL 
  }

  ## 2. Get the value of the matrix.
  get <- function() x

  ## 3. Set the value of the inverse.
  setInverse <- function(solve) InvMatrix <<- solve

  ## 4. Get the value of the inverse.
  getInverse <- function() InvMatrix

  list(set = set, get = get,
	setInverse = setInverse,
	getInverse = getInverse)

}


## cacheSolve:  function to calculate the inverse of the special "matrix" object created by makeCacheMatrix(). If the inverse has already been computed and the matrix has not changed, then function retieves the inverse from the cache and skips the computation.  Otherwise, computes the inverse using the SOLVE function and returns it.

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x' - from cache if available and x has not changed -- otherwise, compute the inverse anew.

	InvMatrix <- x$getInverse()

	## Ideally, we would have some criteria to ensure the input matrix has not changed, but several attempts did not pan out, so we will keep it simple...
	 if( !is.null( InvMatrix ) ) {

		message("NOTE: Using cached inverse matrix to save resources.")
		return(InvMatrix)
	}
	message("NOTE: Computing inverse since no cached inverse matrix found.")
	RawMatrix <- x$get()
	InvMatrix <- solve(RawMatrix, ... )
	x$setInverse(InvMatrix)
	InvMatrix

}

