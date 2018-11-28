## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## Will check if given type is not null.
is.not.null <- function(x) ! is.null(x)

makeCacheMatrix <- function(x = matrix()) {

	get <- function() x
	setInverse <- function(inverseMatx) inversedMatrix <<- inverseMatx
	getInverse <- function() inversedMatrix
	## init inversedMatrix with  null.
	inversedMatrix <- NULL
	set <- function(y) {
		x <<- y
		## init inversedMatrix with  null.
		
		inversedMatrix <<- NULL
		## This is optimisation check if new matrix already has inversed matrix than use same.
		## Check if atomic than bypass optimisation.
		if(!is.atomic(y)) {
		## Get Inv matrix of the new matrix.
		invMatx = y$getInverse()
			if (is.not.null(invMatx)) {
				message("Found cached version of inverse matrix so will use same")
				inversedMatrix <<- invMatx					                
			}
		}
	}
		
	list(set = set,
		 get = get,
		 setInverse = setInverse,
		 getInverse = getInverse)
}

cacheSolve <- function(inputMatrix, ...) {
        ## Return a matrix that is the inverse of 'inputMatrix'
        inverseOfMatrix <- inputMatrix$getInverse()
        if (is.not.null(inverseOfMatrix)) {
                message("Found cached version of inverse matrix")
                return(inverseOfMatrix)
        } 
		else {		
		message("Cached version of inverse matrix  not found. Will create inverse and cache it")
		}
        tempMatx <- inputMatrix$get()
		## Compue the inverse
        inverseOfMatrix <- solve(tempMatx, ...)
		## Set the inver matrix
        inputMatrix$setInverse(inverseOfMatrix)
        ## Print the inversed matrix.
		inverseOfMatrix
}
