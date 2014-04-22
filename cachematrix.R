## The first function, makeCacheMatrix, creates a special "matrix", which is a list
## containing a function to
##       set the value of the matrix
##       get the value of the matrix
##       set the value of the inv (inverse of the matrix)
##       get the value of the inv (inverse of the matrix)

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) inv <<- solve
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The following function, cacheSolve, calculates the inverse of the special "matrix" created with
## the first function, makeCacheMatrix. However, it first checks to see if the inverse
## has already been calculated.
## If it has, it gets the inverse of the matrix from the cache and skips the computation.
## Otherwise, it calculates the inverse of the matrix and sets the value of the inverse
## in the cache via the setinverse function.

## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}

## Usage:
## myM <- matrix(c(2, 3, 4, 5), 2, 2) # creates a matrix myM
## mM <- makeCacheMatrix(myM) # creates special matrix mM; we can also call makeCacheMatrix with
## matrix(...) directly, instead of creating a matrix separately.
## cacheSolve(mM) # finds an inverse of the matrix mM
## calling cacheSolve(mM) again will find the cached version of the ineverse,
## thus skipping computation, and print "getting cached data"
## In order to check the validity, we can assign inverse of the matrix to invM <- cacheSolve(mM)
## make the special matrix of that invMnow <- makeCacheMatrix(invM)
## and finally find the inverse of it, which will give us the original matrix mM,
## cacheSolve(invMnow)