#### Put comments here that give an overall description of what your functions do

##  Pair of functions that cache the inverse of a matrix.
##  Assumption : The matrix supplied as argument to the function is always invertible.


#### Write a short comment describing this function

##   makecachematrix function creates a special "vector", which is really a list containing a function to
##      set the value of the matrix             ##      get the value of the matrix
##      set the value of the inverse            ##      get the value of the inverse



makeCacheMatrix <- function(x = matrix()) {

        ## create a null matrix for storing inverse of the matrix
        inv <- NULL
        
        ## Set the  matrix value using the set function. inverse is set to null if a new matrix is set (changed)
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        ## Get the  matrix value
        get <- function() x
        
        ## Set the inverse of the matrix
        setinverse <- function(inverse) inv <<- inverse
        
        ## Get the inverse of the matrix
        getinverse <- function() inv
        
        ## the special list vector containing four functions is returned
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
             
}


#### Write a short comment describing this function

## cacheSolve function computes the inverse of a square matrix returned by makeCacheMatrix function. 
## Assumption : The matrix supplied as argument to the function is always invertible.
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve retrieve the inverse from the cache value.

cacheSolve <- function(x, ...) {

        ## Get the inverse of the matrix from the cache value using getinverse function from above.
        inv <- x$getinverse()
        
        ## Check if the cache inverse matrix is not null.
        ## if cache inverse is not null display the message and return inverse matrix
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        
        ## Get the matrix from the cache value using get function from above. Cache inverse is null
        data <- x$get()
        
        ## calculate inverse of the matrix
        inv <- solve(data, ...)
        
        ## Set the inverse of the matrix to the cache inverse value
        x$setinverse(inv)
        
        ## Returning a matrix that is the inverse of 'x'
        inv
        
}
