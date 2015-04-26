## Put comments here that give an overall description of what your functions do

##  Pair of functions that cache the inverse of a matrix.

##   makecachematrix function creates a special "vector", which is really a list containing a function to
##      set the value of the matrix             ##      get the value of the matrix
##      set the value of the inverse            ##      get the value of the inverse

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        ## create a null matrix for storing inverse of the matrix
        inv <- NULL
        
        ## Set the cache matrix value using the set function
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        ## Get the cache matrix value
        get <- function() x
        
        ## Set the inverse of the matrix
        setinverse <- function(inverse) inv <<- inverse
        
        ## Get the inverse of the matrix
        getinverse <- function() inv
        
        ## the special vector containing four functions is returned
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
             
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {

        inv <- x$getmean()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- mean(data, ...)
        x$setmean(m)
        m
        
        
        ## Return a matrix that is the inverse of 'x'
}
