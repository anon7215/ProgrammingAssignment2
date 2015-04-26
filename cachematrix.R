## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        ## create a null matrix
        m <- NULL
        
        ## Set the cache matrix value using the set function
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        ## Get the cache matrix value
        get <- function() x
        
        ## Set the inverse of the matrix
        setinverse <- function(inverse) m <<- inverse
        
        ## Get the inverse of the matrix
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
             
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
}
