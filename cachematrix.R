## In this assignment we write functions to cache the inverse of a matrix rather than computing it repeatedly

## makeCacheMatrix function creates a matrix object and caches its inverse.
makeCacheMatrix <- function(x = matrix()) {  
    ## Initialize matrix inverse to NULL
    inv <- NULL    
    ## Function to set the matrix whose inverse is to be calculated
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    ## Function to get the matrix whose inverse is to be calculated
    get <- function() {
        x
    }    
    ## Function to set the inverse of the matrix
    set_inverse <- function(inverse) {
        inv <<- inverse
    }
    ## Function to get the inverse of the matrix
    get_inverse <- function() {
        inv
    }    
    ## Create a list of functions defined inside makeCacheMatrix function
    list(set = set, get = get,
        set_inverse = set_inverse,
        get_inverse = get_inverse)
}



## cacheSolve function computes the inverse of the matrix and returns it
cacheSolve <- function(x, ...) {        
    inv <- x$get_inverse()  
    ##  If the inverse has already been calculated (and the matrix has not changed), 
    ##  then retrieve the inverse from the cache. 
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }  
    ## If the inverse has not been already calculated, calculate the inverse of the matrix 
    ## and set the inverse in the cache via the set_inverse function.
    mat <- x$get()
    inv <- solve(mat, ...)
    x$set_inverse(inv)
    inv
}

