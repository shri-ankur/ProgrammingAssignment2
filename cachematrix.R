## The functions written here return the inverse of a matrix and in case the matrix inverse has
## been calculated, return a cached value of the matrix inverse instead of redoing the calculation.

## The function makeCacheMatrix creates a special matrix object and returns a list containing
## functions to
## 1) set the value of matrix
## 2) get the value of matrix
## 3) set the value of inverse matrix (cache the value of inverse matrix)
## 4) get the value of inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        
        #set the value of setinverse
        setinverse <- function(inverse) inv <<- inverse
        
        getinverse <- function() inv
        
        list(set = set, get = get, setinverse = setinverse,
             getinverse = getinverse)

}


## cacheSolve function computes the inverse of the matrix object created by the makeCacheMatrix
## function. If the inverse has already been calculated (and the matrix object has not changed), then
## this function retrieves the inverse value from cache


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        
        data <- x$get()
        
        inv <- solve(data, ...)
        
        x$setinverse(inv)
        
        inv
}
