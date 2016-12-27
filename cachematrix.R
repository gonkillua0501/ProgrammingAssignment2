## Matrix inversion is a costly computation for large matrix
## These functions are to support caching the inverse of a matrix

## Function to create a special matrix which supports caching of its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv_matrix_ <- NULL
    
    Set <- function(y) {
        x <<- y
        inv_matrix_ <<- NULL
    }
    
    Get <- function() x
    
    SetInvMatrix <- function(inv_matrix) inv_matrix_ <<- inv_matrix
    
    GetInvMatrix <- function() inv_matrix_
    
    list(Set=Set, Get=Get, SetInvMatrix=SetInvMatrix, GetInvMatrix=GetInvMatrix)
}


## Function to compute the inverse of a special matrix created by makeCacheMatrix
## This function first check to see if the inverse matrix has already been computed
## If so, it gets the inverse matrix from the cache and skips the computation. 
## Otherwise, it calculates the inverse matrix and store the inverse matrix in the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv_matrix <- x$GetInvMatrix()
    
    if (!is.null(inv_matrix)) {
        message("Getting cached inverse matrix")
        return(inv_matrix)
    }
    
    data <- x$Get()
    inv_matrix <- solve(data)
    x$SetInvMatrix(inv_matrix)
    inv_matrix
}
