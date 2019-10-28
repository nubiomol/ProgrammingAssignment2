## Put comments here that give an overall description of what your
## functions do

#------------------------------------------------------------------------------
#       Assignment: Caching the Inverse of a Matrix 
#------------------------------------------------------------------------------
#
#       Matrix inversion is usually a costly computation and there may be some
#       benefit to caching the inverse of a matrix rather than compute it 
#       repeatedly (there are also alternatives to matrix inversion that we 
#       will not discuss here).
#
#       In this assignment, two functions that cache the inverse of a matrix
#       are defined. For this functions, the supplied matrix is assumed to be
#       a square invertible matrix. The inverse matrix will be computed using
#       'solve" function in R.
#------------------------------------------------------------------------------

#------------------------------------------------------------------------------
# makeCacheMatrix
#------------------------------------------------------------------------------
#       This function creates a special "matrix" object that can cache its
#       inverse.
#------------------------------------------------------------------------------

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        set_invmatrix <- function(solve) m <<- solve
        get_invmatrix <- function() m
        list(set = set,
             get = get,
             set_invmatrix = set_invmatrix,
             get_invmatrix = get_invmatrix)
}

#------------------------------------------------------------------------------
# cacheSolve
#------------------------------------------------------------------------------
#       This function computes the inverse of the special "matrix" returned by
#       makeCacheMatrix above.
#------------------------------------------------------------------------------

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$get_invmatrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$set_invmatrix(m)
        m
}

#------------------------------------------------------------------------------
# Example test
#------------------------------------------------------------------------------
# 1. creates a random square matrix:
#
#       input_matrix <- matrix(sample(1:16),4,4)
#
#       
# 2. cache its inverse matrix:
#
#       cache_matrix <- makeCacheMatrix(input_matrix)
#       cache_solve<-cacheSolve(cache_matrix)
#
#
# 3. check if the cache matrix is identical to the inverse matrix 
#
#       inverse_matrix <- solve(input_matrix)
#       cache_solve == inverse_matrix
#------------------------------------------------------------------------------
