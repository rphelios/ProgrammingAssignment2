## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix() creates a list of functions to create a special matrix 
## object and to cache its inverse


makeCacheMatrix <- function(x = matrix()) {

        ## set the value of the matrix, initialise cacheMatrix to NULL
        cacheMatrix <- NULL
        setMatrix <- function(y){
                x <<- y
                cacheMatrix <<- NULL
        }
        
        # get the value of the matrix
        getMatrix <- function() x
        
        # set the value of the matrix inverse and store in cache
        setInverse <- function( inverseMatrix ) cacheMatrix <<- inverseMatrix
        
        # retrieve the value of the matrix inverse from cache
        getInverse <- function() cacheMatrix
        
        # make functions availble to in working environment
        list(setMatrix = setMatrix, getMatrix = getMatrix, 
             setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve() computes the inverse of a matrix created by makeCacheMatrix
## If the matrix inverse does not exist in the cache, it is computed in the
## working environment and subsequently stored in the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        cacheMatrix <- x$getInverse()

        # retrieve cached matrix if inverse already stored in cache
        # and print to screen
        if( !is.null(cacheMatrix) ) {
                message("Getting cached matrix")
                return(cacheMatrix)
        }
        # if not in cache, compute and commit to cache
        M <- x$getMatrix()
        
        ## (Assignment allows for the assumption that the supplied matrix 
        ## invertible. The below is meant to be a test of invertibility.)
        
        # check if new matrix is invertible and commit to cache if so, else
        # return NULL with message
        tryCatch( 
        {
                cacheMatrix <- solve(M)
        }, 
        warning = function(war) {
                message(paste("Matrix is not invertible. Returning NULL."))
                #message(war)
                return(NULL)
        },
        error = function(err) {
                message(paste("Matrix is not invertible. Returning NULL."))
                        #message(err)
                        return(NULL)
        },
        finally = {
                # matrix is invertible, so commit to cache...
                x$setInverse(cacheMatrix)
        })
                                             
        # return matrix inverse from cache to screen
        return(cacheMatrix)
}




## --- CODE TEST ---:
## Create a square invertible matrix m
## > m <- rbind(c(1,1),c(-1,2))
## > m
##      [,1] [,2]
## [1,]    1    1
## [2,]   -1    2
##
## Create special matrix object 'testM' with same structure as 'm'
## > testM <- makeCacheMatrix()
## > testM$setMatrix(m)
## Test to see if 'testM' object has same structure as 'm'
## > testM$getMatrix()
##      [,1] [,2]
## [1,]    1    1
## [2,]   -1    2
##
## Compute inverse of 'testM' for the first time
## > cacheSolve(testM)
##      [,1]       [,2]
## [1,] 0.6666667 -0.3333333
## [2,] 0.3333333  0.3333333
##
## Compute inverse of 'testM' for the second time, this time retrieve
## inverse matrix from cache
## > cacheSolve(testM)
## Getting cached matrix
##      [,1]       [,2]
## [1,] 0.6666667 -0.3333333
## [2,] 0.3333333  0.3333333
##
## Try to do the same with a non-invertible matrix
## > m2 <- rbind(c(1,1),c(1,1))
## testM$setMatrix(m2)
## testM$getMatrix()
##      [,1] [,2]
## [1,]    1    1
## [2,]    1    1
## > cacheSolve(testM)
## Matrix is not invertible. Returning NULL.
## NULL
