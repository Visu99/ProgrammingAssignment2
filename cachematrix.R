## 10/23/2015 Author: Vish Yella
## This file contains source code for two functions makeCacheMatrix and cacheSolve
##
## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##      If the inverse has already been calculated (and the matrix has not changed),
##      then cacheSolve retrieve the inverse from the cache.
## 
## These functions demonstrate the features of below operators
##      assignment operator: <-
##      superassignment operator: <<-
 

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(varMatrix = matrix()) {
        varInv <- NULL
        set <- function(argMatrix){  
                varMatrix <<- argMatrix # set the data varMatrix
                varInv    <<- NULL      # not specified yet so initialize it
        }
        get <-function() varMatrix  # just return data varMatrix
        

        setinverse <- function(argInv) varInv <<- argInv
        getinverse <- function() varInv
        
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##      If the inverse has already been calculated (and the matrix has not changed),
##      then cacheSolve retrieve the inverse from the cache.
cacheSolve <- function(argMatrix, ...) {
         
        varInv <- argMatrix$getinverse()  # get inverse that's already there within matrix object 
        if(!is.null(varInv)){             # if it is already computed, just return that
                message("getting cached data")
                return(varInv)
        }
        # inverse is not already computed, so compute now
        matrixData <- argMatrix$get()
        varInv <- solve(matrixData,...)
        argMatrix$setinverse(varInv)  # assign the inverse to given matrix object
        varInv # return the freshly computed inverse
}
