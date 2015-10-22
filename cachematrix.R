## R programming assignment 2.
## Demonstrate use of lexical scoping and super-assignment operator by
## caching a matrix and its inverse, with a function to solve/cache or
## solve/lookup depending on whether the inverse has already been 
## calculated.

## Example use:
## > a <- makeCacheMatrix() #instantiate cacheMatrix object with NULL values
## > a$set(matrix(c(13,22,1,5,2,62,21,42,3), nrow=3)) #set matrix values
## > a$get()
##      [,1] [,2] [,3]
## [1,]   13    5   21
## [2,]   22    2   42
## [3,]    1   62    3
##
## > b <- cacheSolve(a)
## > b <- cacheSolve(a) #inverse has already been calculated and stored in a
## Inverse cached already: Getting cached data
##
## > print(b)
##              [,1]         [,2]        [,3]
## [1,]  0.490929705 -0.243197279 -0.03174603
## [2,]  0.004535147 -0.003401361  0.01587302
## [3,] -0.257369615  0.151360544  0.01587302



## makeCacheMatrix stores a matrix. The stored matrix will have set and get
## methods for accessing and resetting the stored matrix.
## There are also methods setinv and getinv for setting and retrieving the
## inverse matrix. setinv probably shouldn't be called directly...

makeCacheMatrix <- function(data = matrix()) {
    imat <- NULL #ensures that cached matrix is empty on function call
    set <- function(y) { ##set function updates contents of matrix
        data <<- y 
        imat <<- NULL #when set is called, reset cached matrix to NULL
    }
    get    <- function() data
    setinv <- function(ans) imat <<- ans #method takes ans (answer) as 
                                         #input and stores in parent 
                                         #environment (the cacheMatrix
                                         #list that is returned)
    getinv <- function() imat
    cacheMatrix <- list(set = set, get = get,
                   setinv = setinv,
                   getinv = getinv)
    return(cacheMatrix) ##explicit return for clarity
}


## cacheSolve returns a matrix that is the inverse of input matrix cM
## If the inverse has already been calculated it is looked up in the cache and 
## returned, else the inverse is calculated and the answer is set in the cache

cacheSolve <- function(cM, ...) {
    ## Return a matrix that is the inverse of 'cM' (the cacheMatrix object)
    imat <- cM$getinv() #get stored value of inverse from cacheMatrix object
    if(!is.null(imat)) { #if stored value is not NULL, return
        message("Inverse cached already: Getting cached data")
        return(imat)
    }
    data <- cM$get() #query x to get stored matrix data
    imat <- solve(data, ...) #call solve to find inverse of square matrix
    cM$setinv(imat) #set inverse in cacheMatrix object
    return(imat)  ##explicit return for clarity
}

