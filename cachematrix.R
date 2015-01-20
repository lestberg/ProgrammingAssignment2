## The functions below cache a matrix and its inverse as needed 

## The 'makeCacheMatrix function creates a special matrix Object that will
##      store a matrix 'x' and its' inverse 'X_INV', along with 4 functions ... 
##              1. 'set' the matrix value
##              2. 'get' the matrix value
##              3. set the matrix inverse value ('setMatrixInv')
##              4. get the matrix inverse value ('getMatrixInv')

makeCacheMatrix <- function(x = matrix()) {
        ## initialize an empty matrix inverse
        X_INV <- NULL
        
        ## set the matrix value and initialize an empty matrix inverse
        set <- function(y) {
                x <<- y
                X_INV <<- NULL
        }
        
        ## get the matrix value
        get <- function() x
        
        ## set the matrix inverse value
        setMatrixInv <- function(X_Inverse) X_INV <<- X_Inverse
        
        ## get the matrix inverse value
        getMatrixInv <- function() X_INV
        
        ## return the 4 functions embedded in the special matrix 
        ##      just created
        list(set = set, get = get, setMatrixInv = setMatrixInv,
             getMatrixInv = getMatrixInv)
}

## The 'cacheSolve' function returns the inverse of the supplied matrix 'x'
## If the invervse was calculated previously, it was stored as
##      X_INV in the special matrix object for 'x', and is
##      retreived for return
## If the inverse was not calculated previously, it is calculated,
##      stored, and returned

cacheSolve <- function(x, ...) {
        ## attempt to retreive a previously calculated inverse
        X_INV <- x$getMatrixInv()
        
        ## if the inverse was calculated previously
        if(!is.null(X_INV)) {  
                message("getting cached data")
                return(X_INV)
        }
        
        ## if the inverse was not caluculated previously
        matrix <- x$get()  
        X_INV <- solve(matrix, ...)
        x$setMatrixInv(X_INV)
        
        ## Return a matrix that is the inverse of 'x'
        X_INV  
}
