## I have 2 functions here
## together they can be used for caching a matrix's inverse and calling it from cache

## makeCacheMatrix function takes a matrix as an argument and creates a "special" matrix
## that can be used for caching

makeCacheMatrix <- function(matrixx = matrix()) {
        
        stored_inverse <- NULL
        
        set <- function(y) {
                matrixx <<- y
                stored_inverse <<- NULL
        }
        
        get <- function() {
                return (matrixx)
        }
        
        set_inverse <- function(sent_replacement_inverse){
                stored_inverse <<- sent_replacement_inverse
        }
        
        get_inverse <- function() {
                return(stored_inverse)
        }
        
        
        list(set = set, get = get,
             set_inverse = set_inverse,
             get_inverse = get_inverse)
}


## cacheSolve function returns the inverse of the matrix that has been created with makeCacheMatrix
## function above
## If the inverse has been computed before, it returns the inverse from cache

cacheSolve <- function(madeMatrix, ...) {
        
        local_inverse <- madeMatrix$get_inverse()
        
        if(!is.null(local_inverse)) {
                message("getting cached data")
                return(local_inverse)
        }
        else {
                local_data <- madeMatrix$get()
                local_inverse <- solve(local_data, ...)
                
                madeMatrix$set_inverse(local_inverse)
                return(local_inverse) 
        }
}
