## Put comments here that give an overall description of what your
## functions do

# This function intializes an empty matrix to store 
# the value of a matrix inverse for cacheSolve to reference
# the function also develops some additional funcitons for cacheSolve
# to use, either to get from parent env. or to calculate from the matrix for 
# which makeCacheMatrix was called.
 

makeCacheMatrix <- function(x = numeric()) {
        inv <- NULL #initializes an empty inverse matrix
        set <- function(y){
                x <<- y
                inv <<- NULL
        } #sets generic values for x and inverse matrix in parent env.
        get <- function() x #function to get the values of obj. x
        setinv <- function(solve) inv <<- solve #function to solve for matrix inv. saved in parent env.
        getinv <- function() inv #function to retreive inv. from parent env.
        list(set = set, get = get, 
             setinv = setinv,
             getinv = getinv) #list of objects which are function output
}


# This function retreives a cached matrix that is the inverse of 
# the matrix passed to makeCacheMatrix, or caculates the inverse if 
# it does not exist as an object (in any parent environment) already

cacheSolve <- function(x, ...) {
       inv <- x$getinv()  # Return a matrix that is the inverse of 'x' or null if inv is not yet calcd.
       if(!is.null(inv)) {
               message("getting cached data")
               return(inv) #code for case where inv is already populated
       }
        data <- x$get() #assigns matrix passed to makeCacheMatrix to "data"
        inv <- solve(data) #calculates the inverse of data
        x$setinv(inv) #sets the inverse values for future iterations
       inv #returns the inverse to an object or the active console
}
