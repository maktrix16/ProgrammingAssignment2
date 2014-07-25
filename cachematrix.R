## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# makeCacheMatrix function takes a matrix as an argument. After getting matrix argument, it is 
# instantiated into a special matrix object that allows the retrieval ("get") and setting ("set")
# of both the matix value and inverse matrix value

makeCacheMatrix <- function(x = matrix()) {
    #initialize variable to be null for inverse of matrix
    inverseMatrix = NULL
    
    #add basic object functions to a list
    self.list <- list(
        
        #function to get original matrix
        get = function() x
        ,
        
        #function to set new matrix value
        set = function(alt_mat){
            x <<- alt_mat
            inverseMatrix <<- NULL
        },
        
        #function to get inverse of matrix value stored
        getInverse = function() inverseMatrix        
        ,
        
        #function to set inverse of matrix value calculated
        setInverse = function(alt_mat){
            inverseMatrix <<- alt_mat   
        }
        
    )
    
    #set the class name
    class(self.list) <- "makeCacheMatrix"
    
    #return functions in the form of list
    self.list
    
}


## Write a short comment describing this function
# cacheSolve takes the special matix object as argument and returns the inverse matrix value. It 
# checks whether the special matrix object has a cached inverse matrix value. If there is already one,
# it will skip the calculation and use it cached value. If there isn't one, then it will calculate
# the inverse matrix value using "solve()" function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    # check if there is exsisting inverse data cached in matrix object. Use cached data if found
    if (!is.null(x$getInverse())){
        message("getting cached inverse matrix data")
        x$getInverse()
    }
    # when no cached data, perform calculation to store inverse data to cache, and return value
    x$setInverse(solve(x$get()))
    x$getInverse()
}
