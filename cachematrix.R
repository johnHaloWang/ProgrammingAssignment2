## two functions that cache the inverse of a matrix.


#`makeCacheMatrix`: This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        inverseM <- NULL
        set <- function(y){
                x <<- y
                inverseM <<- NULL
        }
        get <- function() x
        setInverseM <- function(inM) inverseM <<- inM
        getInverseM <- function() inverseM
        list(set = set, get = get, setInverseM = setInverseM, getInverseM = getInverseM)
        
}


#This function computes the inverse of the special 
#"matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed), 
#then cacheSolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverseM <- x$getInverseM()
        if(!is.null(inverseM)){
                message("getting cached data")
                return(inverseM)
        }else{
                data <- x$get()
                inverseM <-solve(data, ...)
                x$setInverseM(inverseM)
        }
        
        inverseM
}