## The following pair of functions calculates the Inverse of a matrix
## and returns its value. As its apparent that finding the inverse of a 
## matrix is a time consuming process, its always a good idea to cache its
## value until the matrix is changed. Scoping rules of the R language has
## been exploited in doing this.

## The following function creates a special “matrix” object that can cache its inverse.
## It returns of list of functions viz. set(),get(),setInverse(),getInverse(). 

makeCacheMatrix <- function(x = matrix()) {
        inv_mat <- NULL				## Assign initial value NULL to the inverse matrix variable		
        set <- function(y) {                    ## Define the set function. 
                mat <<- y                       ## Use `<<-` to assign a value to an object in an environment
                inv_mat <<- NULL                ## other than current environment
        }
        get <- function() mat                   ## Define get function to get the matrix.
        setInverse <- function(inverse) inv_mat <<- inverse     ## Set the inverse of a matrix to inverse-matrix-variable
        getInverse <- function() inv_mat                        ## Define the getInverse function which returns the matrix inverse
        list(set = set,                                         ## Return the list containing all the function defined above.
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Following function 'cacheSolve' computes the inverse of the special "matrix" 
## created by 'makeCacheMatrix' above. If the inverse has already been calculated and the 
## matrix has not changed then Inverse is retrieved from the cache.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv_mat <- x$getInverse()                       ## Get the inverse of the matrix using getInverse function
        if (!is.null(inv_mat)) {                        ## check whether inverse has some value or it's NULL.
                message("getting cached data")          ## If the inverse is not NULL(i.e. inverse is already been calculated and cached) 
                return(inv_mat)                         ## then return inverse from the cache and exit from the function
        }
        mat <- x$get()                                  ## If inverse is a NULL then get the matrix using get() and 
        inv_mat <- solve(mat, ...)                      ## solve its inverse using 'solve' function.
        x$setInverse(inv_mat)                           ## Once inverse is found, set its value using 'setInverse' function.
        inv_mat                                         ## Finally return the Inverse of matrix
}
