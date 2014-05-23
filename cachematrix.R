## The makeCacheMatrix function creates a matrix object with 4 properties: set() ,sets a matrix to a value, 
##get() returns the value of the matrix, setInverse() and getInverse() , the inverse matrices the same way

## Creates a matrix object and returns its list of properties. 
##We create a cache matrix and call the cacheSolve function to compute its inverse which is cached.

makeCacheMatrix <- function(x = matrix()) {
    inverseMatrix<-NULL
    set<-function(inputMatrix)
    {    x<<-inputMatrix
         inverseMatrix<<-NULL
    }
    
    get<-function () return(x)
    setInverse<-function(inverse) inverseMatrix<<-inverse
    getInverse<-function() return(inverseMatrix)
    
    return(list(set=set,get=get,setInverse=setInverse,getInverse=getInverse))
    
}


## Calculates and caches the inverse of the matrix returned from makeCacheMatrix.  
## If the inverse matrix is calculated before (and the matrix has not changed), 
##cacheSolve retrieves the inverse from the cache and the 
## "Getting cached inverse" message is shown

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverseMatrix<-x$getInverse()
    if (!is.null(inverseMatrix))  ## if computed before returns the cached one
    {
        message ("Getting cached inverse ")
        return(inverseMatrix)
    }
    matrix<-x$get()
    inverse<-solve(matrix) ## calculate the matrix invesre here
    x$setInverse(inverse) 
    inverse
}
