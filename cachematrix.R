## there are 2 functions created in this file – makeCacheMatrix and cacheSolve
## makeCacheMatrix will create a square matrix with size based on 
## number x (nrow and ncol)
## cacheSolve will check to see if the matrix and inverse matrix are cached.  
## If they are cached and the same size that was requested,the cached values
## will be returned, otherwise it will create those objects



## makeCacheMatrix will create a matrix based on the size
## of the value sent to the function.  X = nrow and ncol
makeCacheMatrix <- function(x = matrix()) {
        y <- x%*%x
        m <- matrix(sample(1:10, y, replace=T), nrow=x, ncol= x)
}




## cacheSolve will check to see if the matrix and inverse already
## exist.  if they exist and are the same size requested,  
## then the saved values will be returned.
## if the matrix and inverse do not exist, the makeCacheMatrix is
## called to create the matrix and then inverse is calculated
## if value not passed to function, the matrix will default to 2 rows/cols
cacheSolve <- function(r = 2, ...) { 
        
        ## initialize objects
        mtrx <- NULL
        im   <- NULL
        
        ##  get cached values - catch error if values don't exist                          
        try(mtrx <- get("m"), silent=T)
        try(im <- get("invmtrx"), silent=T)
        
        
        ## check to see if stored matrix match size passed to function 
        if (is.matrix(mtrx) && 
                    (nrow(mtrx) == r) && (ncol(mtrx) == r) &&
                    (nrow(im) == r) && (ncol(im) == r)) 
        {
                message("returning cached data")
                return(im)
        } else {message("create matrix")}
        
        
        ## if you are here –then the matrix and inverse were not cached 
        ## or were a different size and must be created
        
        ## create matrix m using  makeCacheMatrix
        m <<- makeCacheMatrix(r)
        
        ## create inverse matrix of x using solve
        invmtrx <<- solve(m) 
        return(invmtrx)
        
}


