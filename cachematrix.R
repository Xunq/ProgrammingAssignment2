## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) 
{
    m<-NULL
    
    # set: set x value as y, and initialize m to NULL
    set<-function(y)  
    {
        x<<-y
        m<<-NULL
    }
    
    # get: get x (the input data)
    get<-function() x
    
    # setinverse: set m value as inverse
    setinverse<-function(inverse) m<<-inverse
    
    # getinverse: get m (the inverse)
    getinverse<-function() m
    
    # return a list containing four functions
    list(set=set,
         get=get,
         setinverse=setinverse,
         getinverse=getinverse)
    
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then the inverse is retrieved from cache

cacheSolve <- function(x, ...) 
{
    # get the inverse of x to m
    m<-x$getinverse()
    
    # if already calculated, return the cached value
    if(!is.null(m))
    {
        message("getting cached data")
        return(m)
    }
    
    # get the value of x
    data<-x$get()
    
    # calculate the inverse of x
    m<-solve(data)
    
    # store the inverse of x in cache
    x$setinverse(m)
    
    m
    
}
