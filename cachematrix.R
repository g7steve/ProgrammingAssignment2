
#makeCacheMatrix creates a special "matrix", which is really a list containing a function to
#set the value of the matrix
#get the value of the matrix
#set the value of the inverse
#get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    # use <<- to assign value to parent environment
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) m <<- inverse
  getInverse <- function() m
  
  #return a list of function
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


#cacheSolve
#It takes the output of makeCacheMatrix as the input
#If the cache contains any value as the result of previous run it will return the previous computed value
#If not, it wil compute the reverse of the matrix and set the cache value
cacheSolve <- function(x, ...) {
  m<-x$getInverse()
  
  if(is.null(m))
  {
    m<-solve(x$get())
    x$setInverse(m)
    
  }
  
  #always return m as m can be result of the cache or it just get compute and set to the cache.
  return(m)
}

#sample call
#v1<-matrix((1:4),2)
#temp<-makeCacheMatrix(v1)
#cacheSolve(temp)
