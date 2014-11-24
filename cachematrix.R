###########################################################################
# makeCacheMatrix - cache the inverse of a matrix
# 
###########################################################################

makeCacheMatrix <- function(x = matrix()) {
  m.inv <- NULL  #matrix.inverse set to NULL
  
  ###################################################
  # Name: set                                       #
  # Description: Set a new value for matrix         #
  ###################################################
  
  set <- function(y) 
  {
    x <<- y
    print("Setting m.inv to NULL")
    
    #Set to null so we can recalculate a new matrix inverse
    m.inv <<- NULL 
  }
  
  ###################################################
  # Name: get                                       #
  # Description: returns the matrix that was passed 
  # in and cached for the object         #
  ###################################################
  
  get <- function() 
  {
    x
  }
  
  ###############################################################
  # Name: setinverse                                            #
  # Description: set and cache the inverse of a square matrix   #
  ###############################################################
  
  setinverse <- function(inverse) 
  {
    
    print("Calling setinverse. Caching inverse.....")
    m.inv <<- inverse #Cache the latest value for the inverse
    
  }
  
  
  ###############################################################
  # Name: getinverse                                            #
  # Description: get the inverse of a square matrix         #
  ###############################################################
  
  getinverse <- function() 
  {
    m.inv
  }
  
  #Store all function objects in a list and return the list
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}



###############################################################
# Name: cacheSolve                                            #
# Description: Solve and cache matrix data                    #
###############################################################

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  minv <- x$getinverse()
  
  ##If there is a value in cache retrieve that value else solve for a new matrix value
  if(!is.null(minv)) 
  {
    message("getting cached inverse of matrix")
    return(minv)
  }
  
  #Getter method to retrieve the arguments that were set
  data <- x$get()
  
  #Run solve function on the matrix data
  minv <- solve(data)
  
  #Call setinverse on makeCacheMatrix object to cache the inverse
  x$setinverse(minv) # cache the value so as not to re-compute unless value changes
  
  #Return the inverse
  minv
}
