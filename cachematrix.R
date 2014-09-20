## This program is implemented by M.M. Mahbubul Syeed 
## email: rajit.cit@gmail.com

##Sample input for testing (just to assist the examinar :)
# mat <- matrix(1:4,2,2)
# x <- makeCacheMatrix(mat)
# cacheSolve(x)
# run the last command again to verify the caching


## This function receives a matrix for inversion and also cacheses the result.
## This function creates a special "vector", which is really a list containing a function to
#set and get the value of the given matrix for inversion.  and
#set and get the inversion matrix. 
makeCacheMatrix <- function(x = matrix()) {

  #used to cache the inverse matrix. initialized to null if no inversion available
  inverseMatrix <- NULL
  
  #set the given input matrix
  set <- function(y) {
    x <<- y
    inverseMatrix <<- NULL
  }
  
  #returns the input matrix
  get <- function() x
  
  # sets the inverse of the matrix.
  setInverseMatrix <- function(inverse) inverseMatrix <<- inverse

  #return the inverse matrix (returns null if not cashed before)
  getInverseMatrix <- function() inverseMatrix
  
  #creates the list containg the functions
  list(set = set, get = get, setInverseMatrix = setInverseMatrix, getInverseMatrix = getInverseMatrix)

}


## This funciton calculates the inverse of a matrix if not already cerated and cached by the
# the function ablove.
cacheSolve <- function(x, ...) {
    
  # gets the inverse matrix from cache
  # returns null if the inverse not alrealy cached
    chachedMatrix <- x$getInverseMatrix()
    
  #check if the inverse is available or not
  #if yes then return the result from cache
    if(!is.null(chachedMatrix)) {
      message("getting cached data")
      return(chachedMatrix)
    }
  #otherwise, calculate the inverse matrix and cache it
    data <- x$get()
    inverse <- solve(data)
    x$setInverseMatrix(inverse)
  
  #print the inverse matrix and return
    inverse
}
