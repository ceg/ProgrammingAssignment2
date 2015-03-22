#  This R file cotnains two functions "makeCacheMatrix()" and "cacheSolve()".
#    
#     The first of two the functions "makeCacheMatrix" returns a special "cached" matrix alongside with 6 functions
#     wrapped in a list. 
#     
#    The second function "cacheSolve()" takes the result of "makeCacheMatrix()" as an argument and computes the inverse
#    of a matrix if it is not computed already otherwise it returns then "cached" version of the inverse.
    




#  The function "makeCacheMatrix" defines 6 "inner" functions wihtin it.
#     The 6 functions are for manipulating the matrix and are as follows:
#     1) set
#     2) set_orginal
#     3) setinverse
#     4) get
#     5) getinverse
#     6) is_matrix_unchanged
#     The afroementioned functions are defined in "makeCacheMatrix()" but called in "cacheSolve()"
#     To determine wheter the matrix is changed "is_matrix_unchanged()" is called which compares the "x" argument of
#     "makeCacheMatrix()" with the variable "original_matrix" which is initially set to be equal to "x", but if x 
#     is changed than "original_matrix" and "x" will be different, which siganals that a new inverse matrix has to 
#     be caculate. The value of "original_matrix" has than to be set to equal "x" again,with the "set_origanl()" function, to
#     return the cached value of then inverse, as long as then matrix keeps unchanged.
makeCacheMatrix <- function(x = matrix()) {
    inverseMatrix <- NULL
    original_matrix <- NULL
  
    set <- function(y) {
        x <<- y
        original_matrix <<- y
        inverseMatrix <<- NULL
        
    }
    set_original <- function(y) original_matrix <<- y
    setinverse <- function(inverse) inverseMatrix <<- solve(x)
    get <- function() x
    getinverse <- function() inverseMatrix
    is_matrix_unchanged <- function() (x == original_matrix)
    
    list(set = set, get = get, setinverse = setinverse,
         getinverse = getinverse, set_original= set_original,
         is_matrix_unchanged = is_matrix_unchanged)
}


#  cacheSolve() takes the result of x <-"makeCacheMatrix()" as an argument and computes the inverse of the matrix 
#  within "x", if a cached  value is present, the cached value will be returned as long as the  matrix keeps unchanged
#  otherwise it will be computed again and cached.  
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverseMatrix <- x$getinverse()
        
        if(!is.null(inverseMatrix) && x$is_matrix_unchanged()){
            message("getting cached data")
            return(inverseMatrix)
        }
        data <- x$get()
        x$set_original(data)
        x$setinverse(data)
        inverseMatrix <- x$getinverse()
        inverseMatrix

}

    
    
    
#                       The following section  is just for testing purposes                 #   
matri <- rbind(c(1,2),c(3,4))

cacheMatrix <- makeCacheMatrix(matri)

# Test1 
#  First call of the cacheSolve-function with the 
#  cacheMatrix as argument whereby cacheMatrix <- makeCacheMatrix(matri)
# Expected output: 
#      [,1] [,2]
# [1,] -2.0  1.0
# [2,]  1.5 -0.5
print("Test1 first call of \"cacheSolve\" function")
print(cacheSolve(cacheMatrix))

# Test2 
#   Second call to the "cacheSolve" function with cacheMatrix, since this is the second call to the cacheSolve
#   function with the unchanged matrix in  cacheMatrix it is expected to  get the cached inverse Matrix as
#   return result
# Expected output: 
# "getting cached data"
#      [,1] [,2]
# [1,] -2.0  1.0
# [2,]  1.5 -0.5
print("Test2 second call to the \"cacheSolve\" function")
print(cacheSolve(cacheMatrix)) 

matri2 <- rbind(c(4,4),c(3,4))
cacheMatrix$set(matri2)
## Test3
#    Firs call to the "cacheSolve" function  with the a changed matrix (matri2) wihtin cacheMatrix
#    because it is the first call with the changed matrix it it expected to be computed new
# Expected output:
#       [,1] [,2]
# [1,]  1.00   -1
# [2,] -0.75    1
print("Test3")
print(cacheSolve(cacheMatrix)) 


##Test4
#   Second call to the "cacheSolve" function with cacheMatrix, with the changed matrix (matri2) since this
#   is the second call to the cacheSolve function with the unchanged matrix in  cacheMatrix it is 
#   expected to  get the cached inverse Matrix as return result
#Expected output:
# getting cached data
# [,1] [,2]
# [1,]  1.00   -1
# [2,] -0.75    1
print("Test4")
print(cacheSolve(cacheMatrix)) 