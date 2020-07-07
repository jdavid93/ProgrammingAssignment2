# By definition, one can calculate the inverse of the matrix A; A' only if x 
# is a square matrix with determinant different than zero.
# I tried to come up with a code to prevent errors from popping up but i couldnt.
### if( ncol(x)==!nrow(x) || det(x)==0 ){
###    print("error")
### }

# As in the assignments' example function, this one uses setters and getters just in the same way.
# bases on this example, the makeCacheMatrix function exaclty the same as makeVector function.
# The only differene remains in the input type of both functions.
# Variables:

# x is the input matrix. 
# Inv variable  is the cached inverse matrix of the input matrix. just like m is the cached mean 
# value in the assignment's example.
# Inverse is  the argument for the set function of the makeCacheMatrix used by chacheSolve
# setInverse and getInverse are the correspondingly setters and getters for the makeCacheMatrix function.



makeCacheMatrix <- function(x = matrix()) {   #Set x's default input as matrix type
    Inv<-NULL
    set<-function(y){
        x<<-y
        Inv<<-NULL
    }
    get<-function() x
    setInverse<-function(Inverse) Inv<<-Inverse
    getInverse<-function() Inv
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}
# x here is not a matrix type input as in the last scenario, x is an special type of object
# as the output of the makeCacheMatrix function.
# this function works exactly in the same way as the one in the assignment example, only that the variable m 
# is renamed as Inv which stands for the inverse matrix which is about to be cached.

cacheSolve <- function(x, ...) { 
    Inv <- x$getInverse()
    if(!is.null(Inv)) {
        message("getting cached data")
        return(Inv)
    }
    data <- x$get()
    Inv <- solve(data, ...)
    x$setInverse(Inv)
    Inv
}
#final version
