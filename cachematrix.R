makeMatrixVector <- function(x = mat()) {
  AAA<-NULL
  set<-function(y){
    x<<-y
    AAA<<-NULL
}
get<-function() x
setMMM<-function(solve) AAA<<- solve #calculating the inverse matrix with the help Solve
getMMM<-function() AAA
list(set=set, get=get,
   setMMM=setMMM,
   getMMM=getMMM)
}

cacheSolveVector <- function(x, ...) {
     AAA<-x$getMMM()
    if(!is.null(AAA)){
      message("getting cached inv matrix")
      return(AAA)
    }
    AAA<-x$get()
    AAA<-solve(AAA, ...)
    x$setMMM(AAA)
    AAA
}
##testing
x <-makeMatrixVector(matrix(c(2,6,19,8,8,7,3,2,4),ncol=3,nrow=3))
cacheSolveVector(x)
x$get()
x$getMMM()
