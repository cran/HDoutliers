getHDmembers <-
function( data, maxrows = 10000, radius=NULL) 
{

 data <- as.matrix(data)

 n <- nrow(data)
 p <- ncol(data)
 
if (is.null(radius)) radius <- .1/(log(n)^(1/p))

# nearest-neighbor package
# require(FNN)
  
 members <- rep(list(NULL),n)

 if (n <= maxrows) {
   members <- as.list(1:n)
 }
 else {
   exemplars <- 1
   members[[1]] <- 1
   for (i in 2:n) {
      KNN <- get.knnx(data=data[exemplars,,drop=F],query=data[i,,drop=F],k=1)
      m <- KNN$nn.index[1,1]
      d <- KNN$nn.dist[1,1]
      if (d < radius) {
        l <- exemplars[m]
        members[[l]] <- c(members[[l]],i)
        next
      }
      exemplars <- c(exemplars,i)
      members[[i]] <- i
   }
 }

#names(members) <- 1:n
members <- members[!sapply( members, is.null)]
exemplars <- sapply( members, function(x) x[[1]])
names(members) <- exemplars

members
}
