dataTrans <-
function(data) 
{
 if (any(is.na(data))) stop("missing values not allowed")

# look for categorical variables
 if (is.null(dim(data))) {
   CAT <- !is.numeric(data) 
   data <- if (CAT) as.data.frame(data) else as.matrix(data)
 }
 else {
   if (inherits( data, "matrix")) data <- as.data.frame(data)
   CAT <- sapply( data, function(x) !is.numeric(x))
 }

 if (any(CAT)) {
# convert each categorical variable to numeric via 
# multiple correspondence analysis using MCA function from FactoMineR
#  require(FactoMineR)
   data[,CAT] <- sapply( data[,CAT,drop=F], 
                         function(x) MCA(as.matrix(x),ncp=1,graph=F)$ind$coord)
 }

unitize <- function(z) {
               zrange <- range(z)
               if (!(dif <- diff(zrange))) return(rep(0,length(z)))
               (z - zrange[1])/dif
                }

apply( as.matrix(data), 2, unitize)
}
