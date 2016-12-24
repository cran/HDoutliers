getHDoutliers <-
function (data, memberLists, alpha = 0.05) 
{
    exemplars <- sapply(memberLists, function(x) x[[1]])
    data <- as.matrix(data)
    d <- knn.dist(data[exemplars, ], k = 1)
    n <- length(d)
    ord <- order(d)
    dmin <- min(d)
    dmax <- max(d)
    gaps <- c(0, diff(d[ord]))
    n4 <- max(min(50, floor(n/4)), 2)
    J <- 1:n4
    start <- max(floor(n/2), 1) + 1
    ghat <- numeric(n)
    for (i in start:n) ghat[i] <- sum((J/n4) * gaps[i - J + 1])
    logAlpha <- log(1/alpha)
    use <- start:n
    bound <- Inf
    for (i in start:n) {
        if (gaps[i] > logAlpha * ghat[i]) {
            bound <- d[ord][i - 1]
            break
        }
    }
    ex <- exemplars[which(d > bound)]
    mem1 <- sapply(memberLists, function(x) x[1])
    out <- unlist(memberLists[match(ex, mem1)])
    names(out) <- NULL
    out
}
