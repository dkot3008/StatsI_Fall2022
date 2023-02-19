p <- c(5,1,2,12,7)
q <- c(10,8,3,9,11)
###q1
a <- c(7,8)
getdistance <- function(a, b) sqrt(sum((a - b)^2))
getdistance(p,q)
###q2
lst <- list(c(0, 1, 2),c(2, 3, 4))
c <- list(c(0,1,2,3,4),c(5,6,7,8,9))
getCentroid <- function(pointList) {
  rowMeans(do.call("cbind",pointList))
}

getCentroid(c)

##q3
dfa <- data.frame(w,e,f)
##random points selected
dfx <- dfa[sample(NROW(dfa), 3,replace = TRUE),]
getdistance(dfx)
getCentroid(dfx)

kmeans(dfa,3,)
##q4
listings <- read.csv("~/Downloads/listings.csv", header=FALSE, comment.char="#")
listings <- read.csv("http://data.insideairbnb.com/ireland/leinster/dublin/2022-09-11/data/listings.csv.gz")
kmeans(listings$V31, 3)

long_lat <- data.frame(listings$V31,listings$V32)



lo_la <- na.omit(long_lat)
lo_la <-as.matrix(lo_la)
na.omit(lo_la)
kmeans(lo_la,3)


