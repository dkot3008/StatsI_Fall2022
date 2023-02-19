kmean_withinss <- function(k) {
  cluster <- kmeans(rescale_df, k)
  return (cluster$tot.withinss)
}
kmean_withinss(a)


a <- list(c(0,1,2,3,4),c(1,2,3,4,5),c(5,6,7,8,9))
a <- as.data.frame(a)
ab <- calcCentroid(a)
getCentroid(a)
euclidean(a[[1]],a[[2]])
getdistance()
help(kmeans)
w <- as.integer(a[[1]])
e <- as.integer(a[[2]])
f <- as.integer(a[[3]])



k2 <- kmeans(a, centers = 3, nstart = 25)
getdistance(e,f)
