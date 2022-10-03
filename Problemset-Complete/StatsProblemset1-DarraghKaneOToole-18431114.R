library(tinytex)
help("tinytex")

y <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 113,
       112, 98, 80, 97, 95, 111, 114, 89, 95, 126, 98)
M <- mean(y)
length(y)
s <- sd(y)
sq <- sqrt(25)
Upper_end_90 <- M + 1.68*(s/sq)
Lower_end_90 <- M - 1.68*(s/sq)
M
Lower_end_90
Upper_end_90



cmean <- 100


expenditure <- read.delim("~/Documents/GitHub/StatsI_Fall2022/datasets/expenditure.txt", header=FALSE)
>   View(expenditure)
Y <- expenditure$V2
X1 <- expenditure$V3
X2 <- expenditure$V4
X3 <- expenditure$V5
R <- expenditure$V6
as.factor(R)
plot(X1,Y)
#There is a positive corelation between X1 and Y
plot(X2,Y)
#There is a U shaped relationship-Negative corelation <300 < positve gretaer then
plot(X3,Y)
#positve relationship with extreme outliers at the hiigh end of X3

#plot the relationship between R,Y
plot(R,Y)
#mean per region
NE <- expenditure[2:10,1:6]
NC<- expenditure[11:22,1:6]
S <- expenditure[23:38,1:6]
W <- expenditure[39:51,1:6]

NNE <-as.numeric(NE$V2)
mean(NNE)
NNC <- as.numeric(NC$V2)
mean(NNC)
NS <- as.numeric(S$V2)
mean(NS)
NW<- as.numeric(W$V2)
mean(NW)
#Y and X1
library(tidyverse)

nx1 <- as.numeric(X1)
ny <- as.numeric(Y)
ggplot(expenditure, aes(nx1, ny,color=R,shape=R)) +
  geom_point()+
  scale_x_continuous(breaks=seq(0,3000,200))+
  scale_y_continuous(breaks = seq(0,140,20))





