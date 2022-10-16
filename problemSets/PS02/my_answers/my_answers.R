library(tidyverse)
library(broom)
library(dplyr)
uc <- c(14,6,7,)
lc <- c( 7,7,1)
## (observed-expected)^2/expected
chi <- chisq.test(lc,uc)
chi
ls(chi)
chi$residuals
t.test(lc,uc)

#expected value <- number of bribes/total population*sum of uc
ev <- (13/42*27)
#observed upper class bribes-expected/expected(1-expected prop)(1-uc prop)
t <- (6-ev)^2
b <- ev*(1-13/42)*(1-27/42)
t/b
print(t/b)
##WITH A CHI SUARED OF 2.696 AND degrees of freedom 4 the result is not signifigant at .1

t.test(uc,lc)
print(residuals(chi))
#standardised residuals describe the pattern of relationship between the cells
# The low results of the residuals mean the relationship between variables in the cell 
#are not indicating a strong relationship in any case although the residual for bribes 
#in uc is highest it alone is not strong enough evidence
library(readr)
women <- read_csv("https://raw.githubusercontent.com/kosukeimai/qss/master/PREDICTION/women.csv")
View(women)
#null- Female politicians are not more likely to support to support 
#policies female voters want so there will not be more water in areas
#with more female representtion
#alternative- Female politicians are more likely to support policies 
#female voters want so there will be more water in areas with more female representation
glimpse(women)

women %>%
  filter('village'== 1)%>%
  select('female',)%>%
  group_by('water')%>%
  summarise(n = c()) %>% # perform a summary operation (count the n per month)
  arrange(desc(c))


ggplot(women,aes(x=female,y=irrigation))+
  geom_point()

plot()




plot(lm(women$water~women$reserved))


lm(women$water~women$reserved)
