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
##WITH A CHI SUARED OF 2.696 AND degrees of freedom 4 the result is not signifigant at .1

t.test(uc,lc)
residuals(chi)
#standardised residuals indicate how much of the variation is explained by modeling
library(readr)
women <- read_csv("https://raw.githubusercontent.com/kosukeimai/qss/master/PREDICTION/women.csv")
View(women)
#null- Femal politicians are not more likely to support to support 
#policies female voters want
#alternative- Female politicians are more likely t support policies 
#female voters want
glimpse(women)

women%>%
  filter('female'== 1)%>%
  select('female','water')%>%
  group_by('water')
  summarise(n = n()) %>% # perform a summary operation (count the n per month)
  arrange(desc(n))


