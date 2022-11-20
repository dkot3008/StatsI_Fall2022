library(tidyverse)
library(dplyr)
library(broom)
library(gapminder)
library(stargazer)

glimpse(incumbents_subset)
dat <- incumbents_subset
#####question 1 
###part 1-Run regression where 
###outcome variable is voteshare and explanatory is difflog
mod1 <- lm(difflog ~ voteshare , data = dat)

summary(mod1)


##part 2-scatterplot of the 2 variables and add the regression line
splot1 <- 
  dat %>% 
    ggplot(aes(x=difflog,y=voteshare))+
    geom_point()



####part 3 residuals
resid1 <- mod1$residuals
as.double(resid1) 


#### part 4- The prediction equaltion
Yi = B0 + B1xi



#####question 2
###part 1-Run regression where 
###outcome variable is presvote and explanatory is difflog
mod2 <- lm(difflog ~ presvote , data = dat)

summary(mod2)


##part 2-scatterplot of the 2 variables and add the regression line
splot2 <- 
  dat %>% 
  ggplot(aes(x=difflog,y=presvote))+
  geom_point()



####part 3 residuals
resid2 <- mod2$residuals
as.double(resid2) 


#### part 4- The prediction equaltion
Yi = B0 + B1xi

#####question 3
###part 1-Run regression where 
###outcome variable is voteshare and explanatory is presvote
mod3 <- lm( presvote ~ voteshare , data = dat)

summary(mod3)


##part 2-scatterplot of the 2 variables and add the regression line
splot3 <- 
  dat %>% 
  ggplot(aes(x=presvote,y=voteshare))+
  geom_point()



####part 3 residuals
resid3 <- mod3$residuals
as.double(resid3) 


#### part 4- The prediction equaltion
Yi = B0 + B1xi


#####question 4
