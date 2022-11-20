library(tidyverse)
library(dplyr)
library(broom)
library(gapminder)
library(stargazer)

glimpse(incumbents_subset)
dat <- incumbents_subset
#####question 1 
###part 1-Run regression where 
###outcome variable is (y)voteshare and explanatory is (x)difflog
mod1 <- lm(difflog ~ voteshare , data = dat)

summary(mod1)

##part 2-scatterplot of the 2 variables and add the regression line
splot1 <- 
  dat %>% 
    ggplot(aes(x=difflog,y=voteshare))+
    geom_point()+
    geom_smooth(method='lm', formula= y~x)

plot(splot1)

####part 3 residuals
resid1 <- mod1$residuals

#### part 4- The prediction equaltion
Yi = difflog + voteshare

#####question 2
###part 1-Run regression where 
###outcome variable is (y)presvote and explanatory is (x)difflog
mod2 <- lm(difflog ~ presvote , data = dat)

summary(mod2)

##part 2-scatterplot of the 2 variables and add the regression line
splot2 <- 
  dat %>% 
  ggplot(aes(x=difflog,y=presvote))+
  geom_point()+
  geom_smooth(method='lm', formula= y~x)

plot(splot2)

####part 3 residuals
resid2 <- mod2$residuals

#### part 4- The prediction equaltion
Yi = Voteshare + difflog

#####question 3
###part 1-Run regression where 
###outcome variable is (y)voteshare and explanatory is (x)presvote
mod3 <- lm( presvote ~ voteshare , data = dat)

summary(mod3)

##part 2-scatterplot of the 2 variables and add the regression line
splot3 <- 
  dat %>% 
  ggplot(aes(x=presvote,y=voteshare))+
  geom_point()+
  geom_smooth(method='lm', formula= y~x)

plot(splot3)

###part 3 residuals
resid3 <- mod3$residuals

### part 4- The prediction equaltion
Yi = presvote + voteshare

#####question 4
mod4 <- lm(resid1 ~ resid2)
summary(mod4
        )
splot4 <- 
  dat %>% 
  ggplot(aes(x=resid1,y= resid2))+
  geom_point()+
  geom_smooth(method='lm', formula= y~x)
plot(splot4)
##q3 prediction equation
Yi = mod1$residuals + mod2$residuals

#####question 5
##Run a regression where the outcome variable is the (y)voteshare and explanatory 
## are (x)difflog and presvote
mod5 <- lm( difflog + presvote ~ voteshare , data = dat)
summary(mod5)

splot5 <- 
  dat %>% 
  ggplot(aes(x=presvote + difflog,y= voteshare))+
  geom_point()+
  geom_smooth(method='lm', formula= y~x)
plot(splot5)

##q2 predicion equation
Y = presvote/difflog + voteshare

##q3 what is it in this output that is identical to output in question 4,why?
#Same P value
