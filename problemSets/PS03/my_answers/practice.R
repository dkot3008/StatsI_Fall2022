library(tidyverse)
library(dplyr)
library(broom)
library(gapminder)
library(stargazer)

outcome dpeendant or Y is outcome
indepentdant is x explanatory


glimpse(incumbents_subset)



##question 1 part 1
mod1 <- lm(difflog ~ voteshare , data = incumbents_subset)

summary(mod1)


summary(inc.sub$difflog)
help(ggplot)

help("augment")
amod1 <- augment(mod1)

a <- 
  dat %>% 
  ggplot(aes(x=difflog,y=voteshare))+
    geom_point()
    
help("ggplot_add")
help("abline")

abline()
geom_line(data = amod1 )
    #geom_smooth(method = "lm")+
    #abline(a =lm(difflog~voteshare,data=inc.sub),col='red')

help("unlist")
  
help("geom_point",)

help(abline)
abline(lm(difflog~voteshare,data=inc.sub))

reg1 <- lm(difflog~voteshare,data=inc.sub) 
summary(reg1)

with(inc.sub,plot(difflog,voteshare))
abline(reg1)
    
    #geom_line(data = amod1,aes(y= .fitted ))
  

(lm(difflog ~ voteshare , data = incumbents_subset))
    #geom_smooth(method = "lm")
  #geom_smooth(df=mod1,method = "lm")

ggplot(aes())+
  geom_smooth(method= "loess")
residuals(mod1)

with(dat,plot(Quest_22_residuals,quest_1_residuals,
              main = "q2 and q1 residuals",col = "orange "))+
  abline(quest_2_residuals,quest_1_residuals,data = dat),col = "blue)"


##test
mod5a <- lm( difflog + presvote ~ voteshare  , data = dat)
splot5a <- 
  dat %>% 
  ggplot(aes(x=presvote + difflog,y=voteshare))+
  geom_point()
plot(splot5a)


splot1 <- 
  dat %>% 
  ggplot(aes(x=difflog,y=voteshare))+
  geom_point()+
  geom_smooth(method='lm', formula= y~x)

plot(splot1)

mod4 <- lm(mod1$residuals ~ mod2$residuals)

mod4 <- lm(resid1 ~ resid2)

splot4 <- 
  dat %>% 
  ggplot(aes(x=resid1,y= resid2))+
  geom_point()
plot(splot4)
#x=mod1$residuals,y= mod2$residuals