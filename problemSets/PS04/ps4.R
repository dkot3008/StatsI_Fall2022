install.packages("car")
library(car)
data(Prestige)
help(Prestige)

Prestige$type_prof <- rep(0,nrow(Prestige))
Prestige$type_bw <- rep(0,nrow(Prestige))

prof <- ifelse(Prestige$type == 'prof', 1, 0)
bc_wc <- ifelse(Prestige$type != 'prof' , 1, 0) 
#ifelse(Prestige$type == 'wc' , 1, 0)
#wc <- ifelse(Prestige$type == 'bc' , 1, 0)
print(bc_wc)
print(prof)

df_reg <- data.frame(prof = prof,
                   bc_wc = bc_wc)

(b)
pres_incprof <- lm(Prestige$prestige~df_reg$prof+ Prestige$income)
lm(Prestige$prestige~df_reg$prof+ Prestige$income)

(c)
# Y = prestige~ income + professional
#Prestige = 30.618334 + 22.757 x df_reg$prof + .001371 x Prestige$income  
(d)
##pretige ~ income
#The coeeffecient for income is very low meaning that as prestge goes 1 uo income goes on average .001371 
lm(Prestige$prestige~ Prestige$income)
plot(lm(Prestige$prestige~ Prestige$income))
summary(lm(Prestige$prestige~ Prestige$income))
(e)
##prestige ~ profession
#Going from a non professional to professional has an increase of 22.757 
lm(Prestige$prestige~df_reg$prof)
plot(lm(Prestige$prestige~df_reg$prof))
summary(lm(Prestige$prestige~df_reg$prof))

(f)
f = 30.618334 + 22.757*(1) + .001371*(1000)
print(f)
prestige increases by 54 with an 1000 euro increase.


help(ggplot)
library(tidyverse)
ggplot(data = Prestige,mapping = aes(x=prestige,y=income))+
  geom_smooth(method = 'lm')
(g)
#pro = 30.618334 + 22.757 x 0 + .001371*(6000) 
#pro = 30.618334 + 0 + 8.226
#38.844334

#not pro  = 30.618334 + 22.757 x 1 + .001371*(6000) 
#not pro  = 30.618334 + 22.757 + 8.226
#=61.601334

61.601334 - 38.844334
#=22.757


##question 2
n <- 30
(a)
t.test()
.016-.013/.042

(b)
t.test(.042,.016)
#lawn signs
.042 +(1.96*.016)
.042 -(1.96*.016)
#not lawn signs
.042 +(1.96*.013)
.042 -(1.96*.013)






(c)



(d)



  

