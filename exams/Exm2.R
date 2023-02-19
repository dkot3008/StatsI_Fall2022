##Q6 

#B
4.433 + (1.96*3.561)
4.433 - (1.96*3.561)
##range is -2.54656 to 11.41256
#Given how wide the range is,it is unlikely in my view that the education has 
#no effect on FDI so I disagree with the Author and would rejec tthe Null hypothesis
#educations is a non zero effect on FDI
#C
general -61.03 - (3*gdp) + (7.609*deomcracy) + (4.433*education)
l_ed <- -61.03 - (3*25491.1) + (7.609*0) + (4.433*11.06)
h_ed <- -61.03 - (3*25491.1) + (7.609*0) + (4.433*13.08)
h_ed - l_ed
#8.95466




##Q2
#C
#general model 1 <- -1.83+ (6.51*well depth) - (2.86*distance)
n_h <- -1.83+ (6.51*1) - (2.86*.42)
f_h <- -1.83+ (6.51*1) - (2.86*2.12)
n_h - f_h # = 4.862




##Q3
#gerneral for a ram <- -18.1386 + (2.2980*weight) - (4.0716*group.dummy.2=1)
Ram <- -18.1386 + (2.2980*weight) - (4.0716*1)

##B
#Ewe lamb general-18.1386 + (2.2980*weight)-(8.3622*group.dummy.1) - (4.0716*group.dummy.2)
-18.1386 + (2.2980*6)-(8.3622*0) - (4.0716*0)
#4.3506
#C
#Ewe 
ewe <- -18.1386 + (2.2980*0)-(8.3622*0) - (4.0716*0)
ewe
Ram <- -18.1386 + (2.2980*0)-(8.3622*0) - (4.0716*1)
Ram
wether <- -18.1386 + (2.2980*0)-(8.3622*1) - (4.0716*0)
wether
#ewe has the highest fat index for every weight