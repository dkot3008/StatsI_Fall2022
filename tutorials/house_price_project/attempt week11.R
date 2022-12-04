#####
boxplot(dat$AdjSalePrice)$out
# install the package 
install.packages("ggstatsplot")

# Load the package
library(ggstatsplot)

#Create a boxplot that labels the outliers  
Q <- quantile(dat$AdjSalePrice, probs=c(.0, .99), na.rm = FALSE)
iqr <- IQR(dat$AdjSalePrice)
up <-  Q[2]+1.5*iqr # Upper Range  
low<- Q[1]-1.5*iqr # Lower Range
eliminated<- subset(dat, dat$AdjSalePrice > (Q[1] - 1.5*iqr) & dat$AdjSalePrice < (Q[2]+1.5*iqr))


help("ggbetweenstats")

####plot.ly
ggplot(train, aes(SqFtTotLiving, AdjSalePrice , group = ZipGroup_r)) +
  geom_point(aes(colour = ZipGroup_r)) +
  geom_smooth(method = "lm", aes(colour = ZipGroup_r))

plot_ly(data = train, z = ~AdjSalePrice, x = ~SqFtTotLiving, y = ~ZipGroup_r, 
        color = ~as.factor(ZipGroup_r))

help("facet_wrap")

######adapted


mod3x <- lm(AdjSalePrice ~ SqFtTotLiving, data = eliminated) # bivariate regression model

scatter.smooth(eliminated$SqFtTotLiving, resid(mod3x), # plot a smooth line on the scatter plot
               lpars = list(col = "blue", lwd = 3, lty = 3), 
               main = "Residual Plot (Sale Price ~ Size)",
               xlab = "Total Living Area (sq.ft.)",
               ylab = "Residuals")
abline(h = 0, col = "red") # plot a horizontal line through zero
###
dat <- readRDS("data/train.rds")

mod1 <- lm(AdjSalePrice ~ SqFtTotLiving + SqFtLot + Bathrooms + Bedrooms + BldgGrade + PropertyType, data = dat)
dat <- cbind(dat, residuals = resid(mod1))

zip_group_r <- dat %>%
  group_by(ZipCode) %>%
  summarise(med_price = median(residuals),
            count = n()) %>%
  arrange(med_price) %>%
  mutate(cumul_count = cumsum(count),
         ZipGroup_r = ntile(cumul_count, 5))
dat <- dat %>%
  left_join(select(zip_group_res, ZipCode, ZipGroup_r), by = "ZipCode")

mod2 <- lm(AdjSalePrice ~ SqFtTotLiving + SqFtLot + Bathrooms + Bedrooms + BldgGrade + PropertyType + as.factor(ZipGroup_r), data = dat)

stargazer(mod1, mod2, type = "html")


ggplot(train, aes(SqFtTotLiving, AdjSalePrice, group = ZipGroup)) +
  geom_point(aes(colour = ZipGroup)) +
  geom_smooth(method = "lm", aes(colour = ZipGroup))

plot_ly(data = train, z = ~AdjSalePrice, x = ~SqFtTotLiving, y = ~ZipGroup, 
        color = ~as.factor(ZipGroup))
