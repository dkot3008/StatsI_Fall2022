library(tidyverse)
library(stargazer)
library(broom)
library(dplyr)
library(scatterD3)
library(plotly)
library(car)
dat <- readRDS("data/train.rds")


mod1 <- lm(AdjSalePrice ~ SqFtTotLiving + SqFtLot + Bathrooms + Bedrooms + BldgGrade + PropertyType, data = dat)
dat <- cbind(dat, residuals = resid(mod1))

zip_group_res <- dat %>%
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

ggplot(dat, aes(SqFtTotLiving, AdjSalePrice , group = ZipGroup_r)) +
  geom_point(aes(colour = ZipGroup_r)) +
  geom_smooth(method = "lm", aes(colour = ZipGroup_r))

plot_ly(data = dat, z = ~AdjSalePrice, x = ~SqFtTotLiving, y = ~ZipGroup_r, 
        color = ~as.factor(ZipGroup_r))
help("plot_ly")


mod3 <- lm(AdjSalePrice ~ SqFtTotLiving, data = dat) # bivariate regression model

scatter.smooth(dat$SqFtTotLiving, resid(mod3), # plot a smooth line on the scatter plot
               lpars = list(col = "blue", lwd = 3, lty = 3), 
               main = "Residual Plot (Sale Price ~ Size)",
               xlab = "Total Living Area (sq.ft.)",
               ylab = "Residuals")
abline(h = 0, col = "red") # plot a horizontal line through zero

par(mfrow = c(1,1))
avPlot(mod2, variable = "SqFtTotLiving")

avPlot(mod2, variable = "BldgGrade")

terms <- predict(mod2, type = "terms") # extract the individual regression terms from our model for each observation

partial_resid <- resid(mod2) + terms # add the individual regression terms to the residual for each observation

df <- data.frame(SqFtTotLiving = dat[, "SqFtTotLiving"], # create a new data.frame of these vals
                 Terms = terms[, "SqFtTotLiving"],
                 PartialResid = partial_resid[, "SqFtTotLiving"])

ggplot(df, aes(SqFtTotLiving, PartialResid)) +
  geom_point(alpha = 0.2) +
  geom_smooth() +
  geom_line(aes(SqFtTotLiving, Terms), colour = "red")

mod4 <- lm(AdjSalePrice ~ SqFtTotLiving + I(SqFtTotLiving^2) + SqFtLot + Bathrooms + Bedrooms + BldgGrade + PropertyType + as.factor(ZipGroup_r), data = dat)

stargazer(mod1, mod2, mod4, type = "html")

terms_poly <- predict(mod4, type = "terms") # extract the individual regression terms from our model for each observation

partial_resid_poly <- resid(mod4) + terms_poly # add the individual regression terms to the residual for each observation

df_poly <- data.frame(SqFtTotLiving = dat[, "SqFtTotLiving"], # create a new data.frame of these vals
                      Terms = terms_poly[, "I(SqFtTotLiving^2)"],
                      PartialResid = partial_resid_poly[, "I(SqFtTotLiving^2)"])

ggplot(df_poly, aes(SqFtTotLiving, PartialResid)) +
  geom_point(alpha = 0.2) +
  geom_smooth() +
  geom_line(aes(SqFtTotLiving, Terms), colour = "red")

par(mfrow = c(2, 2)) # we change the graphic device to show 4 plots at once
plot(mod4) # we supply our lm object to plot()

sresid <- rstandard(mod4) # the rstandard() function extracts standardised residuals
index <- order(sresid) # make an index of standardised residuals
dat[index[1:5], c("AdjSalePrice", "SqFtTotLiving", "SqFtLot", "Bathrooms", "Bedrooms", "BldgGrade")]

