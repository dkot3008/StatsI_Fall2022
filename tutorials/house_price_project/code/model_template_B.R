##############################
# Final model script: Team B #
##############################

### Note: fill in the code below until the line.
### Make sure your model works as expected by running summary().

# Load any packages here
# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# load necessary packages
lapply(c("ggplot2", "stargazer", "tidyverse", "stringr", "broom", ),  pkgTest)
library(plotly)
# Load training data
train <- readRDS("data/train.rds")

# Data transformation

#exclude outliers
outliers <- c(15204, 8893, 12591, 4465)

train <-train[-match(outliers,train$ID),]

#add zipgroup
train %>%
  group_by(ZipCode) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  ggplot(aes(as.factor(reorder(ZipCode, n)), n)) +
  geom_col() +
  coord_flip() +
  xlab("Zip Code")

zip_group <- train %>%
  group_by(ZipCode) %>%
  summarise(med_price = median(AdjSalePrice),
            count = n()) %>%
  arrange(med_price) %>%
  mutate(cumul_count = cumsum(count),
         ZipGroup = ntile(cumul_count, 5))

train <- train %>%
  left_join(select(zip_group, ZipCode, ZipGroup), by = "ZipCode")


# Model
mod <- lm(AdjSalePrice ~ 
              SqFtTotLiving + I(SqFtTotLiving^2) + 
              BldgGrade + 
              ZipGroup + I(ZipGroup^2), 
            data = train, 
    na.action = na.omit)

stargazer(mod, type = "text")
  
par(mfrow = c(2, 2)) # we change the graphic device to show 4 plots at once
plot(mod) # we supply our lm object to plot()
  
  
summary(mod)

##Darraghs 2d model
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


########## do not fill in below the line ###########

# Load test data
test <- readRDS("data/test.rds")

# Transform test data
zip_group <- test %>%
  group_by(ZipCode) %>%
  summarise(med_price = median(AdjSalePrice),
            count = n()) %>%
  arrange(med_price) %>%
  mutate(cumul_count = cumsum(count),
         ZipGroup = ntile(cumul_count, 5))

test <- test %>%
  left_join(select(zip_group, ZipCode, ZipGroup), by = "ZipCode")
  
# Run model on test data
test$prediction <- predict(mod, newdata = test)

# Calculate RMSE
test$residuals <- test$AdjSalePrice - test$prediction
(rmse <- sqrt(mean(test$residuals^2)))

# Calculate R^2
mean_y <- mean(test$AdjSalePrice)
tss <- sum((test$AdjSalePrice - mean_y)^2)
rss <- sum((test$AdjSalePrice - test$prediction)^2)
(r_sq <- 1 - (rss/tss))

