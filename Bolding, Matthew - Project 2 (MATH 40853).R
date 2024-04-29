# Matthew Bolding
# Project 2 Code

## Data Preprocessing

library(dplyr)
library(olsrr)

setwd("~/Library/CloudStorage/OneDrive-TexasChristianUniversity/Senior/Spring Semester/MATH 40853 Regression & Time Series/R Code Examples")
housing_data <- read.csv("housing_data.csv")

housing_data$TotalSF <- housing_data$Flr1SF + housing_data$Flr2SF
housing_data$Multistory <- as.factor(ifelse(housing_data$Flr2SF == 0, FALSE, TRUE))
housing_data$CentralAir <- as.factor(ifelse(housing_data$CentralAir == "Y", TRUE, FALSE))

table(housing_data$OverallQual)
housing_data$OverallQual <- ifelse(housing_data$OverallQual %in% c(1, 2, 3), 4, housing_data$OverallQual)
housing_data$OverallQual <- ifelse(housing_data$OverallQual %in% c(10), 9, housing_data$OverallQual)

table(housing_data$OverallCond)
housing_data$OverallCond <- ifelse(housing_data$OverallCond %in% c(1, 2, 3), 4, housing_data$OverallCond)
housing_data$OverallCond <- ifelse(housing_data$OverallCond %in% c(9), 8, housing_data$OverallCond)

housing_data$OverallQual <- as.factor(housing_data$OverallQual)
housing_data$OverallCond <- as.factor(housing_data$OverallCond)

### Visualizations

boxplot(log(housing_data$SalePrice) ~ housing_data$OverallQual,
        main = "Overall Quality vs. Log Sale Price",
        ylab = "Log Sale Price",
        xlab = "Overall Quality")
boxplot(log(housing_data$SalePrice) ~ housing_data$OverallCond,
        main = "Overall Condition vs. Log Sale Price",
        ylab = "Log Sale Price",
        xlab = "Overall Condition")
boxplot(log(housing_data$SalePrice) ~ housing_data$Fireplaces,
        main = "Number of Fireplaces vs. Log Sale Price",
        ylab = "Log Sale Price",
        xlab = "Fireplaces")
boxplot(log(housing_data$SalePrice) ~ housing_data$YrSold,
        main = "Year Sold vs. Log Sale Price",
        ylab = "Log Sale Price",
        xlab = "Year Sold")
boxplot(log(housing_data$SalePrice) ~ housing_data$CentralAir,
        main = "Central Air Presence vs. Log Sale Price",
        ylab = "Log Sale Price",
        xlab = "Central Air Present")
boxplot(log(housing_data$SalePrice) ~ housing_data$Multistory,
        main = "Multistory vs. Log Sale Price",
        ylab = "Log Sale Price",
        xlab = "Multistory")

### Scatter Plots
## Lot Area
cor((housing_data$LotArea), (housing_data$SalePrice))
cor(log(housing_data$LotArea), (housing_data$SalePrice))
cor((housing_data$LotArea), log(housing_data$SalePrice))
cor(log(housing_data$LotArea), log(housing_data$SalePrice)) # Maximum

plot(log(housing_data$LotArea), log(housing_data$SalePrice),
     main = "Log Log Area vs. Log Sale Price",
     ylab = "Log Sale Price",
     xlab = "Log Lot Area")

## Year Built
cor((housing_data$YearBuilt), (housing_data$SalePrice))
cor(log(housing_data$YearBuilt), (housing_data$SalePrice))
cor((housing_data$YearBuilt), log(housing_data$SalePrice)) # Maximum
cor(log(housing_data$YearBuilt), log(housing_data$SalePrice))

plot(housing_data$YearBuilt, log(housing_data$SalePrice),
     main = "Year Built vs. Log Sale Price",
     ylab = "Log Sale Price",
     xlab = "Year Built")

## Garage Area
cor((housing_data$GarageArea), (housing_data$SalePrice))
cor(log(housing_data$GarageArea), (housing_data$SalePrice))
cor((housing_data$GarageArea), log(housing_data$SalePrice)) # Maximum
cor(log(housing_data$GarageArea), log(housing_data$SalePrice))

plot(housing_data$GarageArea, log(housing_data$SalePrice),
     main = "Garage Area vs. Log Sale Price",
     ylab = "Log Sale Price",
     xlab = "Garage Area")

## Flr1SF
cor((housing_data$Flr1SF), (housing_data$SalePrice))
cor(log(housing_data$Flr1SF), (housing_data$SalePrice))
cor((housing_data$Flr1SF), log(housing_data$SalePrice))
cor(log(housing_data$Flr1SF), log(housing_data$SalePrice)) # Maximum

plot(log(housing_data$Flr1SF), log(housing_data$SalePrice),
     main = "Log First Floor Square Footage vs. Log Sale Price",
     ylab = "Log Sale Price",
     xlab = "Log First Floor Square Footage")

## Flr2SF
cor((housing_data$Flr2SF), (housing_data$SalePrice)) # Maximum
cor(log(housing_data$Flr2SF), (housing_data$SalePrice))
cor((housing_data$Flr2SF), log(housing_data$SalePrice))
cor(log(housing_data$Flr2SF), log(housing_data$SalePrice))

plot((housing_data$Flr2SF), log(housing_data$SalePrice),
     main = "Second Floor Square Footage vs. Log Sale Price",
     ylab = "Log Sale Price",
     xlab = "Second Floor Square Footage")

## TotalSF
cor((housing_data$TotalSF), (housing_data$SalePrice))
cor(log(housing_data$TotalSF), (housing_data$SalePrice))
cor((housing_data$TotalSF), log(housing_data$SalePrice))
cor(log(housing_data$TotalSF), log(housing_data$SalePrice)) # Maximum

plot(log(housing_data$TotalSF), log(housing_data$SalePrice),
     main = "Log Total Square Footage vs. Log Sale Price",
     ylab = "Log Sale Price",
     xlab = "Log Total Square Footage")


cor(log(housing_data$TotalSF), log(housing_data$SalePrice))
full_model_boo <- lm(log(SalePrice) ~ log(LotArea) + OverallQual + OverallCond +
                   YearBuilt + YrSold + CentralAir + Fireplaces + GarageArea + Multistory, 
                 data = housing_data)

ols_step_best_subset(full_model_boo, metric = "adjr")

#### Best Subset
library(olsrr)
full_model <- lm(log(SalePrice) ~ log(LotArea) + OverallQual + OverallCond +
                 YearBuilt + YrSold + CentralAir + Fireplaces + GarageArea + 
                 log(Flr1SF) + Flr2SF + log(TotalSF) + Multistory + log(TotalSF)*Multistory, 
                 data = housing_data)

ols_step_best_subset(full_model, metric = "adjr")

best_subset_model <- lm(log(SalePrice) ~ log(LotArea) + OverallQual + OverallCond + YearBuilt + 
                          Fireplaces + GarageArea + log(TotalSF)*Multistory - Multistory - log(TotalSF), data = housing_data)
summary(best_subset_model)

extractAIC(best_subset_model)
create_plots(best_subset_model, "BSS", housing_data)

### Forward Selection

fws_base <- lm(log(SalePrice) ~ 1, data = housing_data)
fws_full <- lm(log(SalePrice) ~ log(LotArea) + OverallQual + OverallCond +
                 YearBuilt + YrSold + CentralAir + Fireplaces + GarageArea + 
                 log(Flr1SF) + Flr2SF + log(TotalSF) + Multistory + log(TotalSF)*Multistory, 
               data = housing_data)

fws_model <- step(fws_base, direction='forward', scope=formula(fws_full), trace = 0, steps = 7)
summary(fws_model)


#### Backwards Selection

bws_model_1_0 <- lm(log(SalePrice) ~ log(LotArea) + OverallQual + OverallCond +
                      YearBuilt + YrSold + CentralAir + Fireplaces + GarageArea + Multistory, 
                    data = housing_data)
bws_model_1_0_adjr2 <- summary(bws_model_0)$adj.r.squared

bws_model_1_1 <- lm(log(SalePrice) ~ OverallQual + OverallCond +
                      YearBuilt + YrSold + CentralAir + Fireplaces + GarageArea + Multistory, 
                    data = housing_data)
bws_model_1_1_adjr2_diff <- bws_model_1_0_adjr2 - summary(bws_model_1_1)$adj.r.squared
  
bws_model_1_2 <- lm(log(SalePrice) ~ log(LotArea) + OverallCond +
                      YearBuilt + YrSold + CentralAir + Fireplaces + GarageArea + Multistory, 
                    data = housing_data)
bws_model_1_2_adjr2_diff <- bws_model_1_0_adjr2 - summary(bws_model_1_2)$adj.r.squared

bws_model_1_3 <- lm(log(SalePrice) ~ log(LotArea) + OverallQual +
                      YearBuilt + YrSold + CentralAir + Fireplaces + GarageArea + Multistory, 
                    data = housing_data)
bws_model_1_3_adjr2_diff <- bws_model_1_0_adjr2 - summary(bws_model_1_3)$adj.r.squared

bws_model_1_4 <- lm(log(SalePrice) ~ log(LotArea) + OverallQual + OverallCond +
                      YrSold + CentralAir + Fireplaces + GarageArea + Multistory, 
                    data = housing_data)
bws_model_1_4_adjr2_diff <- bws_model_1_0_adjr2 - summary(bws_model_1_4)$adj.r.squared

bws_model_1_5 <- lm(log(SalePrice) ~ log(LotArea) + OverallQual + OverallCond +
                      YearBuilt + CentralAir + Fireplaces + GarageArea + Multistory, 
                    data = housing_data)
bws_model_1_5_adjr2_diff <- bws_model_1_0_adjr2 - summary(bws_model_1_5)$adj.r.squared # Minimum, removing YrSold

bws_model_1_6 <- lm(log(SalePrice) ~ log(LotArea) + OverallQual + OverallCond +
                      YearBuilt + YrSold + Fireplaces + GarageArea + Multistory, 
                    data = housing_data)
bws_model_1_6_adjr2_diff <- bws_model_1_0_adjr2 - summary(bws_model_1_6)$adj.r.squared

bws_model_1_7 <- lm(log(SalePrice) ~ log(LotArea) + OverallQual + OverallCond +
                      YearBuilt + YrSold + CentralAir + GarageArea + Multistory, 
                    data = housing_data)
bws_model_1_7_adjr2_diff <- bws_model_1_0_adjr2 - summary(bws_model_1_7)$adj.r.squared

bws_model_1_8 <- lm(log(SalePrice) ~ log(LotArea) + OverallQual + OverallCond +
                      YearBuilt + YrSold + CentralAir + Fireplaces + Multistory, 
                    data = housing_data)
bws_model_1_8_adjr2_diff <- bws_model_1_0_adjr2 - summary(bws_model_1_8)$adj.r.squared

bws_model_1_9 <- lm(log(SalePrice) ~ log(LotArea) + OverallQual + OverallCond +
                      YearBuilt + YrSold + CentralAir + Fireplaces + GarageArea, 
                    data = housing_data)
bws_model_1_9_adjr2_diff <- bws_model_1_0_adjr2 - summary(bws_model_1_9)$adj.r.squared

bws_model_1_1_adjr2_diff
bws_model_1_2_adjr2_diff
bws_model_1_3_adjr2_diff
bws_model_1_4_adjr2_diff
bws_model_1_5_adjr2_diff
bws_model_1_6_adjr2_diff
bws_model_1_7_adjr2_diff
bws_model_1_8_adjr2_diff
bws_model_1_9_adjr2_diff

bws_model_2_0_adjr2 <- summary(bws_model_1_5)$adj.r.squared

bws_model_2_1 <- lm(log(SalePrice) ~ OverallQual + OverallCond +
                      YearBuilt + CentralAir + Fireplaces + GarageArea + Multistory, 
                    data = housing_data)
bws_model_2_1_adjr2_diff <- bws_model_2_0_adjr2 - summary(bws_model_2_1)$adj.r.squared

bws_model_2_2 <- lm(log(SalePrice) ~ log(LotArea) + OverallCond +
                      YearBuilt + CentralAir + Fireplaces + GarageArea + Multistory, 
                    data = housing_data)
bws_model_2_2_adjr2_diff <- bws_model_2_0_adjr2 - summary(bws_model_2_2)$adj.r.squared

bws_model_2_3 <- lm(log(SalePrice) ~ log(LotArea) + OverallQual +
                      YearBuilt + CentralAir + Fireplaces + GarageArea + Multistory, 
                    data = housing_data)
bws_model_2_3_adjr2_diff <- bws_model_2_0_adjr2 - summary(bws_model_2_3)$adj.r.squared

bws_model_2_4 <- lm(log(SalePrice) ~ log(LotArea) + OverallQual + OverallCond +
                      CentralAir + Fireplaces + GarageArea + Multistory, 
                    data = housing_data)
bws_model_2_4_adjr2_diff <- bws_model_2_0_adjr2 - summary(bws_model_2_4)$adj.r.squared

bws_model_2_5 <- lm(log(SalePrice) ~ log(LotArea) + OverallQual + OverallCond +
                      YearBuilt + Fireplaces + GarageArea + Multistory, 
                    data = housing_data)
bws_model_2_5_adjr2_diff <- bws_model_2_0_adjr2 - summary(bws_model_2_5)$adj.r.squared # Minimum, removing CentralAir

bws_model_2_6 <- lm(log(SalePrice) ~ log(LotArea) + OverallQual + OverallCond +
                      YearBuilt + CentralAir + GarageArea + Multistory, 
                    data = housing_data)
bws_model_2_6_adjr2_diff <- bws_model_2_0_adjr2 - summary(bws_model_2_6)$adj.r.squared

bws_model_2_7 <- lm(log(SalePrice) ~ log(LotArea) + OverallQual + OverallCond +
                      YearBuilt + CentralAir + Fireplaces + Multistory, 
                    data = housing_data)
bws_model_2_7_adjr2_diff <- bws_model_2_0_adjr2 - summary(bws_model_2_7)$adj.r.squared

bws_model_2_8 <- lm(log(SalePrice) ~ log(LotArea) + OverallQual + OverallCond +
                      YearBuilt + CentralAir + Fireplaces + GarageArea, 
                    data = housing_data)
bws_model_2_8_adjr2_diff <- bws_model_2_0_adjr2 - summary(bws_model_2_8)$adj.r.squared

bws_model_2_1_adjr2_diff
bws_model_2_2_adjr2_diff
bws_model_2_3_adjr2_diff
bws_model_2_4_adjr2_diff
bws_model_2_5_adjr2_diff
bws_model_2_6_adjr2_diff
bws_model_2_7_adjr2_diff
bws_model_2_8_adjr2_diff

bws_model_3_0_adjr2 <- summary(bws_model_2_5)$adj.r.squared

bws_model_3_1 <- lm(log(SalePrice) ~ OverallQual + OverallCond +
                      YearBuilt + Fireplaces + GarageArea + Multistory, 
                    data = housing_data)
bws_model_3_1_adjr2_diff <- bws_model_3_0_adjr2 - summary(bws_model_3_1)$adj.r.squared

bws_model_3_2 <- lm(log(SalePrice) ~ log(LotArea) + OverallCond +
                      YearBuilt + Fireplaces + GarageArea + Multistory, 
                    data = housing_data)
bws_model_3_2_adjr2_diff <- bws_model_3_1_adjr2 - summary(bws_model_3_2)$adj.r.squared

bws_model_3_3 <- lm(log(SalePrice) ~ log(LotArea) + OverallQual +
                      YearBuilt + Fireplaces + GarageArea + Multistory, 
                    data = housing_data)
bws_model_3_3_adjr2_diff <- bws_model_3_2_adjr2 - summary(bws_model_3_3)$adj.r.squared

bws_model_3_4 <- lm(log(SalePrice) ~ log(LotArea) + OverallQual + OverallCond +
                      Fireplaces + GarageArea + Multistory, 
                    data = housing_data)
bws_model_3_4_adjr2_diff <- bws_model_3_0_adjr2 - summary(bws_model_3_4)$adj.r.squared

bws_model_3_5 <- lm(log(SalePrice) ~ log(LotArea) + OverallQual + OverallCond +
                      YearBuilt + GarageArea + Multistory, 
                    data = housing_data)
bws_model_3_5_adjr2_diff <- bws_model_3_0_adjr2 - summary(bws_model_3_5)$adj.r.squared

bws_model_3_6 <- lm(log(SalePrice) ~ log(LotArea) + OverallQual + OverallCond +
                      YearBuilt + Fireplaces + Multistory, 
                    data = housing_data)
bws_model_3_6_adjr2_diff <- bws_model_3_0_adjr2 - summary(bws_model_3_6)$adj.r.squared

bws_model_3_7 <- lm(log(SalePrice) ~ log(LotArea) + OverallQual + OverallCond +
                      YearBuilt + Fireplaces + GarageArea, 
                    data = housing_data)
bws_model_3_7_adjr2_diff <- bws_model_3_0_adjr2 - summary(bws_model_3_7)$adj.r.squared

bws_model_3_1_adjr2_diff
bws_model_3_2_adjr2_diff
bws_model_3_3_adjr2_diff
bws_model_3_4_adjr2_diff
bws_model_3_5_adjr2_diff
bws_model_3_6_adjr2_diff
bws_model_3_7_adjr2_diff


ninty_fifth_percentile <- function(model, data) {
  residuals <- model$residuals
  standardizedResiduals <- residuals/summary(model)$sigma
  obs_above_3 <- sum(abs(standardizedResiduals) > 3)
  return((obs_above_3/nrow(data)) * 100)
}

ninty_fifth_percentile(fws_model, housing_data)

residuals <- fws_model$residuals
standardizedResiduals <- residuals/summary(fws_model)$sigma
sum(abs(standardizedResiduals) > 3)/nrow(housing_data)


create_plots <- function(model, method, data) {
  # Calculate the Standardized Residuals
  residuals <- model$residuals
  standardizedResiduals <- residuals/summary(model)$sigma
  
  # QQ Plot
  qqnorm(standardizedResiduals,
         main = paste(method, "Normal Q-Q Plot"))
  qqline(standardizedResiduals)
  
  # Plot Log Sale Price vs. Standardized Residuals
  plot(log(data$SalePrice), standardizedResiduals,
       xlab = "Sale Price (Log Scaled)",
       ylab = "Standardized Residuals",
       main =  paste(method, "Sale Price (Log Scaled) vs. Standardized Residuals"))
  
  abline(a = -2, b = 0, lty = 2, col = "orange")
  abline(a = 2, b = 0, lty = 2, col = "orange")
  abline(a = -3, b = 0, col = "red")
  abline(a = 3, b = 0, col = "red")
  
  # Plot Index vs. Standardized Residuals
  plot(standardizedResiduals,
       ylab = "Standardized Residuals",
       main = paste(method, "Index vs. Standardized Residuals"))
}
