data("mtcars")

lm_model_am <- lm(mpg ~ am, mtcars)
summary(lm_model_am)

#Residual Standard Error 4.902 means that the difference between
#actual mpg and predicted mpg on the model differ by about 4.902 percentage points
#Intercept 17.147 - if transmission not considered 17.147 mpg
#am coefficient 7.245 mpg. The difference between transmissions about 7.245 mpg
#R Squared 35.98%. About 35.98% of variability in mpg explained by transmission

lm_model_all <- lm(mpg ~ . , mtcars)
summary(lm_model_all)

#Intercept 12.30337 - if all variables at 0 begin with 12.30337 mpg
#am coefficient 2.52023. Difference between transmissions 2.52 mpg
#Residual Standard Error 2.65%. difference btwn actual and predicted 2.65 
#percentage points
# R Squared 86.9%. 86.9% of variability explained by model

lm_model_no_wt <- lm(mpg ~ .-wt, mtcars)
summary(lm_model_no_wt)
#Intercept 15.57062 - if all variables at 0 begin with 15.57062 mpg
#am coeff 2.90074. Difference between transmissions 2.90 mpg
#Residual Standard Error 2.82%. diff bwtn actual and predicted 2.82 percentage pts
#R Squared 84.5%. Variabiliy explained by model

t.test(mpg ~ am, mtcars)
# P. Value 0.001374. Reject the null that transmission has no effect on MPG

ggplot(mtcars, aes(x = factor(am), y = mpg)) +
  geom_boxplot() +
  labs(x = "Transmission (0 = Automatic, 1 = Manual)", 
       y = "MPG", title = "MPG by Transmission")

hist(mtcars$mpg, xlab = "MPG", main = "MPG Histogram", breaks = 30)


par(mfrow = c(1, 3))
plot(lm_model_am$residuals, pch = 1, xlab = "Index", ylab = "Residuals", 
     main = "Residuals Transmission Alone")

plot(lm_model_am$residuals, pch = 1, xlab = "Index", ylab = "Residuals", 
     main = "Residuals All Variables")

plot(lm_model_no_wt$residuals, pch = 1, xlab = "Index", ylab = "Residuals",
     main = "Residuals Minus Weight")





