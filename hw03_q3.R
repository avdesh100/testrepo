# setup
  set.seed(1)
  n <- 100
  par(mfrow = c(1,3)) # for plots
# a)
  x <- rnorm(n, mean = 0, sd = 1)
  
# b)
  eps <- rnorm(n, mean= 0, sd = sqrt(.25))

# c)
  y <- -1 + 0.5*x + eps
  # length of y = 100; beta0 = -1; beta1 = 0.5
  
# d)
  plot(x,y)
  # x and y are clearly positively correlated
  # more variation among x's near 0 than among larger/smaller x's
  # (to be expected since we have more x's near 0 b/c of their normal dist)
  
# e)
  model1 <- lm(y ~ x)
  summary(model1)
  # beta0_hat = -1.019
  # beta1_hat = 0.499
  # both coefficients are statistically significant with small standard errors
  
# f)
  abline(model1, col = "red")
  legend(x = "bottomrigh", legend = c("data", "regression\nline"),
         pch = c(1, NA), lty = c(NA, 1), col = c("black", "red"))
  
# g)
  model2 <- lm(y ~ x + I(x^2)) # can't just use x^2, will use wrong model
  summary(model2)
  # coefficient on x^2 is close to 0 (-0.059) with a large standard error (0.04);
  # not statistically significantly different from 0
  # ==> no evidence that x^2 term improves model fit
  # can also check with anova:
  anova(model1, model2)
  # here the p-value is large, so we do not reject the null
  # hypothesis that model 1 is a better fit to the data than model 2
  
# h)
  eps2 <- rnorm(n, mean = 0, sd = 0.1) # smaller variance than before
  y2 <- -1 + 0.5*x + eps2
  plot(x,y2)
  model1.2 <- lm(y2 ~ x)
  summary(model1.2)
  # beta0_hat = -0.997
  # beta1_hat = 0.502
  abline(model1.2, col = "red")
  legend(x = "bottomrigh", legend = c("data", "regression\nline"),
        pch = c(1, NA), lty = c(NA, 1), col = c("black", "red"))
  model2.2 <- lm(y2 ~ x + I(x^2)) # can't just use x^2, will use wrong model
  summary(model2.2)
  # beta0 and beta1 are estimated more accurately when we have less
  # noise in our data, which is not surprising. also, the coefficient on
  # the x^2 term is smaller here -- only -0.002, compared to -0.059 above --
  # indicating that less noise in the data also allows us to better rule out
  # model terms that do not reflect the underlying data generating process
  
# i)
  eps3 <- rnorm(n, mean = 0, sd = 2)
  y3 <- -1 + 0.5*x + eps3
  plot(x,y3)
  model1.3 <- lm(y3 ~ x)
  summary(model1.3)
  # beta0_hat = -0.885
  # beta1_hat = 0.389
  abline(model1.3, col = "red")
  legend(x = "bottomright", legend = c("data", "regression\nline"),
         pch = c(1, NA), lty = c(NA, 1), col = c("black", "red"))
  model2.3 <- lm(y3 ~ x + I(x^2)) # can't just use x^2, will use wrong model
  summary(model2.3)
  # with more data, our estimates of beta0 and beta1 are farther from the
  # truth, again not surprising. In addition, the coefficient on the x^2 term
  # is now -0.379, an order of magnitude larger than it was in our first model,
  # and two orders of magnitude larger than when we had less noise in the data.
  
# j)
  # confidence intervals:
    confint(model1)
    confint(model1.2)
    confint(model1.3)
    confint(model2)
    confint(model2.2)
    confint(model2.3)
  # and their lengths:
    abs(confint(model1)[,1] - confint(model1)[,2])
    abs(confint(model1.2)[,1] - confint(model1.2)[,2])
    abs(confint(model1.3)[,1] - confint(model1.3)[,2])
    abs(confint(model2)[,1] - confint(model2)[,2])
    abs(confint(model2.2)[,1] - confint(model2.2)[,2])
    abs(confint(model2.3)[,1] - confint(model2.3)[,2])
    # as we would expect, the CI's get narrower when there is less noise
    # in our data and wider when there is more noise.