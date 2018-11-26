library(microbenchmark)
library(ggplot2)

## Write a function that generates numbers from binomial(n, p) distribution using runif() 
## function.

binom.generate = function (num, n, p) {
  result = rep(NA, num)
  for (i in 1:num) {
    uni = runif(n, 0, 1)
    bernou = as.numeric(uni < p)
    bin = sum(bernou)
    result[i] = bin
  }
  return(result)
}

## Compare performance of your function with rbinom() using microbenchmark() function.

microbenchmark(binom.generate(100, 30, 0.4), rbinom(100, 30, 0.4))

## Suppose we want to simulate data from a linear regression model:
## where e ~ N(0, 3) and X is a covariate that ranges between 20 and 40. Let B0 = 15 and 
## B1 = 0.4 are known coefficients. Generate data (N = 50) from this models with given 
## coefficients. Fit a linear regression model and plot fitted values vs residuals using 
## ggplot() function. Please do not forget to use set.seed() function for reproducibility.

set.seed(10)
x = runif(50, 20, 40)
res = rnorm(50, 0, 3)
y = 15 + 0.4*x + res

fit = lm(y ~ x)

ggplot() +
  geom_point(aes(fit$residuals, fit$fitted.values), size = 2) +
  theme_bw() +
  labs(x = "resiuals", y = "fitted values")

## generate U1 and U2 two independent uniform(0, 1) random variables. X = Rcos(\theta) and 
## Y = Rsin(\theta) are two independent normal variables. Write a function that generates 
## normal variates using Box-Muller algorithm. Compare simulated data from your function 
## with simulated data from rnorm() function using ggplot() (histogram?).

norm_generate = function(n) {
  if (n %% 2 == 1) {
    print("Please enter an even number!")
  } else {
    result = rep(NA, n)
    for(i in 1:n) {
      if(i %% 2 == 1) {
        U1 = runif(1, 0, 1)
        U2 = runif(1, 0, 1)
        R = sqrt(-2*log(U1))
        theta = 2*pi*U2
        x = R*cos(theta)
        y = R*sin(theta)
        result[i] = x
        result[i+1] = y
      }
    }
    return(result)
  }
}

## Generate and compare the data
N = 10000
box_muller = norm_generate(N)
using.rnorm = rnorm(N)

values = c(box_muller, using.rnorm)
group = c(rep("Box-Muller", N), rep("Using rnorm", N))
data = data.frame(values, group)

ggplot(data, aes(x = values, color = group, fill = group)) + 
  geom_histogram(position="dodge", alpha = 0.5) +
  theme_light()
