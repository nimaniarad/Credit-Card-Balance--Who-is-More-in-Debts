Credit Card Balance; Who is More in Debts
================
Nima Niarad
7/22/2021

<style> body {text-align: justify} </style>

<!-- Justify text. -->

## Part B - The Model

**Multiple Linear and Non-Linear Regression**

There are several states need to be considered. First, beginning with
the simplest one and then more complicated models.

As it was discussed in the last chapter, two factors appeared to be more
important than the others: Income and Limit. So:

``` r
library(readr)
Credit <- read_csv("C:/Nima/Rstudio/Git/Credit Card Balance; Who is More in Debts/Credit-Card-Balance--Who-is-More-in-Debts-/Credit.csv")
```

    ## 
    ## -- Column specification --------------------------------------------------------
    ## cols(
    ##   X1 = col_double(),
    ##   ID = col_double(),
    ##   Income = col_double(),
    ##   Limit = col_double(),
    ##   Rating = col_double(),
    ##   Cards = col_double(),
    ##   Age = col_double(),
    ##   Education = col_double(),
    ##   Gender = col_character(),
    ##   Student = col_character(),
    ##   Married = col_character(),
    ##   Ethnicity = col_character(),
    ##   Balance = col_double()
    ## )

``` r
lm.fit.1 = lm(Balance ~ Income + Limit, data = Credit)
summary(lm.fit.1)
```

    ## 
    ## Call:
    ## lm(formula = Balance ~ Income + Limit, data = Credit)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -232.79 -115.45  -48.20   53.36  549.77 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -385.17926   19.46480  -19.79   <2e-16 ***
    ## Income        -7.66332    0.38507  -19.90   <2e-16 ***
    ## Limit          0.26432    0.00588   44.95   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 165.5 on 397 degrees of freedom
    ## Multiple R-squared:  0.8711, Adjusted R-squared:  0.8705 
    ## F-statistic:  1342 on 2 and 397 DF,  p-value: < 2.2e-16

Statistically Significant: Both of them have p-values close to zero

However, the standard error of Income is higher than the standard error
of Limit but instead, it has a major effect in terms of the coefficient.

R-squared close to one: Right Variables & Well Fitted

Let see what happens if added Age and Education to support the idea of
removing them from the model.

``` r
lm.fit.2 = lm(Balance ~ Income + Limit + Age + Education, data = Credit)
summary(lm.fit.2)
```

    ## 
    ## Call:
    ## lm(formula = Balance ~ Income + Limit + Age + Education, data = Credit)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -215.74 -115.87  -45.15   56.40  547.20 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -3.564e+02  4.838e+01  -7.368 1.02e-12 ***
    ## Income      -7.560e+00  3.895e-01 -19.408  < 2e-16 ***
    ## Limit        2.637e-01  5.885e-03  44.810  < 2e-16 ***
    ## Age         -8.034e-01  4.883e-01  -1.645    0.101    
    ## Education    1.056e+00  2.649e+00   0.399    0.690    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 165.3 on 395 degrees of freedom
    ## Multiple R-squared:  0.872,  Adjusted R-squared:  0.8707 
    ## F-statistic: 672.9 on 4 and 395 DF,  p-value: < 2.2e-16

Their p-values are so high. It means failing to reject the null
hypothesis and they are not statistically significant.

They also have a huge standard error. It is interesting that they not
only have this amount of the error, they also have a negative effect on
other two main factors(Income and Limit). When Age and Education added,
the standard error rate of both Income and Limit are increased compared.
It can be said that the reason for this situation is there are
intersections between Age and Education with Limit and Income, but
they(Age and Education) are not well behaved with Balance.

As it appears in the result of the cor() and pairs() functions in the
previous chapter, Income and Limit have something in common. So, it is a
good idea to investigate that.

``` r
lm.fit.3 = lm(Balance ~ Income*Limit, data = Credit)
summary(lm.fit.3)
```

    ## 
    ## Call:
    ## lm(formula = Balance ~ Income * Limit, data = Credit)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -211.53 -110.14  -42.20   49.65  561.27 
    ## 
    ## Coefficients:
    ##                Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  -3.071e+02  3.046e+01 -10.082  < 2e-16 ***
    ## Income       -9.785e+00  7.461e-01 -13.115  < 2e-16 ***
    ## Limit         2.524e-01  6.839e-03  36.906  < 2e-16 ***
    ## Income:Limit  2.672e-04  8.082e-05   3.306  0.00103 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 163.4 on 396 degrees of freedom
    ## Multiple R-squared:  0.8746, Adjusted R-squared:  0.8736 
    ## F-statistic: 920.4 on 3 and 396 DF,  p-value: < 2.2e-16

The intersection of Limit and Income has a small p-value, but the
standard error and t-value are relatively high and small.

However, no rush to make a decision about them. Keeping it in the model,
investigating it later would be a better approach.

Notice that there is a plot of Balance vs Limit in the Part A. They have
a non-linear relationship and it should be included it in the model. For
now, Limit and Income intersection would be removed.

``` r
lm.fit.4 = lm(Balance ~ Income + Limit + I(Limit^2), data = Credit)
summary(lm.fit.4)
```

    ## 
    ## Call:
    ## lm(formula = Balance ~ Income + Limit + I(Limit^2), data = Credit)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -258.03  -99.31  -36.49   40.37  556.29 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -2.297e+02  3.284e+01  -6.995 1.14e-11 ***
    ## Income      -8.774e+00  4.175e-01 -21.014  < 2e-16 ***
    ## Limit        2.044e-01  1.184e-02  17.258  < 2e-16 ***
    ## I(Limit^2)   6.439e-06  1.118e-06   5.761 1.68e-08 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 159.2 on 396 degrees of freedom
    ## Multiple R-squared:  0.8811, Adjusted R-squared:  0.8802 
    ## F-statistic: 977.9 on 3 and 396 DF,  p-value: < 2.2e-16

The model is improved a little bit. The standard error of Income and
Limit is decreased, and there is a a small p-value regarding the added
sentence. Later, trying higher orders would be considered too.

Now it is the time to put them all together:

``` r
lm.fit.4 = lm(Balance ~ Income + Limit + Income*Limit + I(Limit^2), data = Credit)
summary(lm.fit.4)
```

    ## 
    ## Call:
    ## lm(formula = Balance ~ Income + Limit + Income * Limit + I(Limit^2), 
    ##     data = Credit)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -236.05  -85.97  -36.73   27.22  599.56 
    ## 
    ## Coefficients:
    ##                Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  -2.094e+02  3.222e+01  -6.497 2.47e-10 ***
    ## Income       -3.490e+00  1.164e+00  -2.999  0.00288 ** 
    ## Limit         1.334e-01  1.863e-02   7.160 3.96e-12 ***
    ## I(Limit^2)    1.851e-05  2.719e-06   6.810 3.66e-11 ***
    ## Income:Limit -9.275e-04  1.914e-04  -4.846 1.82e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 154.8 on 395 degrees of freedom
    ## Multiple R-squared:  0.8877, Adjusted R-squared:  0.8866 
    ## F-statistic: 780.9 on 4 and 395 DF,  p-value: < 2.2e-16

It seems it is on the right track!

P-values say the factors all are statistically significant. In addition,
RSquared is improved which means the model is fitted better now. The
intersection part can be the power of two or more even because the
relationship between Limit and Balance is non-linear.

so trying some higher orders would probably give a better result.

``` r
lm.fit.5 = lm(Balance ~ Income + Limit + I((Income*Limit)^3)+ I(Limit^3),
data = Credit)
summary(lm.fit.5)
```

    ## 
    ## Call:
    ## lm(formula = Balance ~ Income + Limit + I((Income * Limit)^3) + 
    ##     I(Limit^3), data = Credit)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -210.47  -96.32  -36.73   42.10  559.49 
    ## 
    ## Coefficients:
    ##                         Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)           -2.439e+02  3.012e+01  -8.100 6.90e-15 ***
    ## Income                -8.397e+00  4.078e-01 -20.592  < 2e-16 ***
    ## Limit                  2.190e-01  9.612e-03  22.781  < 2e-16 ***
    ## I((Income * Limit)^3) -6.048e-17  1.493e-17  -4.052 6.13e-05 ***
    ## I(Limit^3)             6.507e-10  1.144e-10   5.686 2.54e-08 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 158.9 on 395 degrees of freedom
    ## Multiple R-squared:  0.8818, Adjusted R-squared:  0.8806 
    ## F-statistic: 736.8 on 4 and 395 DF,  p-value: < 2.2e-16

Fortunately, it is getting better. P-value of Income and Limit is
closing to 0 in comparison to its linear state. What is more, standard
errors are smaller than the previous one.

Higher orders can be tried, but the model would not be improved anymore.
Let’s prove that:

``` r
lm.fit.6 = lm(Balance ~ Income + Limit + I((Income*Limit)^4)+ I(Limit^4) + Student,
data = Credit)
summary(lm.fit.6)
```

    ## 
    ## Call:
    ## lm(formula = Balance ~ Income + Limit + I((Income * Limit)^4) + 
    ##     I(Limit^4) + Student, data = Credit)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -242.218  -69.528   -0.829   57.381  245.754 
    ## 
    ## Coefficients:
    ##                         Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)           -3.285e+02  1.524e+01 -21.562  < 2e-16 ***
    ## Income                -8.746e+00  2.368e-01 -36.941  < 2e-16 ***
    ## Limit                  2.420e-01  4.237e-03  57.110  < 2e-16 ***
    ## I((Income * Limit)^4) -2.570e-23  3.782e-24  -6.794 4.03e-11 ***
    ## I(Limit^4)             4.446e-14  4.752e-15   9.357  < 2e-16 ***
    ## StudentYes             4.374e+02  1.563e+01  27.975  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 93.52 on 394 degrees of freedom
    ## Multiple R-squared:  0.9591, Adjusted R-squared:  0.9586 
    ## F-statistic:  1850 on 5 and 394 DF,  p-value: < 2.2e-16

One of the most important aspects of the model is decreased now;
R-squared. Also, the standard errors are increasing.

Now it is time to consider Student and Married factors.

``` r
lm.fit.7 = lm(Balance ~ Income + Limit + I((Income*Limit)^3)+ I(Limit^3) + Student + Married,
data = Credit)
summary(lm.fit.7)
```

    ## 
    ## Call:
    ## lm(formula = Balance ~ Income + Limit + I((Income * Limit)^3) + 
    ##     I(Limit^3) + Student + Married, data = Credit)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -271.924  -64.254    1.458   58.771  210.482 
    ## 
    ## Coefficients:
    ##                         Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)           -2.692e+02  1.757e+01 -15.322  < 2e-16 ***
    ## Income                -8.790e+00  2.283e-01 -38.505  < 2e-16 ***
    ## Limit                  2.153e-01  5.379e-03  40.019  < 2e-16 ***
    ## I((Income * Limit)^3) -6.777e-17  8.372e-18  -8.095 7.27e-15 ***
    ## I(Limit^3)             7.487e-10  6.417e-11  11.669  < 2e-16 ***
    ## StudentYes             4.381e+02  1.488e+01  29.436  < 2e-16 ***
    ## MarriedYes             2.983e-01  9.175e+00   0.033    0.974    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 88.77 on 393 degrees of freedom
    ## Multiple R-squared:  0.9633, Adjusted R-squared:  0.9627 
    ## F-statistic:  1718 on 6 and 393 DF,  p-value: < 2.2e-16

Married p-value indicates that it is not statistically significant (fail
to reject the null hypothesis).

At this point it is a good idea to try log() on Income. It cannot be
imporoved but no harm to prove it, right:

``` r
lm.fit.8 = lm(Balance ~ log(Income) + Limit + I((Income*Limit)^3)+ I(Limit^3)+ Student,
data = Credit)
summary(lm.fit.8)
```

    ## 
    ## Call:
    ## lm(formula = Balance ~ log(Income) + Limit + I((Income * Limit)^3) + 
    ##     I(Limit^3) + Student, data = Credit)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -350.09  -84.86    5.29   90.29  320.96 
    ## 
    ## Coefficients:
    ##                         Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)            5.126e+02  4.186e+01  12.247  < 2e-16 ***
    ## log(Income)           -3.168e+02  1.301e+01 -24.350  < 2e-16 ***
    ## Limit                  2.174e-01  7.497e-03  28.996  < 2e-16 ***
    ## I((Income * Limit)^3) -9.135e-17  1.153e-17  -7.920 2.44e-14 ***
    ## I(Limit^3)             4.797e-10  8.703e-11   5.511 6.44e-08 ***
    ## StudentYes             4.209e+02  2.043e+01  20.604  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 122.4 on 394 degrees of freedom
    ## Multiple R-squared:   0.93,  Adjusted R-squared:  0.9291 
    ## F-statistic:  1047 on 5 and 394 DF,  p-value: < 2.2e-16

R-squared is decreased and residual standard error is increased.

``` r
Model.Final = lm(Balance ~ Income + Limit + I((Income*Limit)^3)+ I(Limit^3)+ Student,
data = Credit)
summary(Model.Final)
```

    ## 
    ## Call:
    ## lm(formula = Balance ~ Income + Limit + I((Income * Limit)^3) + 
    ##     I(Limit^3) + Student, data = Credit)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -271.760  -64.148    1.586   58.809  210.610 
    ## 
    ## Coefficients:
    ##                         Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)           -2.691e+02  1.683e+01 -15.987  < 2e-16 ***
    ## Income                -8.790e+00  2.280e-01 -38.556  < 2e-16 ***
    ## Limit                  2.153e-01  5.366e-03  40.116  < 2e-16 ***
    ## I((Income * Limit)^3) -6.775e-17  8.335e-18  -8.128 5.72e-15 ***
    ## I(Limit^3)             7.486e-10  6.396e-11  11.704  < 2e-16 ***
    ## StudentYes             4.380e+02  1.482e+01  29.563  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 88.66 on 394 degrees of freedom
    ## Multiple R-squared:  0.9633, Adjusted R-squared:  0.9628 
    ## F-statistic:  2067 on 5 and 394 DF,  p-value: < 2.2e-16

Significant improvement in R-squared and all P-Values are close to zero!
R-squared is 0.9633 which means predictors explain 96.33% of the
variability in Balance that is a great outcome.

However, relying just on R-squared and P-Values is not enough to say
this model is the best one.

Usually, by adding more variables, the adjusted R-squared would be
decreased (directly related to the degrees of freedom).

The good news is the adjusted R-squared is 96.28% saying how powerful is
this final Model. In other words, it means important variables are all
there. Even adding one variable that should not be there can have a
negative effect on the adjusted R-squared.

The residual standard error would be equal to about 9.41 on 394 degrees
of freedom.

The Coefficients are:

``` r
coef(Model.Final)
```

    ##           (Intercept)                Income                 Limit 
    ##         -2.690611e+02         -8.790142e+00          2.152740e-01 
    ## I((Income * Limit)^3)            I(Limit^3)            StudentYes 
    ##         -6.774881e-17          7.486043e-10          4.380298e+02
