Credit Card Balance; Who is More in Debts
================
Nima Niarad
7/22/2021

<style> body {text-align: justify} </style>

<!-- Justify text. -->

## Part D - Conclusion

In this project, I have analyzed the credit card balance.

I have shown which variables play a key role and have more impact on the
balance to be included in the model. There are ten variables to be
investigated the relationships between them and the main dependent
variable, Balance.

After plotting them to know what are the relationships, I decided to
begin with Married, Education, Age, Income, Limit, and Student in
multiple linear regression.

Gradually, with trying different kinds of models, I found out that Age,
Married, and Education are not statistically significant and should be
removed from the model.

There was a non-linearity between Limit and Balance, also Limit and
Income were correlated. So, I tried some high order equations with the
intersection of Limit and Income. Because Limit and Income were
connected and Limit behave in a non-linear way with Balance, I also
tried the intersection with high orders.

The last model in Part B with minimum standard and residual error, and
also R-squared close to one (it interpret about the 96 percent of
variability which was great!) is the best one.

However, just counting on R-squared is not a wise approach. That is why
I investigate the models with better techniques Part C.

Three main tests were applied; the validation set approach. LOOCV, and
K-fold cross-validation.

Using the first one to prove that I was right about the best model. The
second and the third test are almost the same approach with K-fold is
slightly better in some aspects. By using the second and third methods,
I figured it out if I go further in terms of high orders, the model will
not be improved. It turned out the third degree is the best one.

Now that I have selected the best model, it is time to interpret what
happens between Balance and other variables.

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
Model.Final = lm(Balance ~ Income + Limit + I((Income*Limit)^3)+ I(Limit^3)+ Student, data = Credit)
coef(Model.Final)
```

    ##           (Intercept)                Income                 Limit 
    ##         -2.690611e+02         -8.790142e+00          2.152740e-01 
    ## I((Income * Limit)^3)            I(Limit^3)            StudentYes 
    ##         -6.774881e-17          7.486043e-10          4.380298e+02

The Income coefficient telling me if an individual has one dollar more
income, their balance will be decreasing by 8 dollars. In other words,
rich people are supposed to be less in debt to banks which are sounds
logical, but it seems it is not the whole story! When I look at the
limit, understand that if somebody s limit is one dollar higher, they
have about 0.215 dollars more balance. The coefficient of the
intersection of Income and Limit and Limit with the power of three is
small! I cannot expect a lot of influences from them. On the other hand,
students play a key role. If you are a student, you are four and a half
dollars more in debt in comparison to non-students. As a result,
individuals who are both rich and student can manage their balances
better. However, the most reliable persons here are rich people who are
not students, they are less than the others in debt to the banks. It
would be suggested that banks give a high limit to rich people, but
students are not reliable to give them credit cards with high limits. I
need more data regrading what percentage of people are both rich and
students. Even their universities are important, public or private or if
students have a part-time job are less in debt or the ones who have rich
parents are more reliable!
