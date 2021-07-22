Credit Card Balance; Who is More in Debts
================
Nima Niarad
7/22/2021

<style> body {text-align: justify} </style>

<!-- Justify text. -->

## Part B

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
