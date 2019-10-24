191024\_inclass
================
Gavin Ko
10/24/2019

Write some functions.

``` r
set.seed(1)
x = rnorm(n = 30, mean = 4, sd = 2.3)
x_again = rnorm(n = 30, mean = 2, sd = 1.6)
y = rnorm(n = 30, mean = 1.58, sd = 1.58)


z = (x - mean(x)) / sd(x)
z_again = (x_again - mean(x_again)) / sd(x_again)
```

So complicated, let’s write function instead.

``` r
# input in the first parenthesis
z_score = function(x) {
  
# conditional execution, make sure it breaks at the right time
  if (!is.numeric(x)) {
    stop("you are doing something wrong")
  } else if (length(x) <= 3) {
    stop("hey the data length is too small")
  }
  
  (x - mean(x)) / sd(x)
}

z_score(x_again)
```

    ##  [1]  1.5413954 -0.2961850  0.3204956 -0.2345965 -1.8983946 -0.6887392
    ##  [7] -0.6627062 -0.2415224  1.2161757  0.7926368 -0.3738088 -0.4855096
    ## [13]  0.7093845  0.5329776 -1.0329536 -1.0565157  0.2914637  0.7993727
    ## [19] -0.3082034  0.9409190  0.3336151 -0.9364778  0.2619634 -1.5869527
    ## [25]  1.6348722  2.3231173 -0.6286716 -1.4797904  0.5493942 -0.3367558

``` r
z_score(y)
```

    ##  [1]  2.38595050 -0.15569173  0.60338763 -0.08567310 -0.88879488
    ##  [6]  0.08175617 -1.99431792  1.41123701  0.04474978  2.14748861
    ## [11]  0.38031212 -0.85409202  0.52111213 -1.08749856 -1.42022815
    ## [16]  0.18864874 -0.57642714 -0.11368049 -0.03742056 -0.72869410
    ## [21] -0.70698088 -0.25559170  1.11189943 -1.70130726  0.50363910
    ## [26]  0.23186661  0.99216438 -0.43157534  0.27046563  0.16329598

Test another function.

``` r
mean_sd_function = function(input_x) {
  
  if (!is.numeric(x)) {
  stop("you are doing something wrong")
  
  } else if (length(x) <= 3) {
    stop("hey the data length is too small")
    }
  
  list(
  mean_x = mean(input_x),
  sd_x = sd(input_x),
  zscore = (input_x - mean(input_x)) / sd(input_x) )
  
}

mean_sd_function(y)
```

    ## $mean_x
    ## [1] 1.754239
    ## 
    ## $sd_x
    ## [1] 1.517348
    ## 
    ## $zscore
    ##  [1]  2.38595050 -0.15569173  0.60338763 -0.08567310 -0.88879488
    ##  [6]  0.08175617 -1.99431792  1.41123701  0.04474978  2.14748861
    ## [11]  0.38031212 -0.85409202  0.52111213 -1.08749856 -1.42022815
    ## [16]  0.18864874 -0.57642714 -0.11368049 -0.03742056 -0.72869410
    ## [21] -0.70698088 -0.25559170  1.11189943 -1.70130726  0.50363910
    ## [26]  0.23186661  0.99216438 -0.43157534  0.27046563  0.16329598

Multiple inputs

``` r
# we can set default settings for each value
sim_regression = function(n = 30, beta0 = 2, beta1 = 3) {
  
  sim_data = tibble(
    x = rnorm(n, mean = 1, sd = 1),
    y = beta0 + beta1 * x + rnorm(n, mean = 0, sd = 1)
  )

  ls_fit = lm(y ~ x, data = sim_data)

  tibble(
  beta0_hat = coef(ls_fit)[1], # for intercept
  beta1_hat = coef(ls_fit)[2]  # for the first coefficient
  )
  
}

sim_regression(n = 14, beta0 = 24)
```

    ## # A tibble: 1 x 2
    ##   beta0_hat beta1_hat
    ##       <dbl>     <dbl>
    ## 1      23.8      3.49
