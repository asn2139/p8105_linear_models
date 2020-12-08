Bootstrapping
================

## Simulate data

``` r
n_samp = 250

sim_df_const = 
  tibble(
    x = rnorm(n_samp, 1, 1),
    error = rnorm(n_samp, 0, 1),
    y = 2 + 3 * x + error
  )

sim_df_nonconst = sim_df_const %>% 
  mutate(
  error = error * .75 * x,
  y = 2 + 3 * x + error
)
```

Plot the datasets

``` r
sim_df_const %>% 
  ggplot(aes(x=x, y=y))+geom_point()+
  geom_smooth(method="lm")
```

    ## `geom_smooth()` using formula 'y ~ x'

<img src="bootstrapping_files/figure-gfm/unnamed-chunk-2-1.png" width="90%" />

``` r
sim_df_nonconst %>% 
  ggplot(aes(x=x, y=y))+geom_point()+
  geom_smooth(method="lm")
```

    ## `geom_smooth()` using formula 'y ~ x'

<img src="bootstrapping_files/figure-gfm/unnamed-chunk-2-2.png" width="90%" />

``` r
lm(y~x, data=sim_df_const) %>% broom::tidy()
```

    ## # A tibble: 2 x 5
    ##   term        estimate std.error statistic   p.value
    ##   <chr>          <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)     1.98    0.0981      20.2 3.65e- 54
    ## 2 x               3.04    0.0699      43.5 3.84e-118

``` r
lm(y~x, data=sim_df_nonconst) %>% broom::tidy()
```

    ## # A tibble: 2 x 5
    ##   term        estimate std.error statistic   p.value
    ##   <chr>          <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)     1.93    0.105       18.5 1.88e- 48
    ## 2 x               3.11    0.0747      41.7 5.76e-114

\#\#Draw one boostrap sample

``` r
boot_sample=function (df){
  
  sample_frac(df,replace=TRUE) %>% 
    arrange(x)
  
}
```

Check if this works…

``` r
boot_sample(sim_df_nonconst) %>% 
  ggplot(aes(x=x, y=y))+
  geom_point(alpha=.3)+
  geom_smooth(method="lm")
```

    ## `geom_smooth()` using formula 'y ~ x'

<img src="bootstrapping_files/figure-gfm/unnamed-chunk-5-1.png" width="90%" />
