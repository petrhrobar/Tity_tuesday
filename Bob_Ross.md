PCA and Classification of Bob Ross’s Paintings
================
Petr Hrobař
2020-07-26

# Introduction

In this example we shall apply Principal component analysis (PCA) and
Clustering methods for classifying paintings by [Bob
Ross.](https://cs.wikipedia.org/wiki/Bob_Ross) Data used for purposes of
this analysis are comming from tidyverse community
![\\textbf{tidytuesday}](https://latex.codecogs.com/png.latex?%5Ctextbf%7Btidytuesday%7D
"\\textbf{tidytuesday}") project. See more infromations
[here.](https://github.com/rfordatascience/tidytuesday)

We are interested in dataset named ![\\textbf{Bob Ross - painting by the
numbers}](https://latex.codecogs.com/png.latex?%5Ctextbf%7BBob%20Ross%20-%20painting%20by%20the%20numbers%7D
"\\textbf{Bob Ross - painting by the numbers}"). Dataset as well as its
description can be found
[here.](https://github.com/rfordatascience/tidytuesday/tree/master/data/2019/2019-08-06)

First, let’s do some basic data cleaning:

``` r
# packages
library(tidyverse)
library(reshape2)
theme_set(theme_light())

# Loading the data
bob_ross <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-08-06/bob-ross.csv")

# Data Cleaning
df_cleaned <- 
  bob_ross %>% 
  janitor::clean_names() %>% 
  gather(element, value, -title, -episode) %>% 
  mutate(title = str_to_title(str_remove_all(title, '"')),
         element = str_to_title(str_replace(element, "_", " "))) %>% 
  extract(episode, c("season", "episode"), "S(.*)E(.*)",
          convert = T) %>% 
  arrange(season, episode) %>% 
  filter(value == 1)


df_cleaned %>% 
  head()
```

    ## # A tibble: 6 x 5
    ##   season episode title               element   value
    ##    <int>   <int> <chr>               <chr>     <dbl>
    ## 1      1       1 A Walk In The Woods Bushes        1
    ## 2      1       1 A Walk In The Woods Deciduous     1
    ## 3      1       1 A Walk In The Woods Grass         1
    ## 4      1       1 A Walk In The Woods River         1
    ## 5      1       1 A Walk In The Woods Tree          1
    ## 6      1       1 A Walk In The Woods Trees         1

Data are cleaned and gathered, i.e. each row is the element of painting
e.g. “Beach”, “Tree” etc, where we observe wheater one particular
painting has perticulars elements.

# Principal Component Analysis.

Principal component analysis is a Dimensionality reduction technique
that has been around for almost a century now. It turns out that PCA is
so popular it has been independently reinvendet multiple times. Here we
provide basic math overview of technique and various approahces to
computation itself.

Let’s say we have data matrix
![D](https://latex.codecogs.com/png.latex?D "D") that is ![n \\times
m](https://latex.codecogs.com/png.latex?n%20%5Ctimes%20m "n \\times m")
matrix. Where ![n](https://latex.codecogs.com/png.latex?n "n") is the
number of observation and ![m](https://latex.codecogs.com/png.latex?m
"m") is the number of observed features (variables). Firstly, we
standardize the data so that columns means are equal to zero.

Steps above can be perform in
![\\textbf{R}](https://latex.codecogs.com/png.latex?%5Ctextbf%7BR%7D
"\\textbf{R}") using following code:

``` r
D <- df_cleaned %>% 
  acast(title ~ element)

D[c(1:10) ,c(25:35)]
```

    ##                     Flowers Fog Framed Grass Guest Half Circle_frame
    ## A Mild Winter's Day       0   0      0     0     0                 0
    ## A Pretty Autumn Day       0   0      0     1     0                 0
    ## A Walk In The Woods       0   0      0     1     0                 0
    ## Absolutely Autumn         0   0      0     1     0                 0
    ## After The Rain            0   0      0     0     0                 0
    ## Anatomy Of A Wave         0   0      0     0     1                 0
    ## Angler's Haven            0   0      0     0     0                 0
    ## Arctic Beauty             0   0      0     1     0                 0
    ## Arctic Winter Day         0   0      0     0     0                 0
    ## Arizona Splendor          0   0      0     1     0                 0
    ##                     Half Oval_frame Hills Lake Lighthouse Mill
    ## A Mild Winter's Day               0     0    0          0    0
    ## A Pretty Autumn Day               0     0    0          0    0
    ## A Walk In The Woods               0     0    0          0    0
    ## Absolutely Autumn                 0     0    0          0    0
    ## After The Rain                    0     0    0          0    0
    ## Anatomy Of A Wave                 0     0    0          0    0
    ## Angler's Haven                    0     0    1          0    0
    ## Arctic Beauty                     0     0    0          0    0
    ## Arctic Winter Day                 0     0    1          0    0
    ## Arizona Splendor                  0     0    0          0    0

Now we have created a data matrix
![D](https://latex.codecogs.com/png.latex?D "D"), where rows of the
matrix are particular paintings and columns are indicading binary
presence of the element in the painting. Now, we want to standardize the
matrix and hence create a centered matrix
![C](https://latex.codecogs.com/png.latex?C "C"), where column’s means
are equal to zero.

Formally, data centering can be written as:

  
![C\_{ij} = D\_{ij} -
\\hat{D}\_{j}](https://latex.codecogs.com/png.latex?C_%7Bij%7D%20%3D%20D_%7Bij%7D%20-%20%5Chat%7BD%7D_%7Bj%7D
"C_{ij} = D_{ij} - \\hat{D}_{j}")  
.

Additionally, if each variables of data (columns of
![D](https://latex.codecogs.com/png.latex?D "D")) are measured on
different units, we should not only subtract column means from each
observation, but also devide each observation by associated column’s
standart deviation. This conversion proces is called the
![\\textbf{Z-standardization}](https://latex.codecogs.com/png.latex?%5Ctextbf%7BZ-standardization%7D
"\\textbf{Z-standardization}") and it is commontly used to transfort
normal distribution to standart normal distribution. However, since all
of our columns in data matrix are binary indicators of element presence,
no variance transformations are required.

``` r
# Centering the data - substracting columns means from each column
C <- t(t(D) - colMeans(D))

# Column means are now equal to zero.
colMeans(C[ ,c(2, 4, 5, 10)])
```

    ## Aurora Borealis           Beach            Boat          Cactus 
    ##   -1.830613e-19   -3.242800e-18   -1.830613e-19   -5.840526e-19

### Mathematical Solution to PCA

As mentioned above, PCA has multiple approaches to the solution. We will
focus on two solution that are at disposal.

  - 1)  Eigenvector and values decomposition of Variance-Covariance
        Matrix

  - 2)  Singular value decompositon of Centered data Matrix

First approach of solving PCA requires to have the
![\\textbf{Variance-Covariance
Matrix}](https://latex.codecogs.com/png.latex?%5Ctextbf%7BVariance-Covariance%20Matrix%7D
"\\textbf{Variance-Covariance Matrix}") of (Centered) dataset. Using
Centered data matrix we can calculate the variance-covariance matrix as:
  
![C^{T}C\\frac{1}{n-1}](https://latex.codecogs.com/png.latex?C%5E%7BT%7DC%5Cfrac%7B1%7D%7Bn-1%7D
"C^{T}C\\frac{1}{n-1}")  

After this step, we can calculate PCA as a eigenvector and eigenvalue
decompositon of variance-covariance matrix.

``` r
# Variance-Covariance Matrix
COV <- (t(C) %*% C) * 1/(nrow(C) - 1)

# eigenvectors and eigenvalues of covariance matrix
PCA_eigen <- eigen(COV)

# Weights of elements on first four PC space.
PCA_eigen$vectors %>% 
  as.data.frame() %>% 
  mutate(elements = colnames(D)) %>% 
  select(elements, PC1 = V1, PC2 =  V2, PC3 =  V3, PC4 = V4) %>% 
  tibble() %>% 
  head()
```

    ## # A tibble: 6 x 5
    ##   elements             PC1      PC2     PC3      PC4
    ##   <chr>              <dbl>    <dbl>   <dbl>    <dbl>
    ## 1 Apple Frame     -0.00179  0.00366 0.00213  0.00527
    ## 2 Aurora Borealis  0.00428  0.00617 0.0151  -0.00180
    ## 3 Barn            -0.0357   0.0446  0.0561   0.0173 
    ## 4 Beach           -0.129   -0.223   0.0521   0.0114 
    ## 5 Boat            -0.00728 -0.00791 0.00363  0.0101 
    ## 6 Bridge          -0.0125   0.0262  0.00991 -0.00214

This approach was performed just to demenstrate basic algebra behind the
PCA.

Second approach of solving PCA uses ![\\textbf{Singular Value
Decomposition
(SVD)}](https://latex.codecogs.com/png.latex?%5Ctextbf%7BSingular%20Value%20Decomposition%20%28SVD%29%7D
"\\textbf{Singular Value Decomposition (SVD)}"). It turns out that
computing variance-covariance matrix and then performing eigenvectors
and eigenvalues caluclation is not necessarily the most computationally
efficient way of performing PCA.

Now let’s use
[SVD](https://en.wikipedia.org/wiki/Singular_value_decomposition) to
obtain PCA results. Firstly, we take our center matrix
![C](https://latex.codecogs.com/png.latex?C "C") and perform SVD.

``` r
# SVD on center matrix C
PCA_svd <- svd(C)

# Weights of elements on first four PC space.
PCA_svd %>% 
  broom::tidy(matrix = "v") %>% 
  filter(PC <= 4) %>% 
  mutate(elements = colnames(D)[column]) %>% 
  pivot_wider(names_from = PC,
              names_prefix = "PC") %>% 
  select(-column) %>% 
  head()
```

    ## # A tibble: 6 x 5
    ##   elements             PC1      PC2      PC3      PC4
    ##   <chr>              <dbl>    <dbl>    <dbl>    <dbl>
    ## 1 Apple Frame     -0.00179  0.00366 -0.00213  0.00527
    ## 2 Aurora Borealis  0.00428  0.00617 -0.0151  -0.00180
    ## 3 Barn            -0.0357   0.0446  -0.0561   0.0173 
    ## 4 Beach           -0.129   -0.223   -0.0521   0.0114 
    ## 5 Boat            -0.00728 -0.00791 -0.00363  0.0101 
    ## 6 Bridge          -0.0125   0.0262  -0.00991 -0.00214

So we can see that both results are equal. Quickly, we can use build-in
function to confirm that the results are exactly whe same:

``` r
# Build in function (Hastie)
PCA_func <- prcomp(D, center = T)

# Weights of elements on first four PC space.
PCA_func$rotation %>%
  as.data.frame() %>% 
  mutate(elements = colnames(D)) %>% 
  select(elements, PC1, PC2, PC3, PC4) %>% 
  tibble() %>% 
  head()
```

    ## # A tibble: 6 x 5
    ##   elements             PC1      PC2      PC3      PC4
    ##   <chr>              <dbl>    <dbl>    <dbl>    <dbl>
    ## 1 Apple Frame     -0.00179  0.00366 -0.00213  0.00527
    ## 2 Aurora Borealis  0.00428  0.00617 -0.0151  -0.00180
    ## 3 Barn            -0.0357   0.0446  -0.0561   0.0173 
    ## 4 Beach           -0.129   -0.223   -0.0521   0.0114 
    ## 5 Boat            -0.00728 -0.00791 -0.00363  0.0101 
    ## 6 Bridge          -0.0125   0.0262  -0.00991 -0.00214

Since all the results are the same, we can from now on work only with
one PCA object. Let’s work with object we named *PCA\_svd*. We are
interested in what percentage of variability is explained by each
component.

``` r
PCA_svd %>% 
  broom::tidy(matrix = "d") %>% 
  select(-std.dev) %>% 
  gather(key, value, -PC) %>% 
  ggplot(aes(PC, value, color = key)) + 
  geom_line(size = 1) + 
  geom_point(size = 2.5) + 
  scale_y_continuous(labels = scales::percent_format()) + 
  scale_x_continuous(breaks = c(seq(0, 66, by = 5))) + 
  labs(x = "PC", 
       y = "Percentage of variability explained", 
       title = "Percentage of variability explained by each PC", 
       color = "Type of measure")
```

![](Bob_Ross_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

As can be seen, using just 2 PCs we are able to explain about 27% of
variablitity of elements presence. Using 3 PCs number of variability
explained will increase to 37%.

Let’s examine whether some form of clustering is present.

``` r
PCA_svd %>% 
  broom::tidy(matrix = "v") %>% 
  filter(PC <= 4) %>% 
  mutate(elements = colnames(D)[column]) %>% 
  pivot_wider(names_from = PC,
              names_prefix = "PC") %>% 
  select(-column) %>% 
  ggplot(aes(PC1, PC2, label = elements)) + 
  geom_point(color = "dodgerblue", size = 2) + 
  geom_text(color = "gray21") + 
  geom_hline(aes(yintercept = 0), lty = 2, color = "red") + 
  geom_vline(aes(xintercept = 0), lty = 2, color = "red") + 
  labs(title = "Element's Weights on PC1 and PC2 space")
```

![](Bob_Ross_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

Above we can see minor clustering of elements of the paintings that tend
to appear together. For example, ![Mountain, Snow,
Winter](https://latex.codecogs.com/png.latex?Mountain%2C%20Snow%2C%20Winter
"Mountain, Snow, Winter"). Still a lot of variability is not explained
using simple 2-dimensions. Now we can inspect what paintings tend to be
more similiar:

``` r
set.seed(2020)

Paintings_clusters <- 
  PCA_svd %>% 
  broom::tidy(matrix = "u") %>% 
  filter(PC <= 4) %>% 
  mutate(painting = rownames(D)[row]) %>% 
  pivot_wider(names_from = PC,
              names_prefix = "PC") %>% 
  select(painting, PC1, PC2) %>% 
  filter(grepl("Winter's|Beach|Waves|Waterfall|sun|Sun", painting)) %>% 
  mutate(color_element = 
           case_when(
             grepl("Winter's", painting) ~ "Winter Theme",
             grepl("Beach", painting) ~ "Beach Theme",
             grepl("Waves", painting) ~ "Beach Theme")) %>% 
  filter(!is.na(color_element))

Paintings_clusters %>% 
  ggplot(aes(PC1, PC2, label = painting, color = color_element)) + 
  geom_point() + 
  geom_text(check_overlap = TRUE, size = 3) + 
  geom_hline(aes(yintercept = 0), lty = 2, color = "red") + 
  geom_vline(aes(xintercept = 0), lty = 2, color = "red") + 
  labs(title = "Painting's clustering using first two Principal Components", 
       subtitle = "We are inspecting themes of elements, e.g. Winter, beach, Sun etc.")
```

![](Bob_Ross_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

We can see that we can somewhat cluster paintings using PCA of
painting’s elements. Lets inspect some paintings here.

``` r
Paintings_clusters %>% 
  select(painting, color_element)
```

    ## # A tibble: 12 x 2
    ##    painting              color_element
    ##    <chr>                 <chr>        
    ##  1 A Mild Winter's Day   Winter Theme 
    ##  2 Balmy Beach           Beach Theme  
    ##  3 Perfect Winter's Day  Winter Theme 
    ##  4 Rowboat On The Beach  Beach Theme  
    ##  5 Secluded Beach        Beach Theme  
    ##  6 Sunset Over The Waves Beach Theme  
    ##  7 Waves Of Wonder       Beach Theme  
    ##  8 Windy Waves           Beach Theme  
    ##  9 Winter's Elegance     Winter Theme 
    ## 10 Winter's Grace        Winter Theme 
    ## 11 Winter's Paradise     Winter Theme 
    ## 12 Winter's Peace        Winter Theme

``` r
Paintings_clusters %>% 
acast(painting ~ color_element, margins = TRUE)
```

    ##                       Beach Theme Winter Theme (all)
    ## A Mild Winter's Day             0            1     1
    ## Balmy Beach                     1            0     1
    ## Perfect Winter's Day            0            1     1
    ## Rowboat On The Beach            1            0     1
    ## Secluded Beach                  1            0     1
    ## Sunset Over The Waves           1            0     1
    ## Waves Of Wonder                 1            0     1
    ## Windy Waves                     1            0     1
    ## Winter's Elegance               0            1     1
    ## Winter's Grace                  0            1     1
    ## Winter's Paradise               0            1     1
    ## Winter's Peace                  0            1     1
    ## (all)                           6            6    12
