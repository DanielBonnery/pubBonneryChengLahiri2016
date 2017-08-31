# Multivariate Composite Estimation with An Application to the U.S. Labor Force Statistics 
`pubBonneryChengLahiri2016` is an R package that contains the source code to reproduce the graphs and simulations of different documents, including
* ["Multivariate Composite Estimation with An Application to the
U.S. Labor Force Statistics"]() by Bonnery, Cheng and Lahiri (see Section 2 and 3. below).

## 1. How to install the package

```r
devtools::install_github("DanielBonnery/pubBonneryLahiriCheng2016")
```

Note that this package depends on different packages I developped, that will be installed automatically, including:
* [CompositeRegressionEstimation](https://github.com/DanielBonnery/CompositeRegressionEstimation). CompositeRegressionEstimation is a package that contains generic functions to compute composite regression estimators, AK estimators, Empirical best linear estimators.
* [dataCPS](https://github.com/DanielBonnery/dataCPS). dataCPS is a package that contains tools to download the public use data files related to CPS from the Census Bureau website and convert them to R data frames.


## 2. Reproduction of the computations made on CPS data. 

### 2.1. Note on the data 
Here we give the programs we used to compute the estimates on the Census data, and we apply those programs to the CPS data that is published on the web by the Census. Differences between web data and data we had access to when we were working at the Census may exist and explain the differences we can see.

### 2.2. Execution


```r
library("pubBonneryChengLahiri2016")
demo(ComputeestimatesonCPSwebdata)
```





### 2.3. Results


The results of this first demonstration code are the two figures obtained from real data:
Differences from published paper come from possible difference between the online CPS data and the data that we used when we had access to the CPS data in the Census.
### 2.3.1. Figure 2.a.

```r
print(figure2.a)
```

![plot of chunk fig2a](figure/fig2a-1.png)


### 2.3.2. Figure 2.b.

```r
print(figure2.b)
```

![plot of chunk fig2b](figure/fig2b-1.png)


## 3. Reproduction of the simulations. 


### 3.1 R code

This code took 3 days to run on a latitude e6430 dell laptop.

```r
library("DanielBonnery/pubBonneryLahiriCheng2016")
demo(Simulation)
```




### 3.2. Results

Here, the results slightly differ from the published paper because we did not set the seed for the paper.
### 3.2.1. Table 2

```r
knitr::kable(table2.a,caption="Optimal (a_1,k_1) values for the three populations and the 3 objective functions")
```



|                       |level                  |change                |compromise            |CPSmethod     |
|:----------------------|:----------------------|:---------------------|:---------------------|:-------------|
|Independent            |( 0.0201 , 0.892 )     |( 0.0245 , 0.889 )    |( 0.0255 , 0.892 )    |( 0.3 , 0.4 ) |
|2dorder                |( 0.0115 , 0.109 )     |( 0.0193 , 0.0931 )   |( 0.0171 , 0.1 )      |( 0.3 , 0.4 ) |
|2dorder-indexdependent |( -0.00628 , -0.0321 ) |( 0.00357 , -0.0596 ) |( 0.00151 , -0.0524 ) |( 0.3 , 0.4 ) |

```r
knitr::kable(table2.b,caption="Optimal (a_2,k_2) values for the three populations and the 3 objective functions")
```



|                       |level                 |change                |compromise            |CPSmethod     |
|:----------------------|:---------------------|:---------------------|:---------------------|:-------------|
|Independent            |( -0.0143 , -0.1838 ) |( -0.0385 , -0.3653 ) |( -0.0065 , -0.1641 ) |( 0.4 , 0.7 ) |
|2dorder                |( 0.0363 , 0.309 )    |( 0.0355 , 0.2779 )   |( 0.0355 , 0.2877 )   |( 0.4 , 0.7 ) |
|2dorder-indexdependent |( 0.0534 , 0.5052 )   |( 0.0533 , 0.4788 )   |( 0.0538 , 0.4842 )   |( 0.4 , 0.7 ) |

## Bonus

### Table 1

```r
CPSrotationchart()
```

