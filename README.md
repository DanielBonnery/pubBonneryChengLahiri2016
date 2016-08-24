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


## 2. Reproduction of the computation made on CPS data. 

### 2.1. Note on the data 
Here we give the programs we used to compute the estimates on the Census data, and we apply those programs to the CPS data that is published on the web by the Census. Differences between web data and data we had access to when we were working at the Census may exist and explain the differences we can see.

### 2.2. Execution


```r
library("pubBonneryChengLahiri2016")
demo(ComputeestimatesonCPSwebdata)
```

### 2.3. Results



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


## 3. Reproduction of the simulations made on CPS data. 


### 3.1 R code


```r
library("DanielBonnery/pubBonneryLahiriCheng2016")
demo(Simulation)
```

### 3.2. Results

### 3.2.1. Figure 1

```r
print(1)
```
### 3.2.2. Table 2

```r
knitr::kable(data.frame(x=1))
```



|  x|
|--:|
|  1|
### 3.2.3. Table 3

```r
knitr::kable(data.frame(x=1))
```



|  x|
|--:|
|  1|
### 3.2.4. Table 4

```r
knitr::kable(data.frame(x=1))
```



|  x|
|--:|
|  1|
### 3.2.4. Table 5

```r
knitr::kable(data.frame(x=1))
```



|  x|
|--:|
|  1|
### 3.2.4. Table 6

```r
knitr::kable(data.frame(x=1))
```



|  x|
|--:|
|  1|

### 3.2.4. Table 7

```r
CPSrotationchart()
```

```
FALSE 
FALSE % Table created by stargazer v.5.2 by Marek Hlavac, Harvard University. E-mail: hlavac at fas.harvard.edu
FALSE % Date and time: Tue, Aug 23, 2016 - 09:47:02 PM
FALSE \begin{table}[!htbp] \centering 
FALSE   \caption{} 
FALSE   \label{table:rotchart} 
FALSE \begin{tabular}{@{\extracolsep{0pt}} ccccccccccccccccccccc} 
FALSE \\[-1.8ex]\hline 
FALSE \hline \\[-1.8ex] 
FALSE Jan 05 & \$S\_\{1!,!1\}\$ & \$S\_\{1!,!2\}\$ & \$S\_\{1!,!3\}\$ & \$S\_\{1!,!4\}\$ &  &  &  &  &  &  &  &  & \$S\_\{1!,!5\}\$ & \$S\_\{1!,!6\}\$ & \$S\_\{1!,!7\}\$ & \$S\_\{1!,!8\}\$ &  &  &  &  \\ 
FALSE Feb 05 &  & \$S\_\{2!,!1\}\$ & \$S\_\{2!,!2\}\$ & \$S\_\{2!,!3\}\$ & \$S\_\{2!,!4\}\$ &  &  &  &  &  &  &  &  & \$S\_\{2!,!5\}\$ & \$S\_\{2!,!6\}\$ & \$S\_\{2!,!7\}\$ & \$S\_\{2!,!8\}\$ &  &  &  \\ 
FALSE Mar 05 &  &  & \$S\_\{3!,!1\}\$ & \$S\_\{3!,!2\}\$ & \$S\_\{3!,!3\}\$ & \$S\_\{3!,!4\}\$ &  &  &  &  &  &  &  &  & \$S\_\{3!,!5\}\$ & \$S\_\{3!,!6\}\$ & \$S\_\{3!,!7\}\$ & \$S\_\{3!,!8\}\$ &  &  \\ 
FALSE Apr 05 &  &  &  & \$S\_\{4!,!1\}\$ & \$S\_\{4!,!2\}\$ & \$S\_\{4!,!3\}\$ & \$S\_\{4!,!4\}\$ &  &  &  &  &  &  &  &  & \$S\_\{4!,!5\}\$ & \$S\_\{4!,!6\}\$ & \$S\_\{4!,!7\}\$ & \$S\_\{4!,!8\}\$ &  \\ 
FALSE May 05 &  &  &  &  & \$S\_\{5!,!1\}\$ & \$S\_\{5!,!2\}\$ & \$S\_\{5!,!3\}\$ & \$S\_\{5!,!4\}\$ &  &  &  &  &  &  &  &  & \$S\_\{5!,!5\}\$ & \$S\_\{5!,!6\}\$ & \$S\_\{5!,!7\}\$ & \$S\_\{5!,!8\}\$ \\ 
FALSE Jun 05 &  &  &  &  &  & \$S\_\{6!,!1\}\$ & \$S\_\{6!,!2\}\$ & \$S\_\{6!,!3\}\$ & \$S\_\{6!,!4\}\$ &  &  &  &  &  &  &  &  & \$S\_\{6!,!5\}\$ & \$S\_\{6!,!6\}\$ & \$S\_\{6!,!7\}\$ \\ 
FALSE Jul 05 &  &  &  &  &  &  & \$S\_\{7!,!1\}\$ & \$S\_\{7!,!2\}\$ & \$S\_\{7!,!3\}\$ & \$S\_\{7!,!4\}\$ &  &  &  &  &  &  &  &  & \$S\_\{7!,!5\}\$ & \$S\_\{7!,!6\}\$ \\ 
FALSE Aug 05 &  &  &  &  &  &  &  & \$S\_\{8!,!1\}\$ & \$S\_\{8!,!2\}\$ & \$S\_\{8!,!3\}\$ & \$S\_\{8!,!4\}\$ &  &  &  &  &  &  &  &  & \$S\_\{8!,!5\}\$ \\ 
FALSE Sep 05 &  &  &  &  &  &  &  &  & \$S\_\{9!,!1\}\$ & \$S\_\{9!,!2\}\$ & \$S\_\{9!,!3\}\$ & \$S\_\{9!,!4\}\$ &  &  &  &  &  &  &  &  \\ 
FALSE Oct 05 &  &  &  &  &  &  &  &  &  & \$S\_\{10!,!1\}\$ & \$S\_\{10!,!2\}\$ & \$S\_\{10!,!3\}\$ & \$S\_\{10!,!4\}\$ &  &  &  &  &  &  &  \\ 
FALSE Nov 05 &  &  &  &  &  &  &  &  &  &  & \$S\_\{11!,!1\}\$ & \$S\_\{11!,!2\}\$ & \$S\_\{11!,!3\}\$ & \$S\_\{11!,!4\}\$ &  &  &  &  &  &  \\ 
FALSE Dec 05 &  &  &  &  &  &  &  &  &  &  &  & \$S\_\{12!,!1\}\$ & \$S\_\{12!,!2\}\$ & \$S\_\{12!,!3\}\$ & \$S\_\{12!,!4\}\$ &  &  &  &  &  \\ 
FALSE Jan 06 &  &  &  &  &  &  &  &  &  &  &  &  & \$S\_\{13!,!1\}\$ & \$S\_\{13!,!2\}\$ & \$S\_\{13!,!3\}\$ & \$S\_\{13!,!4\}\$ &  &  &  &  \\ 
FALSE Feb 06 &  &  &  &  &  &  &  &  &  &  &  &  &  & \$S\_\{14!,!1\}\$ & \$S\_\{14!,!2\}\$ & \$S\_\{14!,!3\}\$ & \$S\_\{14!,!4\}\$ &  &  &  \\ 
FALSE Mar 06 &  &  &  &  &  &  &  &  &  &  &  &  &  &  & \$S\_\{15!,!1\}\$ & \$S\_\{15!,!2\}\$ & \$S\_\{15!,!3\}\$ & \$S\_\{15!,!4\}\$ &  &  \\ 
FALSE Apr 06 &  &  &  &  &  &  &  &  &  &  &  &  &  &  &  & \$S\_\{16!,!1\}\$ & \$S\_\{16!,!2\}\$ & \$S\_\{16!,!3\}\$ & \$S\_\{16!,!4\}\$ &  \\ 
FALSE May 06 &  &  &  &  &  &  &  &  &  &  &  &  &  &  &  &  & \$S\_\{17!,!1\}\$ & \$S\_\{17!,!2\}\$ & \$S\_\{17!,!3\}\$ & \$S\_\{17!,!4\}\$ \\ 
FALSE Jun 06 &  &  &  &  &  &  &  &  &  &  &  &  &  &  &  &  &  & \$S\_\{18!,!1\}\$ & \$S\_\{18!,!2\}\$ & \$S\_\{18!,!3\}\$ \\ 
FALSE Jul 06 &  &  &  &  &  &  &  &  &  &  &  &  &  &  &  &  &  &  & \$S\_\{19!,!1\}\$ & \$S\_\{19!,!2\}\$ \\ 
FALSE Aug 06 &  &  &  &  &  &  &  &  &  &  &  &  &  &  &  &  &  &  &  & \$S\_\{20!,!1\}\$ \\ 
FALSE \hline \\[-1.8ex] 
FALSE \end{tabular} 
FALSE \end{table} 
FALSE 
FALSE % Table created by stargazer v.5.2 by Marek Hlavac, Harvard University. E-mail: hlavac at fas.harvard.edu
FALSE % Date and time: Tue, Aug 23, 2016 - 09:47:03 PM
FALSE \begin{table}[!htbp] \centering 
FALSE   \caption{} 
FALSE   \label{table:rotchart} 
FALSE \begin{tabular}{@{\extracolsep{0pt}} c} 
FALSE \\[-1.8ex]\hline 
FALSE \hline \\[-1.8ex] 
FALSE CPS rotation chart \\ 
FALSE \hline \\[-1.8ex] 
FALSE \end{tabular} 
FALSE \end{table} 
FALSE \begin{table}[\!htbp] \centering    \caption{}    \label{table:rotchart}  \begin{tabular}{@{\extracolsep{0pt}} ccccccccccccccccccccc}  \\[-1.8ex]\hline  &$G_{1}$&$G_{2}$&$G_{3}$&$G_{4}$&$G_{5}$&$G_{6}$&$G_{7}$&$G_{8}$&$G_{9}$&$G_{10}$&$G_{11}$&$G_{12}$&$G_{13}$&$G_{14}$&$G_{15}$&$G_{16}$&$G_{17}$&$G_{18}$&$G_{19}$&$G_{20}$\\\\ \hline \\[-1.8ex]  Jan 05 & $S_{1\!,\!1}$ & $S_{1\!,\!2}$ & $S_{1\!,\!3}$ & $S_{1\!,\!4}$ &  &  &  &  &  &  &  &  & $S_{1\!,\!5}$ & $S_{1\!,\!6}$ & $S_{1\!,\!7}$ & $S_{1\!,\!8}$ &  &  &  &  \\  Feb 05 &  & $S_{2\!,\!1}$ & $S_{2\!,\!2}$ & $S_{2\!,\!3}$ & $S_{2\!,\!4}$ &  &  &  &  &  &  &  &  & $S_{2\!,\!5}$ & $S_{2\!,\!6}$ & $S_{2\!,\!7}$ & $S_{2\!,\!8}$ &  &  &  \\  Mar 05 &  &  & $S_{3\!,\!1}$ & $S_{3\!,\!2}$ & $S_{3\!,\!3}$ & $S_{3\!,\!4}$ &  &  &  &  &  &  &  &  & $S_{3\!,\!5}$ & $S_{3\!,\!6}$ & $S_{3\!,\!7}$ & $S_{3\!,\!8}$ &  &  \\  Apr 05 &  &  &  & $S_{4\!,\!1}$ & $S_{4\!,\!2}$ & $S_{4\!,\!3}$ & $S_{4\!,\!4}$ &  &  &  &  &  &  &  &  & $S_{4\!,\!5}$ & $S_{4\!,\!6}$ & $S_{4\!,\!7}$ & $S_{4\!,\!8}$ &  \\  May 05 &  &  &  &  & $S_{5\!,\!1}$ & $S_{5\!,\!2}$ & $S_{5\!,\!3}$ & $S_{5\!,\!4}$ &  &  &  &  &  &  &  &  & $S_{5\!,\!5}$ & $S_{5\!,\!6}$ & $S_{5\!,\!7}$ & $S_{5\!,\!8}$ \\  Jun 05 &  &  &  &  &  & $S_{6\!,\!1}$ & $S_{6\!,\!2}$ & $S_{6\!,\!3}$ & $S_{6\!,\!4}$ &  &  &  &  &  &  &  &  & $S_{6\!,\!5}$ & $S_{6\!,\!6}$ & $S_{6\!,\!7}$ \\  Jul 05 &  &  &  &  &  &  & $S_{7\!,\!1}$ & $S_{7\!,\!2}$ & $S_{7\!,\!3}$ & $S_{7\!,\!4}$ &  &  &  &  &  &  &  &  & $S_{7\!,\!5}$ & $S_{7\!,\!6}$ \\  Aug 05 &  &  &  &  &  &  &  & $S_{8\!,\!1}$ & $S_{8\!,\!2}$ & $S_{8\!,\!3}$ & $S_{8\!,\!4}$ &  &  &  &  &  &  &  &  & $S_{8\!,\!5}$ \\  Sep 05 &  &  &  &  &  &  &  &  & $S_{9\!,\!1}$ & $S_{9\!,\!2}$ & $S_{9\!,\!3}$ & $S_{9\!,\!4}$ &  &  &  &  &  &  &  &  \\  Oct 05 &  &  &  &  &  &  &  &  &  & $S_{10\!,\!1}$ & $S_{10\!,\!2}$ & $S_{10\!,\!3}$ & $S_{10\!,\!4}$ &  &  &  &  &  &  &  \\  Nov 05 &  &  &  &  &  &  &  &  &  &  & $S_{11\!,\!1}$ & $S_{11\!,\!2}$ & $S_{11\!,\!3}$ & $S_{11\!,\!4}$ &  &  &  &  &  &  \\  Dec 05 &  &  &  &  &  &  &  &  &  &  &  & $S_{12\!,\!1}$ & $S_{12\!,\!2}$ & $S_{12\!,\!3}$ & $S_{12\!,\!4}$ &  &  &  &  &  \\  Jan 06 &  &  &  &  &  &  &  &  &  &  &  &  & $S_{13\!,\!1}$ & $S_{13\!,\!2}$ & $S_{13\!,\!3}$ & $S_{13\!,\!4}$ &  &  &  &  \\  Feb 06 &  &  &  &  &  &  &  &  &  &  &  &  &  & $S_{14\!,\!1}$ & $S_{14\!,\!2}$ & $S_{14\!,\!3}$ & $S_{14\!,\!4}$ &  &  &  \\  Mar 06 &  &  &  &  &  &  &  &  &  &  &  &  &  &  & $S_{15\!,\!1}$ & $S_{15\!,\!2}$ & $S_{15\!,\!3}$ & $S_{15\!,\!4}$ &  &  \\  Apr 06 &  &  &  &  &  &  &  &  &  &  &  &  &  &  &  & $S_{16\!,\!1}$ & $S_{16\!,\!2}$ & $S_{16\!,\!3}$ & $S_{16\!,\!4}$ &  \\  May 06 &  &  &  &  &  &  &  &  &  &  &  &  &  &  &  &  & $S_{17\!,\!1}$ & $S_{17\!,\!2}$ & $S_{17\!,\!3}$ & $S_{17\!,\!4}$ \\  Jun 06 &  &  &  &  &  &  &  &  &  &  &  &  &  &  &  &  &  & $S_{18\!,\!1}$ & $S_{18\!,\!2}$ & $S_{18\!,\!3}$ \\  Jul 06 &  &  &  &  &  &  &  &  &  &  &  &  &  &  &  &  &  &  & $S_{19\!,\!1}$ & $S_{19\!,\!2}$ \\  Aug 06 &  &  &  &  &  &  &  &  &  &  &  &  &  &  &  &  &  &  &  & $S_{20\!,\!1}$ \\  \hline \\[-1.8ex]  \end{tabular}  \end{table}  \begin{table}[\!htbp] \centering    \caption{}    \label{table:rotchart}  \begin{tabular}{@{\extracolsep{0pt}} c}  \\[-1.8ex]\hline  \hline \\[-1.8ex]  CPS rotation chart \\  \hline \\[-1.8ex]  \end{tabular}  \end{table}
```
## Bonus

### Table 1

```r
CPSrotationchart()
```
