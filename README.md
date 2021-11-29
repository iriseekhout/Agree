# Agree

The Agree package is developed to calculate the agreement and reliability between the scores of multiple raters or repeated measurments. The (proportion) agreement between multiple raters can be calculated for dichotomous scores as well as for polytomous scores. The confidence interval around the agreement can be calculated, adjusted for multiple raters. Additionally, the Intra Class Correlation (ICC) and Standard Error of Measurement (SEM) can be calculated for continuous scores. These can be obtained from estimated variance components extracted from a multilevel model.

## Package installation

The package can be installed directly from GitHub by `remotes::install_github(repo = 'iriseekhout/Agree')`


## Agreement for dichotomous outcomes

The use of the Agree package for the data example that was used in the paper on specific agreement on dichotomous outcomes in the situation of more than two raters. The method was fully explained an described in @devet2017.

### Description of data example
For the data example we used data from a study by @dikmans2017. This data is based on photographs of breasts of 50 women after breast reconstruction. The photographs are  independently scored by 5 surgeons, the patient, and three mothers. They each rated the quality of the reconstruction on a 5 point ordinal scale with the verbal anchors on the left side ‘very dissatisfied’ on the left end and on the right end ‘very satisfied’ on the right end. They specifically rated the volume, shape, symmetry, scars and nipple. In this paper we use the data of 4 surgeons because one surgeon had some missing values and we look at the rates for symmetry. The satisfaction scores were dichotomised into satisfied (scores 4 and 5) and not satisfied (scores 1,2, and 3).

```{r}
data(breast)
variable <- "symmetry"
raters <- c("PCH1", "PCH2", "PCH3", "PCH4")
ratersvars <- paste(raters, variable, sep="_")
data1 <- data.frame(breast[ratersvars])
 for (r in 1:length(ratersvars)){
    data1[ratersvars[r]] <- ifelse(data1[ratersvars[r]]=="very satisfied"|data1[ratersvars[r]]=="satisfied","satisfied","not satisfied")
 }
data1 <- data.frame(apply(data1[ratersvars], 2, as.factor), stringsAsFactors = TRUE)

```

### Agreement table
First the agreement tables are summed for all rater combinations into one agreement table. Then the off diagonal cells are averaged to obtain symmetry agreement tables. Note that data1 contains a column per rater for the variable of interest.

```{r}
sumtable(data1,offdiag = FALSE) 
sumtable(data1,offdiag = TRUE) 
```

### Agreement
From the agreement table we can calculate the agreement. And we can calculate the confidence interval around this agreement.
```{r}
agreement(data1, confint = TRUE)
```

### Specific agreement
The specific agreement for dichotomous data can be evaluated for the satisfied scores and for the not satisfied scores. 
```{r}
agreement(data1, specific="satisfied", confint = TRUE)
agreement(data1, specific="not satisfied", confint = TRUE)
```


## Polytomous outcomes

The use of the Agree package for two data examples that are used in the paper on specific agreement on polytomous outcomes in the situation of more than two raters [@devet2018]. The first data example is an example of ordinal ratings and the second example of nominal rating. 

### Ordinal data example
For the ordinal data example we use data from a study by @dikmans2017. This data is based on photographs of breasts of 50 women after breast reconstruction. The photographs are  independently scored by 5 surgeons, the patients, and three mothers. They each rated the quality of the reconstruction on a 5 point ordinal scale with the verbal anchors on the left side ‘very dissatisfied’ on the left end and on the right end ‘very satisfied’ on the right end. They specifically rated the volume, shape, symmetry, scars and nipple. In this paper we use the data of 4 surgeons because one surgeon had some missing values and we look at the rates for symmetry. Data set 1 is used for the example of ordinal categories.

```{r}
data(breast)

variable <- "symmetry"
raters <- c("PCH1", "PCH2", "PCH3", "PCH4")
ratersvars <- paste(raters, variable, sep="_")
data1 <- data.frame(breast[ratersvars])

```

### Agreement table
First the agreement table are summed for all rater combinations into one agreement table. Then the off diagonal cells are averaged to obtain symmetry agreement tables. 

```{r}

sumtable(data1,offdiag = FALSE)
sumtable(data1,offdiag = TRUE) 
```

### Agreement
From the agreement table we can calculate the agreement. And we can calculate the confidence interval around this agreement.
```{r}
agreement(data1, confint = TRUE)

```

### Specific agreement
The specific agreement for polytomous data, can be defined in two ways: the agreement of for one category versus not that category (e.g. very satisfied versus all other categories) or the agreement for one category versus any other (e.g. very satistfied versus satisfied). Below the Confidence intervals for the specific agreements are bootstrapped.


```{r}
agreement(data1, specific="satisfied", confint = TRUE)
agreement(data1, specific=c("satisfied", "very satisfied"), confint = TRUE)
agreement(data1, specific= c("satisfied","neutral"), confint = TRUE)

```

### Conditional probability
We can calulate the probability of any other outcome conditional on an specific outcome. 

```{r}
conditional.agreement(data1) 
```


### Weighted agreement
For ordinal data it might also be useful to look at the agreement when they may be one category off. So the agreement plus or minus one category, that categories is weighted (default weight=1). 

```{r}
weighted.agreement(data1)
weighted.agreement(data1, weight=0.5)

```

### nominal data example
For the nominal data example we use a data set that was used in a paper by @fleis1971. In this data patients are diagnosed in 5 categories: Depression, Personality Disorder, Schizophrenia, Neurosis, and Other by 6 raters. 

```{r}

data(diagnoses) 
data2 <- data.frame(lapply(diagnoses,as.factor), stringsAsFactors = TRUE)

 levels(data2$rater1) <- c("Depression", "Pers disord.", "Schizophrenia", "Neurosis", "Other")
 levels(data2$rater2) <- c("Depression", "Pers disord.", "Schizophrenia", "Neurosis", "Other")
 levels(data2$rater3) <- c("Depression", "Pers disord.", "Schizophrenia", "Neurosis", "Other")
 levels(data2$rater4) <- c("Depression", "Pers disord.", "Schizophrenia", "Neurosis", "Other")
 levels(data2$rater5) <- c("Depression", "Pers disord.", "Schizophrenia", "Neurosis", "Other")

```


### Agreement table
First the agreement table are summed for all rater combinations into one agreement table. Then the off diagonal cells are averaged to obtain symmetry agreement tables. 

```{r}
sumtable(data2,offdiag = FALSE) 
sumtable(data2,offdiag = TRUE) 
```

### Agreement
From the agreement table we can calculate the agreement. And we can calculate the confidence interval around this agreement.
```{r}
agreement(data2, confint = TRUE)
```

### Specific agreement
The specific agreement for polytomous data, can be defined in two ways: the agreement of for one category versus not that category (e.g. Depression versus all other categories) or the agreement for one category versus any other (e.g. Depression versus Schizophrenia). The confidence intervals for specific agreement are bootstrapped.
```{r}
agreement(data2, specific="Depression", confint = TRUE)
agreement(data2, specific="Pers disord.", confint = TRUE)
agreement(data2, specific="Schizophrenia", confint = TRUE)
agreement(data2, specific="Neurosis", confint = TRUE)
agreement(data2, specific="Other", confint = TRUE)

```

### Conditional agreement

```{r}
conditional.agreement(data2) %>% kable()
```

## ICC and SEM

The computational background and the use of the `icc()` function from the `Agree` package. We developed the `icc()` functions for this package in connection with a simulation study about sample size requirements for studies on reliability and measurement error @mokkink1 and a methodological paper about how to design and conduct a study on reliability and measurement error @mokkink2. 


## Data example

### Breast

The intra-class agreement is usually obtained for continuous ratings. As an example we can use data from data study by @dikmans2017. This data is based on photographs of breasts of 50 women after breast reconstruction. The photographs are independently scored by 5 surgeons, the patients, and three mammography nurses. They each rated the quality of the reconstruction on a 5 point ordinal scale with the verbal anchors on the left side ‘very dissatisfied’ on the left end and on the right end ‘very satisfied’ on the right end. They specifically rated the volume, shape, symmetry, scars and nipple. For the `icc` examples we can use the sum scores for volume, shape, symmetry, scars and nipple as an overall rating from each rater. 


```{r}
breast_scores <- 
Agree::breast %>%
  dplyr::select(Patient_score, PCH1_score, PCH2_score, PCH3_score, PCH4_score, 
                PCH5_score, Mam1_score, Mam2_score, Mam3_score)
```

The example data shows missings. The `icc` function can deal with these missings, because a mixed model is used to estimate the variances to compute the icc with.

For a mixed model, the data needs to be restructured to a long format. we can use the `pivot_longer()` function from the `tidyr` package to do that:

```{r}
breast_long <- breast_scores %>%
 mutate(id = 1:nrow(breast_scores)) %>% #add id column
  pivot_longer(cols = -id, names_to = "rater", values_to = "score")

breast_long
```

### Variance components

The variances that are used to compute the `icc` are obtained from a linear mixed model. This model is estimated with the `lmer()` function from the `lme4` package. In the `breast` example we have two levels: patients are level 1 and raters/observers are level 2. The two-level multilevel model is defined as $Y_{ijr} = \beta_0 + b_{0j} + b_{0r} + \epsilon_{ijr}$, where $b_{0j}$ is the random intercept at the subject level and $b_{0r}$ the random intercept at the rater/observer level. The $\epsilon_{ijr}$ is the residual error. The `r-code` for the model in `lme4` is: `lmer(score ~ (1|id) + (1|observer), data, REML = T)`

De exact specification of the multilevel model, depends on the design of the study and the type of ICC that one wants to compute. 



### Types of ICC

There are three types of icc incorporated in the `icc` function. The ICC oneway, ICC agreement and the ICC consistency. 


#### ICC oneway

The ICC type oneway is the variance between the subjects ($\sigma^2_j$) divided by the sum of the subject variance ($\sigma^2_j$) and the residual variance ($\sigma^2_{\epsilon}$). The $ICC_{oneway}$ is computed as follows: 

$ICC_{oneway} = \frac{\sigma^2_j}{\sigma^2_j + \sigma^2_{\epsilon}}$

The ICC oneway assumes that each subject is rated by a different set of raters, that are randomly selected from a larger population of judges (@shrout1979). 
The `icc_oneway()` uses the `varcomp()` function to compute the variance components. These variances are estimated from a `lmer` model with random slope for the subjects. $Y_{ij} = \beta_0 + b_{0j} + \epsilon_{ij}$

The standard error of measurement ($sem$) is the square root of the error variance (i.e. $sem = \sigma^2_{\epsilon}$). The confidence intervals are computed with the exact F method. $F = \frac{k  \sigma^2_{j} + \sigma^2_{\epsilon}}{\sigma^2_{\epsilon}}$, with $df1 = n - 1$ and $df2 = n  (k - 1)$ (@shrout1979).


For the oneway ICC, only the level 1, the patient level, is random. The rater variance is not used. 

The `r-code` to extract the variance component with the `varcomp` function is:

```{r}
varcomp(score~(1|id), data = breast_long)

```

The variance components, can be used to compute the ICC oneway:

```{r}
vc <- varcomp(score~(1|id), data = breast_long)
vc["id", "vcov"]/sum(vc[,"vcov"])

```

We have also incorporated a function that computed the ICC oneway directly from the data in the wide format, using the same steps. This is the `icc_oneway` function.


```{r}
icc_oneway(breast_scores)
```



#### ICC agreement

The icc type agreement is the variance between the subjects ($\sigma^2_j$) divided by the sum of the subject variance ($\sigma^2_j$), rater variance ($\sigma^2_k$) and the residual variance ($\sigma^2_\epsilon$). The $ICC_{agreement}$ is computed as follows:

$ICC_{agreement} = \frac{\sigma^2_j}{\sigma^2_j + \sigma^2_k + \sigma^2_{\epsilon}}$

The ICC for agreement generalizes to other raters within a population (@shrout1979). All subjects are rated by the same set of raters, and the rater variance is taken into account in the calculation of the ICC. The variance components are computed with the `icc_model()` function. This is a `lmer` model with a random slope for the subjects and for the raters. The $sem$ is the square root of the sum of the rater variance and the error variance (i.e. $sem = \sqrt{\sigma^2_r + \sigma^2_\epsilon}$). The confidence intervals are approximated to account for the three independent variance components, as defined by @satter1946 & @shrout1979.

For the ICC for agreement, both the level 1 and level 2 are random.

The `r-code` to extract the variance component with the `varcomp` function is:

```{r}
varcomp(score ~ (1|id) + (1|rater), data = breast_long)

```

The variance components, can be used to compute the ICC agreement:

```{r}
vc <- varcomp(score~ (1|id) + (1|rater), data = breast_long)
vc["id", "vcov"]/sum(vc[,"vcov"])

```

We have also incorporated a function that computed the ICC for agreement directly from the data in the wide format, using the same steps. This is the `icc_agreement` function.


```{r}
icc_agreement(breast_scores)
```


#### ICC consistency

The ICC type consistency is the variance between the subjects (&sigma;<sup>2</sup><sub>j</sub>) ($\sigma^2_j$) divided by the sum of the subject variance ($\sigma^2_j$) and the residual variance ($\sigma^2_\epsilon$). The rater variance is not used to calculate the ICC and can therefore also be considered as a fixed effect. The $ICC_{consistency}$ is computed as follows:

$ICC_{consistency} = \frac{\sigma^2_j}{\sigma^2_j + \sigma^2_{\epsilon}}$

The ICC for consistency generalizes only to the set of raters in the data (@shrout1979). The `varcomp()` function is used to compute the variance components. These variances are computed from a a `lmer` model with a random slope for the subjects and a fixed effect for the raters. The sem is the square root of the error variance. The confidence are computed with the exact F method. $F = \frac{(k  \sigma^2_j + \sigma^2_\epsilon)}{\sigma^2_\epsilon}$, with $df1 = n - 1$ and $df2 = (n - 1)  (k - 1)$ (@shrout1979).


For the ICC for consistency, the level 1 is a random effect and the level 2 is fixed.

The `r-code` to extract the variance component with the `varcomp` function is:

```{r}
varcomp(score ~ (1|id) + rater, data = breast_long)

```

The variance components, can be used to compute the ICC consistency:

```{r}
vc <- varcomp(score~ (1|id) + rater, data = breast_long)
vc["id", "vcov"]/sum(vc[,"vcov"])

```

We have also incorporated a function that computed the ICC consistency directly from the data in the wide format, using the same steps. This is the `icc_consistency` function.


```{r}
icc_consistency(breast_scores)
```



### Comparing ICC types

We have one general `icc` function that computes all three ICC types for a data set. The differences in computations between the ICC types can quickly be seen in the variance components returned by the `icc` function. We can obtain the variances by using `var = TRUE` in the `icc()` function, the `var_level2` shows the variance between the raters. Only for the ICC for agreement this variance component is estimated.

```{r}
# ICC for all methods
icc(breast_scores, var = TRUE)

```

When we estimate the ICC for the surgeons only, we can see that the variance at the rater level is decreased. This effect is directly shown in the ICC.

In the icc we can also use the data in wide format and use the `cols` option to define the rater columns that we want to use. 

```{r}
# ICC for all methods
icc(breast_scores, 
    cols = c("PCH1_score", "PCH2_score", "PCH3_score", "PCH4_score", "PCH5_score"), 
    var = TRUE)

```

When we estimate the ICC for the mammography nurses only, we see that the variance at the rater level is increased. This effect is directly shown in the ICC.

```{r}
# ICC for all methods
icc(breast_scores, 
    cols = c("Mam1_score", "Mam2_score", "Mam3_score"), 
    var = TRUE)

```

### Overview technical terms

|Term |Description|
|-----|------------------------------------|
|&beta;<sub>0</sub>|Fixed intercept|
|$b_{0j}$|Random intercept at subject level|
|$b_{0r}$|Random intercept at rater level|
|$\epsilon_{ijr}$|Residual error|
|$\sigma_j$|Variance between subjects|
|$\sigma_r$|Variance between raters|
|$\sigma_\epsilon$|Residual error variance|
|$k$|Number of raters/observers|
|$n$|Number of subjects|



