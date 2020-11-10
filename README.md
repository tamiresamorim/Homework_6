# Homework_6
================
Tamires Amorim, Yamei Li and Meirou Guan and Carol
11/9/2020

``` r
load("~/R/acs2017_ny_data.RData")
```

### Binary Dependent Variable Model

The following project wants to understand the interactions of binary
variables as the dependent variable in a regression model. The analysis
will be based on the labor force participation, looking into different
groups and their working choices.

#### 1\. Loading and Understanding the data

First we loaded the data we want to observe, and transformed it into a
factor, this way our dummy variables will not be treated as a continuous
variable.

``` r
acs2017_ny$LABFORCE <- as.factor(acs2017_ny$LABFORCE)
levels(acs2017_ny$LABFORCE) <- c("NA","Not in LF","in LF")
acs2017_ny$MARST <- as.factor(acs2017_ny$MARST)
levels(acs2017_ny$MARST) <- c("married spouse present","married spouse absent","separated","divo
rced","widowed","never married")
```

Before getting to the interpretation of the models, we wanted to do the
summary of the variables to see the components of the sample we are
looking into.

``` r
acs2017_ny$age_bands <- cut(acs2017_ny$AGE,breaks=c(0,25,35,45,55,65,100))
table(acs2017_ny$age_bands,acs2017_ny$LABFORCE)
```

    ##           
    ##               NA Not in LF in LF
    ##   (0,25]   31680     11717 13256
    ##   (25,35]      0      4271 20523
    ##   (35,45]      0      4064 18924
    ##   (45,55]      0      5406 21747
    ##   (55,65]      0     10563 18106
    ##   (65,100]     0     28701  5880

When observing the difference between “NA” and Not in the Labor Force
based on the age group, we can assume the people not in the labor force
are the ones that do not work and are not actively seeking for a
job.Another important information is the fraction of people aging from
55 to 65 which is 18106/98436 or 0.1839, 18% of the labor force. The
larger group in the labor force are 45 to 55 years old consisting of 22%
of the group, and the smaller group aging from 65 to 100 (5.97%) which
can be some retirees that also have a job. From this we can conclude
that a good predictor for participation in the labor force might be the
gender, family constitution, marital status, since people on in the
higher age group will work to support their families.

#### 2\. Logit models and its analysis

##### A) subset the group (males from 25 to 65 years old)

We decided to look at different subsets including Male and female
separation. Because the coefficients on different X variables has
different effect on labor force based on gender, so while a X variable
could be positive overall it might be negative for 1 particular gender.
We also included the age group from 25 to 65, just as a baseline.

``` r
pick_use1 <- (acs2017_ny$AGE >25) & (acs2017_ny$AGE <= 65) & (acs2017_ny$female == 0)
dat_use1 <- subset(acs2017_ny, pick_use1)
dat_use1$LABFORCE <- droplevels(dat_use1$LABFORCE) # actually not necessary since logit is smart enough to drop unused levels, but helps my personal sense of order
```

Regression Model 1: Here we have age, race, and education as the
explanatory variables for the labor force.

``` r
model_logit1 <- glm(LABFORCE ~ AGE + I(AGE^2) + AfAm + Asian + race_oth + Hispanic
 + educ_hs + educ_somecoll + educ_college + educ_advdeg
 ,
 family = binomial, data = dat_use1)
summary(model_logit1)
```

    ## 
    ## Call:
    ## glm(formula = LABFORCE ~ AGE + I(AGE^2) + AfAm + Asian + race_oth + 
    ##     Hispanic + educ_hs + educ_somecoll + educ_college + educ_advdeg, 
    ##     family = binomial, data = dat_use1)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -2.6872   0.2983   0.4664   0.6532   2.0141  
    ## 
    ## Coefficients:
    ##                 Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)   -5.0345015  0.2096089 -24.019  < 2e-16 ***
    ## AGE            0.2944925  0.0093547  31.481  < 2e-16 ***
    ## I(AGE^2)      -0.0036214  0.0001011 -35.833  < 2e-16 ***
    ## AfAm          -0.6941129  0.0334176 -20.771  < 2e-16 ***
    ## Asian          0.3452579  0.0545181   6.333 2.41e-10 ***
    ## race_oth       0.0448291  0.0471890   0.950 0.342117    
    ## Hispanic       0.1365643  0.0414802   3.292 0.000994 ***
    ## educ_hs        0.9160095  0.0371118  24.682  < 2e-16 ***
    ## educ_somecoll  1.3569031  0.0425378  31.899  < 2e-16 ***
    ## educ_college   1.9552819  0.0468752  41.713  < 2e-16 ***
    ## educ_advdeg    2.2409633  0.0553954  40.454  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 49729  on 50199  degrees of freedom
    ## Residual deviance: 43231  on 50189  degrees of freedom
    ## AIC: 43253
    ## 
    ## Number of Fisher Scoring iterations: 5

##### Conclusions model1:

1)  The variables are statistically significant, less than 0.001 at
    confidence level.
2)  Other race cannot be predicted on the model, because there is not
    enough evidence to reject the null, and it is not adequately
    capturing the information we are looking for.
3)  Age squared is negative related with labor force, because as people
    age their participation in the labor force decrease over time.
4)  The African American participation in the labor force is also a
    negative, probably due to social differences in the country.

##### B) subset the group (females from 25 to 65 years old)

``` r
pick_use2 <- (acs2017_ny$AGE >25) & (acs2017_ny$AGE <= 65) & (acs2017_ny$female == 1)
dat_use2 <- subset(acs2017_ny, pick_use2)
dat_use2$LABFORCE <- droplevels(dat_use2$LABFORCE) 
```

Regression Model 2: Here we have age, race, education and marital status
as the explanatory variables for the labor force, we added the marital
status now because our bias is that women with family might not work as
much as a single women.

``` r
model_logit2 <- glm(LABFORCE ~ AGE + I(AGE^2) + AfAm + Asian + race_oth + Hispanic
 + educ_hs + educ_somecoll + educ_college + educ_advdeg
 + MARST ,
 family = binomial, data = dat_use1)
summary(model_logit2)
```

    ## 
    ## Call:
    ## glm(formula = LABFORCE ~ AGE + I(AGE^2) + AfAm + Asian + race_oth + 
    ##     Hispanic + educ_hs + educ_somecoll + educ_college + educ_advdeg + 
    ##     MARST, family = binomial, data = dat_use1)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -2.8623   0.2316   0.4256   0.6550   2.2976  
    ## 
    ## Coefficients:
    ##                              Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)                -2.5368051  0.2192261 -11.572  < 2e-16 ***
    ## AGE                         0.2343927  0.0095951  24.428  < 2e-16 ***
    ## I(AGE^2)                   -0.0031832  0.0001034 -30.791  < 2e-16 ***
    ## AfAm                       -0.4598483  0.0346910 -13.256  < 2e-16 ***
    ## Asian                       0.1905173  0.0551460   3.455 0.000551 ***
    ## race_oth                    0.0678073  0.0479541   1.414 0.157360    
    ## Hispanic                    0.1834625  0.0424238   4.325 1.53e-05 ***
    ## educ_hs                     0.8140069  0.0382283  21.293  < 2e-16 ***
    ## educ_somecoll               1.2185340  0.0437695  27.840  < 2e-16 ***
    ## educ_college                1.8275143  0.0480724  38.016  < 2e-16 ***
    ## educ_advdeg                 2.0583516  0.0566877  36.310  < 2e-16 ***
    ## MARSTmarried spouse absent -1.0903747  0.0649882 -16.778  < 2e-16 ***
    ## MARSTseparated             -0.7423781  0.0759638  -9.773  < 2e-16 ***
    ## MARSTdivo\nrced            -0.6105280  0.0425878 -14.336  < 2e-16 ***
    ## MARSTwidowed               -0.8386823  0.1053436  -7.961 1.70e-15 ***
    ## MARSTnever married         -1.3550902  0.0320004 -42.346  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 49729  on 50199  degrees of freedom
    ## Residual deviance: 41277  on 50184  degrees of freedom
    ## AIC: 41309
    ## 
    ## Number of Fisher Scoring iterations: 5

##### Conclusions model2:

1)  Apparently, with greater the amount of variables, some of the
    previous variables are not as important to predict the model for
    examples African American and Hispanic women, MARSTmarried spouse
    absent and MARSTwidowed are also out of our possible prediction.
2)  Other race now can be predicted, but with a lower significance level
    than the other variables.  
3)  Age squared keeps the negative relation with labor force, because as
    people age their participation in the labor force decrease over
    time. And education keeps its positive relation.

##### C) subset the group (everyone from 25 to 65 years old)

``` r
pick_use3 <- (acs2017_ny$AGE >25) & (acs2017_ny$AGE <= 65)
dat_use3 <- subset(acs2017_ny, pick_use1)
dat_use3$LABFORCE <- droplevels(dat_use3$LABFORCE)
```

Regression Model 3: Here we have age, race, education and marital status
as the explanatory variables for the labor force.

``` r
model_logit3 <- glm(LABFORCE ~ AGE + I(AGE^2) + female + AfAm + Asian + race_oth + Hispanic
 + educ_hs + educ_somecoll + educ_college + educ_advdeg
 + MARST ,
 family = binomial, data = dat_use3)
summary(model_logit3)
```

    ## 
    ## Call:
    ## glm(formula = LABFORCE ~ AGE + I(AGE^2) + female + AfAm + Asian + 
    ##     race_oth + Hispanic + educ_hs + educ_somecoll + educ_college + 
    ##     educ_advdeg + MARST, family = binomial, data = dat_use3)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -2.8623   0.2316   0.4256   0.6550   2.2976  
    ## 
    ## Coefficients: (1 not defined because of singularities)
    ##                              Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)                -2.5368051  0.2192261 -11.572  < 2e-16 ***
    ## AGE                         0.2343927  0.0095951  24.428  < 2e-16 ***
    ## I(AGE^2)                   -0.0031832  0.0001034 -30.791  < 2e-16 ***
    ## female                             NA         NA      NA       NA    
    ## AfAm                       -0.4598483  0.0346910 -13.256  < 2e-16 ***
    ## Asian                       0.1905173  0.0551460   3.455 0.000551 ***
    ## race_oth                    0.0678073  0.0479541   1.414 0.157360    
    ## Hispanic                    0.1834625  0.0424238   4.325 1.53e-05 ***
    ## educ_hs                     0.8140069  0.0382283  21.293  < 2e-16 ***
    ## educ_somecoll               1.2185340  0.0437695  27.840  < 2e-16 ***
    ## educ_college                1.8275143  0.0480724  38.016  < 2e-16 ***
    ## educ_advdeg                 2.0583516  0.0566877  36.310  < 2e-16 ***
    ## MARSTmarried spouse absent -1.0903747  0.0649882 -16.778  < 2e-16 ***
    ## MARSTseparated             -0.7423781  0.0759638  -9.773  < 2e-16 ***
    ## MARSTdivo\nrced            -0.6105280  0.0425878 -14.336  < 2e-16 ***
    ## MARSTwidowed               -0.8386823  0.1053436  -7.961 1.70e-15 ***
    ## MARSTnever married         -1.3550902  0.0320004 -42.346  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 49729  on 50199  degrees of freedom
    ## Residual deviance: 41277  on 50184  degrees of freedom
    ## AIC: 41309
    ## 
    ## Number of Fisher Scoring iterations: 5

##### Conclusions model3:

1)  As we increased the predictors from model 1 to model 3 the intercept
    got bigger from -5.0345015 (model 1), to -3.242e+00 (model 2) and
    -2.6002178 (model3).
2)  Divorced people and separated are not as statistically significant
    as the other variables.
3)  Asian and other race cannot predict given lack of evidence to reject
    the null.
4)  Female have a negative relation with labor force and all the other
    variables are statistically significant.

#### 3\. Probit Model and its analysis

##### A) The subset used is the same as the first model\_logit1:

Males from 25 to 65 years old and the predictors involve age, race and
education.

``` r
regn_probit1 <- glm(LABFORCE ~ AGE + I(AGE^2) + AfAm + Asian + race_oth + Hispanic 
            + educ_hs + educ_somecoll + educ_college + educ_advdeg,
            family = binomial (link = 'probit'), data = dat_use1)
summary(regn_probit1)
```

    ## 
    ## Call:
    ## glm(formula = LABFORCE ~ AGE + I(AGE^2) + AfAm + Asian + race_oth + 
    ##     Hispanic + educ_hs + educ_somecoll + educ_college + educ_advdeg, 
    ##     family = binomial(link = "probit"), data = dat_use1)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -2.7687   0.2777   0.4649   0.6661   1.9721  
    ## 
    ## Coefficients:
    ##                 Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)   -2.859e+00  1.180e-01 -24.231  < 2e-16 ***
    ## AGE            1.690e-01  5.270e-03  32.075  < 2e-16 ***
    ## I(AGE^2)      -2.081e-03  5.712e-05 -36.433  < 2e-16 ***
    ## AfAm          -4.041e-01  1.958e-02 -20.638  < 2e-16 ***
    ## Asian          1.677e-01  2.982e-02   5.625 1.86e-08 ***
    ## race_oth       1.374e-02  2.628e-02   0.523  0.60119    
    ## Hispanic       7.295e-02  2.343e-02   3.114  0.00185 ** 
    ## educ_hs        5.337e-01  2.218e-02  24.062  < 2e-16 ***
    ## educ_somecoll  7.898e-01  2.487e-02  31.755  < 2e-16 ***
    ## educ_college   1.116e+00  2.638e-02  42.296  < 2e-16 ***
    ## educ_advdeg    1.265e+00  3.011e-02  42.019  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 49729  on 50199  degrees of freedom
    ## Residual deviance: 43203  on 50189  degrees of freedom
    ## AIC: 43225
    ## 
    ## Number of Fisher Scoring iterations: 5

##### Conclusions probit model:

1)  The intercept is greater than the model1 (logit).
2)  Other race cannot predict given lack of evidence to reject the
    null.And Hispanic is not as statistically significant as the other
    variables, different from model one which might be due to the fact
    that we are looking at predicted probabilities, implying that the
    changes in labor force are not strongly related to the Hispanic
    population.Although, it might be a sign that the dummies selected
    are not capturing the variations in the labor force.

#### 4\. Predicting the model logit 1

``` r
#Predict
set.seed(11111)
index<-sample(x=2,size=nrow(dat_use1),replace=TRUE,prob=c(0.7,0.3))
train<-dat_use1[index==1,]
test<-dat_use1[index==2,]
dim(dat_use1)
```

    ## [1] 50200   110

``` r
dim(train)
```

    ## [1] 35233   110

``` r
dim(test)
```

    ## [1] 14967   110

``` r
trainmodel<-glm(LABFORCE ~ AGE + I(AGE^2) + AfAm + Asian + race_oth + Hispanic
 + educ_hs + educ_somecoll + educ_college + educ_advdeg
 ,
 family = binomial, data = dat_use1)
prob<-predict(object=trainmodel,newdata=test,type="response")# use the train model to predict
pred<-cbind(test,prob)
pred<-transform(pred,predict=ifelse(prob<=0.5,0,1)) #Reclassify the predicted probability values
ta<-table(pred$LABFORCE,pred$predict) #Compare the actual and predicted values of the model
ta
```

    ##            
    ##                 0     1
    ##   Not in LF   430  2478
    ##   in LF       323 11736

``` r
sum_diag<-sum(diag(ta)) #the sum of the correct predict Numbers
sum<-sum(ta) #the sum of predict Numbers
sum_diag/sum #Prediction accuracy
```

    ## [1] 0.8128549

##### Conclusion:

The prediction accuracy is 0.81258549. And from the plot below we want
to confirm the prediction.

``` r
#ROC CURVE
library(pROC)
```

    ## Warning: package 'pROC' was built under R version 4.0.3

    ## Type 'citation("pROC")' for a citation.

    ## 
    ## Attaching package: 'pROC'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     cov, smooth, var

``` r
roc_curve<-roc(test$LABFORCE,prob)
```

    ## Setting levels: control = Not in LF, case = in LF

    ## Setting direction: controls < cases

``` r
x<-1-roc_curve$specificities
y<-roc_curve$sensitivities
plot(x=x,y=y,xlim=c(0,1),ylim=c(0,1),xlab="1-specificity",
ylab="sensitivity",main="ROC Curve",type="l",lwd=2)
abline(a=0,b=1,col="grey")
auc<-roc_curve$auc
text(0.5,0.4,paste("AUC",round(auc,digits=2)),col="blue")
```

(HM--6_files/figure-gfm/unnamed-chunk-18-1.png)
The AUC=0.76, more than 0.75, the accuracy of this model is might be good.
