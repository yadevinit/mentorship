# Deeper Learning with Research Data from Top 20 (2022-Sept)
The chosen model now incorporates research data of institution-wise publications and citations. Compared to the earlier model, this chosen model shows lesser `Deviance Residuals` and `AIC`. So, it may be preferred over the earlier one. Here is an understanding of the `Estimate` of the chosen statistical model's coefficients, which refers to the code-session extract included further ahead:
-  While holding the explanatory variables at mean (or baseline-reference) values, the expected mean `Score` (response variable) is around `3.6050e-01 *100 =36`.
-  A thousand units (`1000`) rise in (count of) actual undergraduate students `UGactualStudents` associates with a `-1.9624e-05 *100 *1000 =-1.96` rise in `Score`, i.e., adverse drop of `Score`. Less significantly, `PGactualStudents` associates with a bigger `-2.9555e-05 *100` impact, again adverse.
-  A hundred units (`100`) rise in Financial-Resources Operating Expense `FROsalaries` (in Indian Rs. crore) associates with a `4.6465e-04 *100 *100 =4.65` rise in `Score`. `FROmtce` has a lesser `3.9026e-04 *100` impact. `faculty` has even lesser (positive) impact `1.4158e-04 *100`.
-  A ten units (`10`) rise in per-faculty research publications `works2022perFaculty` has an adverse `-5.9139e-03 *100 *10 =-5.9` impact. But a thousand units (`1000`) rise in (count over 3 years with a lag of 2 years of) publication works `works_count2022` associates with expected `Score` raised by `1.6875e-04 *100 *1000 =16.9`.
-  A tenth of a unit (`0.1`) rise in per-faculty Amounts (in Indian Rs. crore) through Sponsored Research or Consultancy `SRCrevPerFaculty` associates with an expected `Score` rise of about `2.9483e-01 *100 *0.1 =2.9`. `SRagencies` impacts `6.4706e-04 *100`; `SRamount` impacts `-9.2171e-04 *100`. `Cprojects` impacts `1.0727e-04 *100`; `Corgs` impacts `-2.2666e-04 *100`.
-  Though statistically less significant: `year2022` has `1.3289e-02 *100 =1.3` impact, i.e., rise vs. (baseline-reference) `year2021`. `categoryIITnew` associates with a `1.4999e-01 *100 =15` rise vs. (baseline-reference) `categoryIIT5` (first-five IITs); `categoryNIT` too similarly.

The above understanding of the `Estimate` of chosen statistical model's coefficients refers to the following code-session extract and while holding the other explanatory variables at mean (or baseline-reference) values:

```{r}
[1] "Inference with this robust sandwich covar-matrix estimator while using Poisson means (and underestimated var)."

z test of coefficients:

                       Estimate  Std. Error z value  Pr(>|z|)    
(Intercept)          3.6050e-01  6.6317e-02  5.4360 5.448e-08 ***
year2022             1.3289e-02  6.3824e-03  2.0821 0.0373320 *  
UGactualStudents    -1.9624e-05  4.0107e-06 -4.8930 9.932e-07 ***
PGactualStudents    -2.9555e-05  1.5346e-05 -1.9258 0.0541266 .  
PhDpursuing         -2.0573e-05  2.0835e-05 -0.9874 0.3234424    
FROsalaries          4.6465e-04  1.2999e-04  3.5744 0.0003511 ***
FROmtce              3.9026e-04  7.7517e-05  5.0345 4.791e-07 ***
FROseminars         -2.1379e-03  1.6931e-03 -1.2627 0.2066837    
SRprojects          -4.3223e-05  6.3072e-05 -0.6853 0.4931522    
SRagencies           6.4706e-04  2.9114e-04  2.2225 0.0262520 *  
SRamount            -9.2171e-04  3.1703e-04 -2.9073 0.0036452 ** 
Cprojects            1.0727e-04  3.4245e-05  3.1324 0.0017336 ** 
Corgs               -2.2666e-04  6.9162e-05 -3.2772 0.0010484 ** 
Camount             -2.2175e-04  3.1728e-04 -0.6989 0.4846102    
faculty              1.4158e-04  4.0427e-05  3.5022 0.0004615 ***
categoryIITnew       1.4999e-01  6.1105e-02  2.4546 0.0141023 *  
categoryNIT          1.4897e-01  5.8520e-02  2.5457 0.0109072 *  
categoryOther        1.0327e-01  7.5523e-02  1.3674 0.1714939    
propPGPhD           -8.8484e-02  8.4375e-02 -1.0487 0.2943194    
propPhD              1.4487e-01  1.9591e-01  0.7395 0.4596181    
SRCrevPerFaculty     2.9483e-01  1.2145e-01  2.4276 0.0152011 *  
works_count2021     -1.3782e-04  9.7927e-05 -1.4074 0.1593179    
TC2021               2.9258e-06  3.0446e-06  0.9610 0.3365574    
works_count2022      1.6875e-04  9.1629e-05  1.8416 0.0655293 .  
TC2022              -2.3592e-06  2.7792e-06 -0.8489 0.3959462    
TCperWork2021       -1.4049e-03  9.1598e-03 -0.1534 0.8781034    
TCperWork2022        1.6284e-03  8.5322e-03  0.1908 0.8486451    
works2022perFaculty -5.9139e-03  1.1719e-03 -5.0465 4.500e-07 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1


Call:
glm(formula = fmla1b, data = dat)

Deviance Residuals: 
      Min         1Q     Median         3Q        Max  
-0.027479  -0.010275   0.000000   0.007636   0.034235  
[... snip ...]
(Dispersion parameter for gaussian family taken to be 0.0004975733)

    Null deviance: 0.4577053  on 42  degrees of freedom
Residual deviance: 0.0074636  on 15  degrees of freedom
  (23 observations deleted due to missingness)
AIC: -192.3

Number of Fisher Scoring iterations: 2
```

The corresponding (updated) Jupyter Notebook with `R` code and session output is at [inNIRF.ipynb](./worldClass/inNIRF-2022Sept01-2249.ipynb). The interested reader may extending this project, e.g., by properly reducing the factors institutions need to consider for raising `Score`.
