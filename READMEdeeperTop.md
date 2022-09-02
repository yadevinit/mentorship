# Deeper Learning from Top 20 (2022-Aug)
For use by institutional stakeholders, here is an understanding of the `Estimate` of a chosen statistical model's coefficients, which refers to the code-session extract included further ahead:
-  While holding the explanatory variables at mean (or baseline) values, the expected mean `Score` (response variable) is around `6.4693e-01 *100 =64.7`.
-  As per this model, it would be unacceptably erroneous to conclude that explanatory variables such as `year` or `category` of institutions (e.g., whether `NIT` or the first-five IITs `IIT5`) have significant impact on `Score` (response).
-  Unit rise in count of Consultancy Projects `Cprojects` is associated with around `-1.2678e-04 *100` impact on `Score`. In contrast to that adverse impact, unit rise in count of Consultancy Organizations `Corgs` associates with `1.9670e-04 *100` impact. So, more (Consultancy) Organizations with fewer Projects may be preferred, unless institutions make any (`Cprojects`-related) adverse-impact linkages (on `Score`) disappear. Unit rise in count of Sponsored Research Projects `SRprojects` too shows an adverse impact of about `-1.1249e-04 *100`; so, fewer of those projects, and bigger ones instead, may be preferred.
-  A tenth of a unit (`0.1`) rise in proportion of actual postgraduate students and those pursuing PhD `propPGPhD` associates with a `-2.5978e-01 *100 *0.1` impact, i.e., adverse drop of `Score` by about `2.5978`. Though less significant, thousand units (`1000`) rise in (count of) actual undergraduate students `UGactualStudents` associates with `-9.1098e-06 *100 *1000 =-0.91098 Score` impact, again adverse. In contrast, a tenth of a unit (`0.1`) rise in per-faculty Amounts (in Indian Rs. crore) through Sponsored Research or Consultancy `SRCrevPerFaculty` associates with an expected `Score` rise of about `3.5872e-01 *100 *0.1 =3.5872`.

The above understanding of the `Estimate` of chosen statistical model's coefficients refers to the following code-session extract and while holding the other explanatory variables at mean (or baseline) values:

```{r}
[1] "Inference with this robust sandwich covar-matrix estimator while using Poisson means (and underestimated var)."

z test of coefficients:

                    Estimate  Std. Error z value  Pr(>|z|)    
(Intercept)       6.4693e-01  5.4684e-02 11.8304 < 2.2e-16 ***
year2022          7.0863e-03  7.3535e-03  0.9637  0.335215    
UGactualStudents -9.1098e-06  5.5055e-06 -1.6547  0.097992 .  
PGactualStudents  1.4254e-05  1.5446e-05  0.9228  0.356100    
PhDpursuing       3.9126e-05  2.9047e-05  1.3470  0.177978    
FROsalaries       1.3649e-04  2.1695e-04  0.6291  0.529258    
FROmtce           8.9298e-05  9.9140e-05  0.9007  0.367733    
FROseminars      -1.4952e-03  2.9028e-03 -0.5151  0.606493    
SRprojects       -1.1249e-04  5.9059e-05 -1.9046  0.056828 .  
SRagencies        5.2146e-04  4.0927e-04  1.2741  0.202619    
SRamount         -3.6481e-04  2.9965e-04 -1.2174  0.223436    
Cprojects        -1.2678e-04  3.1589e-05 -4.0133 5.988e-05 ***
Corgs             1.9670e-04  6.0761e-05  3.2374  0.001206 ** 
Camount          -6.7810e-05  4.9487e-04 -0.1370  0.891010    
faculty           4.6736e-05  5.5602e-05  0.8405  0.400608    
categoryIITnew   -5.6166e-02  6.9194e-02 -0.8117  0.416953    
categoryNIT      -4.1791e-02  6.3029e-02 -0.6631  0.507298    
categoryOther    -5.9196e-02  6.3214e-02 -0.9364  0.349049    
propPGPhD        -2.5978e-01  9.1313e-02 -2.8449  0.004442 ** 
propPhD           2.3588e-01  1.8394e-01  1.2824  0.199701    
SRCrevPerFaculty  3.5872e-01  1.3388e-01  2.6795  0.007373 ** 
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1


Call:
glm(formula = fmla1, data = dat)

Deviance Residuals: 
      Min         1Q     Median         3Q        Max  
-0.059557  -0.012109   0.001587   0.012946   0.048870  
[... snip ...]
(Dispersion parameter for gaussian family taken to be 0.0009968028)

    Null deviance: 0.45771  on 42  degrees of freedom
Residual deviance: 0.02193  on 22  degrees of freedom
  (23 observations deleted due to missingness)
AIC: -159.96

Number of Fisher Scoring iterations: 2
```
The corresponding Jupyter Notebook with `R` code and session output is at [`inNIRF.ipynb`](./worldClass/inNIRF-2022Aug26-1518.ipynb). The interested reader who wishes to extend this study could (a) include quality of research publications, e.g., via citations, (b) adjust for past years' `Score`, and (c) include more institutions.
