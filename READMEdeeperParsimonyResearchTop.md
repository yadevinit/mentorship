# Parsimonious Deeper Learning with Research Data from Top 20 (2022-Sept)
The chosen model now incorporates research data of institution-wise publications and citations. Compared to the earlier model, this chosen model shows lesser `Deviance Residuals` and `AIC`. So, it may be preferred over the earlier one. Here is an understanding of the `Estimate` of the chosen statistical model's coefficients, which refers to the code-session extract included further ahead:
-  While holding the explanatory variables at mean (or baseline-reference) values, the expected mean `Score` (response variable) is around `3.6050e-01 *100 =36`.
-  A thousand units (`1000`) rise in (count of) actual undergraduate students `UGactualStudents` associates with a `-1.9624e-05 *100 *1000 =-1.96` rise in `Score`, i.e., adverse drop of `Score`. Less significantly, `PGactualStudents` associates with a bigger `-2.9555e-05 *100` impact, again adverse.
-  A hundred units (`100`) rise in Financial-Resources Operating Expense `FROsalaries` (in Indian Rs. crore) associates with a `4.6465e-04 *100 *100 =4.65` rise in `Score`. `FROmtce` has a lesser `3.9026e-04 *100` impact. `faculty` has even lesser (positive) impact `1.4158e-04 *100`.
-  A ten units (`10`) rise in per-faculty research publications `works2022perFaculty` has an adverse `-5.9139e-03 *100 *10 =-5.9` impact. But a thousand units (`1000`) rise in (count over 3 years with a lag of 2 years of) publication works `works_count2022` associates with expected `Score` raised by `1.6875e-04 *100 *1000 =16.9`.
-  A tenth of a unit (`0.1`) rise in per-faculty Amounts (in Indian Rs. crore) through Sponsored Research or Consultancy `SRCrevPerFaculty` associates with an expected `Score` rise of about `2.9483e-01 *100 *0.1 =2.9`. `SRagencies` impacts `6.4706e-04 *100`; `SRamount` impacts `-9.2171e-04 *100`. `Cprojects` impacts `1.0727e-04 *100`; `Corgs` impacts `-2.2666e-04 *100`.
-  Though statistically less significant: `year2022` has `1.3289e-02 *100 =1.3` impact, i.e., rise vs. (baseline-reference) `year2021`. `categoryIITnew` associates with a `1.4999e-01 *100 =15` rise vs. (baseline-reference) `categoryIIT5` (first-five IITs); `categoryNIT` too similarly.

The above understanding of the `Estimate` of chosen statistical model's coefficients refers to the following code-session extract and while holding the other explanatory variables at mean (or baseline-reference) values:

```{r}
TBD
```

The corresponding (updated) Jupyter Notebook with `R` code and session output is at [`inNIRF.ipynb`](./worldClass/inNIRF-2022Sept03-1424.ipynb). The interested reader may extend this project, e.g., by properly reducing the factors that institutions need to consider for raising `Score`.
