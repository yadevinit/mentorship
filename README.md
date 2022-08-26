# mentorship
## Deeper Learning from Top 20 (for Higher-Education Institutions)
[NIRF2022](https://www.nirfindia.org/2022/EngineeringRanking.html) ranks India's Engineering institutions. Beyond the ranks, what can that data teach us? This [author](mailto:yadevinit@gmail.com) attempts an answer by (a) manually compiling recent years' data for the top-20 institutions and (b) statistically modeling that using `Score` as response variable. (To quickly recall, the highest `Score` between `0` and `100` is what's ranked `1`.) For use by institutional stakeholders, here is an understanding of the `Estimate` of a chosen statistical model's coefficients, which refers to the code-session extract included further ahead:
-  While holding the explanatory variables at mean (or baseline) values, the expected mean `Score` (response variable) is around `6.4693e-01 *100 =64.7`.
-  As per this model, it would be unacceptably erroneous to conclude that explanatory variables such as `year` or `category` of institutions (e.g., whether `NIT` or the first-five IITs `IIT5`) have significant impact on `Score` (response).
-  Unit rise in count of Consultancy Projects `Cprojects` is associated with around `-1.2678e-04 *100` impact on `Score`. In contrast to that adverse impact, unit rise in count of Consultancy Organizations `Corgs` associates with `1.9670e-04 *100` impact. So, more (Consultancy) Organizations with fewer Projects may be preferred, unless institutions make any (`Cprojects`-related) adverse-impact linkages (on `Score`) disappear. Unit rise in count of Sponsored Research Projects `SRprojects` too shows an adverse impact of about `-1.1249e-04 *100`; so, fewer of those projects, and bigger ones instead, may be preferred.
-  A tenth of a unit (`0.1`) rise in proportion of actual postgraduate students and those pursuing PhD `propPGPhD` associates with a `-2.5978e-01 *100 *0.1` impact, i.e., adverse drop of `Score` by about `2.5978`. In contrast, a tenth of a unit (`0.1`) rise in per-faculty Amounts (in Indian Rs. crore) through Sponsored Research or Consultancy `SRCrevPerFaculty` associates with an expected `Score` rise of about `3.5872e-01 *100 *0.1 =3.5872`.

The above understanding of the `Estimate` of chosen statistical model's coefficients refers to the following code-session extract and while holding the other explanatory variables at mean (or baseline) values:

```r
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
The corresponding Jupyter Notebook with `R` code and session output is at [`inNIRF.ipynb`](./worldClass/inNIRF-2022Aug26-1518.ipynb). For NIRF details, the reader may refer to [Summary of Ranking Parameters and Weightages](https://www.nirfindia.org/nirfpdfcdn/2022/framework/Engineering.pdf).


## Survival Analyses and Optimization (for Student Learners)
The [author](mailto:yadevinit@gmail.com) initiated this while being an Alumni Mentor for undergraduate students with [IIT (BHU) Varanasi, India](https://saic.iitbhu.ac.in/) in 2022. What is shared here for all is as a record of real work done and to offer curated learning pathways of staged complexity, specifically: (a) a Mentee who had done "dashboard analytics" and never did such ("data science") analyses overcame that "barrier" and completed the work in couple of weeks and (b) another Mentee who had done Deep Learning and competitive programming (and given up on cancer prognostication) took on outperforming a Nature research publication (of 2019); he completed an analysis using a "shallower" Machine Learning, got renewed breadth and depth of the related biomedical domain, and learnt from peers sharing their "shallower" statistical analyses on the same or similar datasets. Personally for this author as Mentor, his father-in-law passed away just as interaction began with the first Mentee, and his dog passed away few months later just after concluding interaction with the same Mentee; such coincidences in the wake of the COVID-19 epidemic could serve as a reminder of how we have *survived* all along and how precious life is, which we sometimes take for granted. The following two distinct analyses also got integrated as the Mentees and Mentor observed that optimization gets used in Survival and other types of analyses.

-  Survival analyses with cancer data:
    *  [Surviving pancreatic cancer] has a Student Mentee's contribution. [Survival analysis of lung cancer](https://github.com/AjayKumarRedu/Survival-Analysis#survival-analysis) has another Student Mentee's contribution. Also inspired by yet another Student Mentee, this author adapted [Cox-PH and DeepSurv and possible extensions](./cox-ph-havakvPycoxExamplesDeepsurv-2022Jun16-1025.ipynb).
    *  [Surviving pancreatic cancer] uses a [survival dataset] generated by this author by developing a Jupyter Notebook with `R` code [`surviveMetastasis.ipynb`](./surviveMetastasis-2022Apr28-1229.ipynb) that cleaned data (and made it more usable) from the [Computational Modeling of Pancreatic Cancer Reveals Kinetics of Metastasis Suggesting Optimum Treatment Strategies](https://doi.org/10.1016/j.cell.2011.11.060) as shown in [session output of data cleaning and preliminary exploratory analysis](./surviveMetastasis-2022Apr28-1230.pdf). (The [survival dataset] is accessibly provided here in the spirit of mentorship while duly acknowledging the source.)

-  Optimization with COVID-19 data:
    *  [Optimization with COVID-19 data](https://github.com/AjayKumarRedu/Optimization-with-covid-19-data#optimization-with-covid-19-data) is contributed by a Student Mentee.
    *  That used an [optimization dataset] generated by this author. Here's more on this author's contribution:
        +  [optimal weights] ranging from `0` to `1` are what are allocated by various solvers (column wise in the table); for each row, the weight multiplied by total vaccines produced is what's optimal for allocating to the corresponding USA county (whose unique 5-digit FIPS Code is in 1st column). These [optimal weights] were generated by running a Jupyter Notebook for optimization using `R` kernel [`optVacciNation.ipynb`](./optVacciNation-2022Jun16.ipynb); that used [optimization dataset] as shown in similar session output [`optVacciNation.pdf`](./optVacciNation-2022May22-1303.pdf).  The "unexpected" negative weights, e.g., `-1.8388896451189e-10`, are likely due to arithmetic precision and could be treated as (indistinguishable from) `0`.
        +  In turn, that [optimization dataset] was got by running [`optVaccine.ipynb`] (with URLs for about 700MB of data sources not included here) as shown in session output [`optVaccine.pdf`](./optVaccine-2022May16-1036.pdf).  Along with Objective to be minimized, it also constrained each county's weight to be `<= 0.1`.  Towards the end of the session, you can see similarities and differences in outputs from the various solvers.  In the session, you can also see the optimization problem initially specified via functions and later specified in simpler-to-solve linear-programming algebraic form.  There is a "TBD" note at the end of [`optVaccine.ipynb`] listing what could be done further.  Solver output in binary `op2lp.sol0.1.rds` (non text) has the output solutions from the solvers; if you wish to analyze it programmatically, read it into an `R` cell of a Jupyter Notebook (and then use Python or any supported language) using the following code:

            >    `myrds <- readRDS(file="op2lp.sol0.1.rds")`

[survival dataset]:<./mmc1tabS1a-202204281228.csv>
[optimization dataset]:<./myData.tVaccMi.csv>
[optimal weights]:<./op2lp.sol.list.df.csv>
[`optVaccine.ipynb`]:<./optVaccine.ipynb>
[Surviving pancreatic cancer]:<https://github.com/Anchaliya75/Pancreatic-Cancer-Research-Paper-Implementation#pancreatic-cancer>
