# Parsimonious Deeper Learning with Research Data from Top 20 (2022-Sept-03)
Building on earlier analyses, here this [author](mailto:yadevinit@gmail.com) attempts parsimonious regression by properly reducing the factors that institutions need to consider for raising `Score`. This uses [PLSR and PCR](http://www.science.smith.edu/~jcrouser/SDS293/labs/lab11-r.html): Partial Least Squares (or Projection onto Latent Structures) Regression and Principal Components Regression. Here are the highlights:
-  Response `Score` and over 25 variables have been standardized so that their (unit) impacts can be numerically compared. About 90% of their variance is explained by "latent" components, as per the chosen count of 5 components.
-  The following code-session extracts shrink the component-wise variables to show only those with greater impact from their `loadings`. This is for readably assisting institutional stakeholders re-orient their strategies considering expected impact of (latent components of) variables. For example:
   +    PLSR-identified `Comp 1` clubs Financial-Resources Operating expenses `FROcost` and amounts from Sponsored-Research (and Consultancy-Projects) `SRCrevenue`.
   +    TBD.

The corresponding (updated) Jupyter Notebook with `R` code and complete session output is at [`inNIRF.ipynb`](./worldClass/inNIRF-2022Sept03-1424.ipynb).

## PLSR
```{r}
Comp 1	Comp 2	Comp 3	Comp 4	Comp 5
SRamount	UGactualStudents	PGactualStudents	PGactualStudents	TC2021
FROcost	faculty	faculty	Cprojects	TCperWork2021
SRCrevenue	UGPGPhD	UGPGPhD	Corgs	TCperWork2022

Comp 1	Comp 2	Comp 3	Comp 4	Comp 5
UGactualStudents	0.034	-0.413	-0.070	-0.033	0.027
PGactualStudents	0.132	-0.319	0.225	0.073	0.052
SRamount	0.260	0.124	-0.163	0.068	0.102
Cprojects	0.108	-0.002	-0.175	-0.572	-0.170
Corgs	0.123	0.019	-0.160	-0.553	-0.186
faculty	0.104	-0.398	0.013	0.020	0.033
UGPGPhD	0.088	-0.413	0.016	-0.004	0.031
FROcost	0.271	-0.072	-0.103	0.096	0.163
SRCrevenue	0.262	0.117	-0.189	0.023	0.101
TC2021	0.252	-0.002	0.264	-0.090	-0.126
TCperWork2021	0.083	0.136	0.392	-0.297	0.294
TCperWork2022	0.075	0.136	0.394	-0.300	0.287
```

## PCR
```{r}
Comp 1	Comp 2	Comp 3	Comp 4	Comp 5
FROsalaries	UGactualStudents	propPGPhD	Cprojects	works_count2021
FROcost	faculty	TCperWork2021	Corgs	works_count2022
SRCrevenue	UGPGPhD	TCperWork2022	TCperWork2022	works2022perFaculty

	Comp 1	Comp 2	Comp 3	Comp 4	Comp 5
UGactualStudents	0.034	-0.413	-0.070	-0.033	0.027
FROsalaries	0.267	-0.114	-0.139	-0.019	0.124
Cprojects	0.108	-0.002	-0.175	-0.572	-0.170
Corgs	0.123	0.019	-0.160	-0.553	-0.186
faculty	0.104	-0.398	0.013	0.020	0.033
UGPGPhD	0.088	-0.413	0.016	-0.004	0.031
propPGPhD	0.124	0.193	0.332	0.147	0.111
FROcost	0.271	-0.072	-0.103	0.096	0.163
SRCrevenue	0.262	0.117	-0.189	0.023	0.101
works_count2021	0.246	-0.147	0.097	0.043	-0.300
works_count2022	0.247	-0.153	0.085	0.065	-0.295
TCperWork2021	0.083	0.136	0.392	-0.297	0.294
TCperWork2022	0.075	0.136	0.394	-0.300	0.287
works2022perFaculty	0.049	0.196	0.092	0.091	-0.610
```
