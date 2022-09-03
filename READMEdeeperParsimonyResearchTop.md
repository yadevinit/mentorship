# Parsimonious Deeper Learning with Research Data from Top 20 (2022-Sept-03)
Building on earlier analyses, this [author](mailto:yadevinit@gmail.com) here attempts parsimonious regression by properly reducing the factors that institutions need to consider for raising `Score`. This uses [PLSR and PCR](http://www.science.smith.edu/~jcrouser/SDS293/labs/lab11-r.html): Partial Least Squares (or Projection onto Latent Structures) Regression and Principal Components Regression. Here are the highlights:
-  Response `Score` and over 25 variables have been standardized so that their (unit) impacts can be numerically compared. About 90% of their variance is explained by "latent" components, as per the chosen count of 5 components.
-  Under the headings ahead, the code-session extracts shrink the component-wise variables to show only those with greater impact from their `loadings`. This is for readably assisting institutional stakeholders to re-orient their strategies considering expected impact of (latent components of) variables. For example:
   +    PLSR-identified component `Comp 1` clubs Financial-Resources Operating expenses `FROcost` and amounts from Sponsored-Research (and Consultancy) projects `SRCrevenue`. This is similar to PCR-identified `Comp 1`.
   +    `Comp 2` shows negative `loadings` impact from count of students `UGactualStudents` (undergraduate as well as `UGPGPhD`) as well as `faculty` on `Score`. This shows up through both PLSR and PCR. PLS shows adverse impacts of Consultancy projects `Cprojects` and Organizations `Corg` too, though not in PCR's `Comp 2`.
   +    `Comp 3` is positively impacted via `faculty`. TBD.
   +    PCR-identified `Comp 5` negatively clubs (a) count of Works per faculty `works2022perFaculty`---"publication productivity" across 3 years lagged by 2 years---with (b) the count of publications `works_count2021` (and `works_count2022`, across all faculty for each given institution). Their negative `loadings` indicate their adverse impact on `Score`. In contrast within the same component, unit rise in Citations per Work `TCperWork2021` (and `TCperWork2022`) positively impacts `Score`, just as in PLSR-identified `Comp 5`.

The code sessions have been extracted from an (updated) Jupyter Notebook with `R` code which is at [`inNIRF.ipynb`](./worldClass/inNIRF-2022Sept03-1811.ipynb) with complete session output. Regarding [Works](https://docs.openalex.org/):

>    Works are papers, books, datasets, etc; they *cite* other works.

## PLSR Extract
![](./worldClass/PLSRcomps.png)

## PCR Extract
![](./worldClass/PCRcomps.png)
