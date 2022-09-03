# Parsimonious Deeper Learning with Research Data from Top 20 (2022-Sept-03)
Building on earlier analyses, here this [author](mailto:yadevinit@gmail.com) attempts parsimonious regression by properly reducing the factors that institutions need to consider for raising `Score`. This uses [PLSR and PCR](http://www.science.smith.edu/~jcrouser/SDS293/labs/lab11-r.html): Partial Least Squares (or Projection onto Latent Structures) Regression and Principal Components Regression. Here are the highlights:
-  Response `Score` and over 25 variables have been standardized so that their (unit) impacts can be numerically compared. About 90% of their variance is explained by "latent" components, as per the chosen count of 5 components.
-  The following code-session extracts shrink the component-wise variables to show only those with greater impact from their `loadings`. This is for readably assisting institutional stakeholders re-orient their strategies considering expected impact of (latent components of) variables. For example:
   +    PLSR-identified `Comp 1` clubs Financial-Resources Operating expenses `FROcost` and amounts from Sponsored-Research (and Consultancy-Projects) `SRCrevenue`.
   +    TBD.

The corresponding (updated) Jupyter Notebook with `R` code and complete session output is at [`inNIRF.ipynb`](./worldClass/inNIRF-2022Sept03-1811.ipynb).

## PLSR
```{r}

```

## PCR
```{r}

```
