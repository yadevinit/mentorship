# mentorship
The [author](mailto:yadevinit@gmail.com) initiated this while being an Alumni Mentor for undergraduate students with [IIT (BHU) Varanasi, India](https://saic.iitbhu.ac.in/) in 2022.

-  Survival analysis with cancer data:
    *  [Surviving pancreatic cancer](https://github.com/Anchaliya75/Pancreatic-Cancer-Research-Paper-Implementation#pancreatic-cancer) has a Student Mentee's contribution.
    *  TBD other Student Mentees' contributions.
    *  These use a [survival dataset] generated by this author by developing a Jupyter Notebook with `R` code [`surviveMetastasis.ipynb`](./surviveMetastasis-2022Apr28-1229.ipynb) that cleaned data (and made it more usable) from the [Computational Modeling of Pancreatic Cancer Reveals Kinetics of Metastasis Suggesting Optimum Treatment Strategies](https://doi.org/10.1016/j.cell.2011.11.060) as shown in [session output of data cleaning and preliminary exploratory analysis](./surviveMetastasis-2022Apr28-1230.pdf). (The [survival dataset] is accessibly provided here in the spirit of mentorship while duly acknowledging the source.)

-  Optimization with COVID-19 data:
    *  TBD contribution from a Student Mentee.
    *  That used an [optimization dataset] generated by this author. Here's more on this author's contribution:
        +  [optimal weights] ranging from `0` to `1` are what are allocated by various solvers (column wise in the table); for each row, the weight multiplied by total vaccines produced is what's optimal for allocating to the corresponding USA county (whose unique 5-digit FIPS Code is in 1st column). These [optimal weights] were generated by running a Jupyter Notebook for optimization using `R` kernel [`optVacciNation.ipynb`](./optVacciNation.ipynb); that used [optimization dataset] as shown in session output [`optVacciNation.pdf`](./optVacciNation-2022May22-1303.pdf).  The "unexpected" negative weights, e.g., `-1.8388896451189e-10`, are likely due to arithmetic precision and could be treated as (indistinguishable from) `0`.
        +  In turn, that [optimization dataset] was got by running [`optVaccine.ipynb`] (with URLs for about 700MB of data sources not included here) as shown in session output [`optVaccine.pdf`](./optVaccine-2022May16-1036.pdf).  Along with Objective to be minimized, it also constrained each county's weight to be `<= 0.1`.  Towards the end of the session, you can see similarities and differences in outputs from the various solvers.  In the session, you can also see the optimization problem initially specified via functions and later specified in simpler-to-solve linear-programming algebraic form.  There is a "TBD" note at the end of [`optVaccine.ipynb`] listing what could be done further.  Solver output in binary `op2lp.sol0.1.rds` (non text) has the output solutions from the solvers; if you wish to analyze it programmatically, read it into an `R` cell of a Jupyter Notebook (and then use Python or any supported language) using the following code:

            >    `myrds <- readRDS(file="op2lp.sol0.1.rds")`

[survival dataset]:<./mmc1tabS1a-202204281228.csv>
[optimization dataset]:<./myData.tVaccMi.csv>
[optimal weights]:<./op2lp.sol.list.df.csv>
[`optVaccine.ipynb`]:<./optVaccine.ipynb>
