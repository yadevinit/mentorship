# mentorship
## Migraine Diary Bayesian
Refer [link](./sevDura/sevDura.md).

## TSP in Gujarat
TiPSy? No, that's Traveling Salesperson Problem (TSP) being solved for a [custom Gujarat tour](https://docs.google.com/document/d/1xm_VhIrW8I7KPI3jKdEJNCF0MEOzEFXrFTe42NRU6yk/edit?usp=sharing)---refer heading Option#7 for detailed itinerary (with Google Map routes)---to see what route could have been more optimal. Here's a preliminary [R-code Jupyter Notebook](./TSPinGujarat/TSPinGujarat.ipynb) (with session output appended with "takeaway" inferences) for that. It approximates "cost" as spatial distances between map coordinates of the spots (graph nodes or vertices) visited, including access airports. What's possible next from this combinatorial-optimization analysis is to deal with an issue:

> Time and money "cost" are not considered. Presently, some tour legs (edges in graph) are partly over water and land. Tour (time) duration can be reduced---to fit a travellers' constraint---by choosing flights, whose money "cost" differs from road travel. Flight (availability and) timings and money differ based on choice of airport(s), e.g., there are even more airports in Gujarat (and rest of India such as Chennai's) which are not included in [data](./TSPinGujarat/INGUJ50_GPS.csv). (Prices could surge as well, e.g., considering holidays.)
    

## Deeper Learning from Top 20 (for Higher-Education Institutions)
[NIRF2022](https://www.nirfindia.org/2022/EngineeringRanking.html) ranks India's Engineering institutions. Beyond the ranks, what can that data teach us? This [author](mailto:yadevinit@gmail.com) attempts an answer by (a) manually compiling recent years' data for the top-20 institutions, (b) joining that with institution-wise research-performance data, and (c) statistically modeling that using `Score` as response variable. (To quickly recall, the highest `Score` between `0` and `100` is what's ranked `1`.) For use by institutional stakeholders, the following sub-projects---with recent ones listed first---create various statistical models and help understand the `Estimate` of chosen statistical model's coefficients, which refer to code-session extracts included within:
-  [Parsimonious Deeper Learning with Research Data from Top 20 (2022-Sept-05)](./READMEdeeperParsimonyResearchTop.md).
-  [Deeper Learning with Research Data from Top 20 (2022-Sept-01)](./READMEdeeperResearchTop.md). It uses [Research Organization Registry](https://ror.org/) identifier `ror` as key to join earlier-fetched research-performance data with NIRF data.
-  [Fetching Research Data](./worldClass/researchLio-2022Sept01-1041.ipynb) on publications and citations after news that [Massive open index of scholarly papers launches](https://www.nature.com/articles/d41586-022-00138-y) [`OpenAlex`](https://openalex.org/):
    >    a replacement for Microsoft Academic Graph (MAG), a free alternative to subscription-based platforms such as Scopus, Dimensions and Web of Science.
-  [Deeper Learning from Top 20 (2022-Aug)](./READMEdeeperTop.md).

For NIRF details, the reader may refer to [Summary of Ranking Parameters and Weightages](https://www.nirfindia.org/nirfpdfcdn/2022/framework/Engineering.pdf).


## Survival Analyses and Optimization (for Student Learners)
Refer [link](./READMEsurvOpt.md).
