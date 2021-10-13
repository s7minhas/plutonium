---
  title: "JWac Paper Status"
output: html_document
---
  
  ```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## Basic Paper Outline
Section 1: Intro
  -Basic question: Does US distraction due to wars, particularly in the Middle East lead to adverse outcomes in the rest of the world.
  -Contributions:
    1) Novel and superior measure of state affinity/preferences/closeness
    2) Novel measure of US distraction
    3) Findings about how US distraction leads states to cozy up/distance themselves from China
  -Can use US withdrawal from Afghanistan and implications for US/China competition as an easy hook
  -Understand that requires two things, neither of which has really gotten traction in IR (lit reviewy stuff)
    -Measure of US distraction
    -Measure of state preferences
Section 2: Our stylized argument/hypotheses etc.
  -Two opposing arguments
    -1) Limited resources/attention, distraction allows for trouble elsewhere
    -2) Credibility/resolve -- US doing costly things in Middle East should cause us to update (positively) that theyre willing to do costly things elsewhere
Section 3: Data and Methods
  -Measuring Distraction
  -Measuring State preferences
Section 4: Downstream stuff
  -Closeness to China as a DV, main findings support argument 2
  -Notably heterogeneous effect -- as US becomes more distracted, it seems like most states move away from China, but Chinas neighbors do the opposite
Section 5: Highlight cool results


##Our Major Measures in More Detail
1) Measure of state affinity.
-Drawn from latent factor model.
-Basic idea of latent factor model is a model that can account for 3 levels of network dependency
  -1st order deps -- certain actors are more/less popular than others in all their interactions
  -2nd order deps -- reciprocality is a thing
  -3rd order deps -- homophily and stochastic equivalence (the two that most network models arent able to get at)
-Gets at 3rd order dependencies using "multiplicative effect" that relies on singular value decomposition.
-So we run LFM using a set of economic relations as our dependent variable, and extract this multiplicative effect, as our main measure of state affinity.
-Higher values of it are associated with states being more likely to engage in these types of economic cooperation, lower values less likely
-We then difference it to get the year to year change in state affinity.
2) Measure of US distraction.
-Take data on US military casualties and mideast commitments, and run a PCA
-Alternative measure based on foreign troop commitments and US defense spending

##Model Setup
Unit of observation: country year from 1992-2019
DV: Change in our affinity measure for the country to China
Key IV: lagged measure of US distraction
Controls: Polity, Population, Distance to Beijing, Bailey/Voeten/Strezhnev Ideal Point Distance, GDP
Random Effects: By region (using UNs 23 regions)


##Results


## Heterogeneous Effecrts
## Outstanding Issues