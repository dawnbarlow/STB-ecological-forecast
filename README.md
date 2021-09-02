# STB-ecological-forecast

This repository contains code and data necessary to generate the analyses and figures associated with the following manuscript: 

Barlow, D. R., & Torres, L. G. (2021). Planning ahead: Dynamic models forecast blue whale distribution with applications for spatial management. Journal of Applied Ecology, https://doi.org/10.1111/1365-2664.13992 


# Abstract: 
1. Resources in the ocean are ephemeral, and effective management must therefore account for the dynamic spatial and temporal patterns of ecosystems and species of concern. We focus on the South Taranaki Bight (STB) of New Zealand, where upwelling generates productivity and prey to support an important foraging ground for blue whales that overlaps with anthropogenic pressure from industrial activities.
2. We incorporate regional ecological knowledge of upwelling dynamics, physical–biological coupling and associated lags in models to forecast sea surface temperature (SST) and net primary productivity (NPP) with up to 3 weeks lead time. Forecasted environmental layers are then implemented in species distribution models to predict suitable blue whale habitat in the STB. Models were calibrated using data from the austral summers of 2009–2019, and ecological forecast skill was evaluated by predicting to withheld data.
3. Boosted regression tree models skilfully forecasted SST (CV deviance explained = 0.969–0.970) and NPP (CV deviance explained = 0.738–0.824). The subsequent blue whale distribution forecast models had high predictive performance (AUC = 0.889), effectively forecasting suitable habitat on a daily scale with 1–3 weeks lead time.
4. The spatial location and extent of forecasted blue whale habitat were variable, with the proportion of petroleum and mineral permit areas that overlapped with daily suitable habitat ranging from 0% to 70%. Hence, the STB and these forecast models are well-suited for dynamic management that could reduce anthropogenic threats to whales while decreasing regulatory burdens to industry users relative to a traditional static protected area.
5. Synthesis and applications. We develop and test ecological forecast models that predict sea surface temperature, net primary productivity and blue whale suitable habitat up to 3 weeks in the future within New Zealand's South Taranaki Bight region. These forecasts of whale distribution can be effectively applied for dynamic spatial management due to model foundation on quantified links and lags between physical forcing and biological responses. A framework to operationalize these forecasts through a user-driven
application is in development to proactively inform conservation management decisions. This framework is implemented through stakeholder engagement, allows flexibility based on management objectives, and is amenable to improvement as new knowledge and feedback are received.

# In this repository: 

- Data: This folder contains data files needed to run scripts and produce plots.
- Models: Boosted regression tree models generated in this study and used to produce resulting plots are stored in this folder as .rds files. 
- Scripts: This folder contains the R scripts used for data processing, analysis, and visualization. 
