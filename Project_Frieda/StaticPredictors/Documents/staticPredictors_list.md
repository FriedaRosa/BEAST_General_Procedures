---
tags:
  - PhD
  - BEAST
  - StaticPredictors
title: Predictors
related notes: "[[Predicting temporal change from static patterns]]"
---
| Predictor group | Predictor sub-group | Predictor | Source | Status | Note |
| ---- | ---- | ---- | ---- | ---- | ---- |
| Species traits | Evol. constrain | Phylogenetic distinctness | calculated from Jetz 2012 BirdTree (taken from Weeks et al 2022 data repository) | - [x] | Fair proportion metric (Isaac et al., 2007). |
| Species traits |  | Species Genus, Family, Order | AVONET | - [...] | Genus not included so far |
|  | Threat Status | IUCN Global/Continental/National Red List for species | IUCN | - [...] | Don't have the national one yet [[Questions & Ideas]] |
|  | % Occurrence in threatened habitat OR classification of species | IUCN Red List of threatetend terrestrial and freshwater habitats in Europe OR Jiri Reif  Populations of farmland birds (Ref: Rosenberg 2019: introduced, landbird, shorebirds, waterbirds, waterfowl, agricultural) | IUCN | - [ ] | I don't have this data in a usable format. Maybe in combination with land cover data? [[Questions & Ideas]] |
|  | Dispersal trait | Hand-Wing-Index (Flight intensity), mIgratory status, trophic strategy.. check Paper by Stoch 2023  | AVONET | - [x] | from Weeks et al 2022 data repository |
|  | Scale-Area-Relationship | slope of lm(log(AOO)~log(scale)) <br>intercept of lm(log(AOO)~log(scale))  | calculated from CZ Atlas 1 (future change) and 2 (past change) | - [x] |  |
|  | Occupancy | AOO | calculated from CZ Atlas 1 (future change) and 2 (past change) | - [x] |  |
|  |  | Relative occupancy N cells -> this | calculated from CZ Atlas 1 (future change) and 2 (past change) | - [x] |  |
|  |  | Occupancy N cells | calculated from CZ Atlas 1 (future change) and 2 (past change) | - [x] |  |
|  | Geometry | orientation of distribution (n-s, e-w)? - center of gravity, how far it is away from the border - Sothernness * Arena Size // Westernness --> major pressure from invasion |  | - [ ] | discuss with P [[Questions & Ideas]] |
|  |  | some summary metric calculated from cell-properties for each species (area, shape, length, perimeter, sampling effort) |  | - [ ] | discuss with P [[Questions & Ideas]] |
|  | Statistical patterns | spational autocorrelation: Global for species [[Autocorrelation]] | from Carmen | - [ ] | talk to Carmen #Tasks  |
|  |  | regression-to-mean -> exclude and only look at Occupancy | Calculated from best sampled cells | - [ ] | as offset? #Tasks  |
|  | Environmental | Climate space and area ? -> Niche (local or global eBird) (continental) | e.g. from CHELSA | - [ ] | discuss with P [[Questions & Ideas]] |
|  |  | Topography/Elevation --> EU scale | from geodata R package | - [x] |  |
|  | Global Range Size | -> https://datazone.birdlife.org/home   | IUCN rangemaps for global; Range.Size from Weeks et al 2022 // https://datazone.birdlife.org/home | - [x] |  |
|  | Species Interactions | Co-occurrence probability | calculated from CZ atlas for inside CZ;<br>EU BBS for outside of CZ | - [x]<br>- [ ] | discuss with P [[Questions & Ideas]] |
|  |  |  |  | - [ ] |  |
|  |  |  |  | - [ ] |  |
| Characteristics of the 'arena'<br><br>(i.e., the area across which we study change)<br><br>--> we need at least 2 different Atlases together to make this a useful predictor |  | Extent |  | - [ ] |  |
|  |  | Perimeter |  | - [ ] |  |
|  | Geometry | Orientation/Shape ? - index of elongation |  | - [ ] |  |
|  |  | % of threatened species per country/'arena'  |  | - [ ] |  |
|  |  | % threatened habitats per country/'arena' |  | - [ ] |  |
|  | Biological | total Species Richness |  | - [ ] |  |
|  | Sampling in Atlas --- Biases of Atlasses | average local richness, beta diversity, equilibrium species richness, ratio = gamma |  | - [ ] |  |
![[MindMap Static Predictors.canvas|MindMap Static Predictors]]