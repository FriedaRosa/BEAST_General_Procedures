| Hypothesis     | Name                          | calculated for | #Predictor | Predictor                                                 | Reasoning                                                                                                                                                                                                                                | Reference                                                                                                                                                                                                                                                                                                                                                                                                                                 | Source                                                                                                    | Status | Note                                       |
| -------------- | ----------------------------- | -------------- | ---------- | --------------------------------------------------------- | ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | --------------------------------------------------------------------------------------------------------- | ------ | ------------------------------------------ |
| H1             | Geometry                      | Species        | 1          | Scale-Area                                                | Spatial structure (pattern of aggregation) of a species distribution can be fully determined by population's temporal rates of change (colonization and extinction) and conversely predict temporal change                               | Kunin 1989, Wilson et al., 2004, Hui 2011                                                                                                                                                                                                                                                                                                                                                                                                 | Calculated from Atlas as: Dji = 2-2x bij from the Area ~ Scale relationship                               | done   |                                            |
|                |                               |                | 2          | Autocorrelation                                           |                                                                                                                                                                                                                                          |                                                                                                                                                                                                                                                                                                                                                                                                                                           | Calculated from Atlas data using global Moran's I                                                         | done   |                                            |
|                |                               |                | 3          | Relative Occupancy                                        | More widespread species tend to expand, while restricted species are more subject to stochastic extinction events. Relative occupancy to utilize comparison between different regions of the world                                       |                                                                                                                                                                                                                                                                                                                                                                                                                                           | Calculated from Atlas data: as relative AOO                                                               | done   |                                            |
|                |                               |                | 4          | Shape of the Distribution                                 |                                                                                                                                                                                                                                          |                                                                                                                                                                                                                                                                                                                                                                                                                                           | Calculated from Atlas data                                                                                | done   |                                            |
|                |                               |                | 5          | Southernness / Westernness (centroid)                     | For CZ: indicator for having invasives                                                                                                                                                                                                   |                                                                                                                                                                                                                                                                                                                                                                                                                                           | Calculated from Atlas as: classification of population centroid to N, S, E, W                             | done   |                                            |
|                |                               |                | 6          | Distance from Border (centroid)                           | Species closer to the border may potentially have a truncated range inside the 'arena' if there is no ecological barrier matching the administrative country borders which may impact the temporal change trend                          |                                                                                                                                                                                                                                                                                                                                                                                                                                           | Calculated from Atlas data population centroids                                                           | done   |                                            |
|                |                               |                | 7          | Distance from center of gravity of all species (centroid) | Distance from the center of gravity of all populations in an arena may indicate less interspecific competition                                                                                                                           |                                                                                                                                                                                                                                                                                                                                                                                                                                           | Calculated from Atlas data population centroids                                                           | done   |                                            |
|                | Geometry                      | Arena          | 8          | Grain                                                     | Scale affects population dynamics                                                                                                                                                                                                        |                                                                                                                                                                                                                                                                                                                                                                                                                                           | Each Atlas has a different grain as original resolution                                                   | done   |                                            |
|                |                               |                | 9          | Extent                                                    | Area affects Species Richness and Edge-effects                                                                                                                                                                                           |                                                                                                                                                                                                                                                                                                                                                                                                                                           | Each Atlas has a different extent                                                                         | done   |                                            |
|                |                               |                | 10         | Shape                                                     | an North-South elongated Arena may provide more opportunity for species to move north with climate change than a East-West elongated arena                                                                                               |                                                                                                                                                                                                                                                                                                                                                                                                                                           | Calculated from Atlas data                                                                                | done   |                                            |
| H2             | Traits                        | Species        | 11         | Hand-Wing-Index                                           | As an index for dispersal ability through flight intensity.                                                                                                                                                                              | "HWI as 100 × Kipp's distance/wing length, where Kipp's distance is the length of the distance between the tip of the first secondary feather and the tip of the longest primary feather on a folded wing, and wing length is the distance from the carpal joint to the tip of the longest primary feather (Claramunt & Wright, [2017](https://besjournals.onlinelibrary.wiley.com/doi/full/10.1111/1365-2435.14056#fec14056-bib-0008))." | Taken from AVONET                                                                                         | done   |                                            |
|                |                               |                | 12         | Phylogenetic Distinctness                                 | Evolutionary distinct species tend to be more specialized than evolutionary common species; also evolutionary distinct species may be less able to adapt to contemporary climate change                                                  |                                                                                                                                                                                                                                                                                                                                                                                                                                           | Calculated from BirdTree (Jetz et al., 2012) using Fair Proporion metric                                  | done   |                                            |
|                |                               |                | 13         | Genus/Family/Order                                        | As an indicator whether the population trend may be related to a specific clade                                                                                                                                                          |                                                                                                                                                                                                                                                                                                                                                                                                                                           | From Bird Taxonomy                                                                                        | done   |                                            |
|                |                               |                | 14         | Climate Niche (PCA1,PCA2)                                 | Species with wider climatic niches will be less subject to extinction, thereby increasing the colonization to extinction ratio and hence they tend to expand their range instead of decline                                              |                                                                                                                                                                                                                                                                                                                                                                                                                                           | Calculated from CHELSA PCAs (reduced to breeding bird season) as the variance within PCA for each species | done   |                                            |
|                |                               |                | 15         | Life Strategy                                             |                                                                                                                                                                                                                                          | Storchová, Lenka; Hořák, David (2018). Data from: Life-history characteristics of European birds [Dataset]. Dryad. https://doi.org/10.5061/dryad.n6k3n                                                                                                                                                                                                                                                                                    | Taken from Stochová et al. 2018<br><br>Or from AVONET                                                     | done   |                                            |
|                |                               |                | 16         | Migratory Status                                          | Species that migrate may have rescue populations in more places in the world than those that do not migrate and will thus tend to change less (?)                                                                                        |                                                                                                                                                                                                                                                                                                                                                                                                                                           | Taken from Atlas data                                                                                     | done   |                                            |
|                |                               |                | 17         | Threat status (national, continental, global)             | Threatened species should have a declining trend (different scales tested here)                                                                                                                                                          |                                                                                                                                                                                                                                                                                                                                                                                                                                           | Taken from National Red Lists, Continental Red Lists, IUCN Global red lists                               |        | For CZ it's done                           |
|                |                               |                | 18         | Trophic strategy                                          | Insectivorous or not - those that rely on insects may be stronger affected by temporal change due to loss of insects with global change                                                                                                  |                                                                                                                                                                                                                                                                                                                                                                                                                                           | Taken from Storch et al., 2023<br><br>OR from AVONET                                                      | done   |                                            |
|                |                               |                | 19         | Human Association                                         | Species more associated with humans may benefit from food sources that are not available for wild species and thuse, human associated species tend to change less                                                                        |                                                                                                                                                                                                                                                                                                                                                                                                                                           |                                                                                                           |        |                                            |
|                |                               |                | 20         | Global range size                                         | Again: More widespread species tend to expand, while restricted species are more subject to stochastic extinction events. (global scale)                                                                                                 |                                                                                                                                                                                                                                                                                                                                                                                                                                           | Taken from Weeks et al. 2022 // AVONET                                                                    | done   |                                            |
| H3             | Patterns of species diversity | Arena          | 21         | total Species Richness                                    | Whether species will decline or expand across an Arena depends on the interactions between species and can thus be related to total species richness of the Arena                                                                        |                                                                                                                                                                                                                                                                                                                                                                                                                                           | Calculated from Atlas data                                                                                | done   |                                            |
|                |                               |                | 22         | mean SR of individual cells                               | Whether species will colonize or go extinct across an specific cells across the Arena depends on the interactions between species and can thus be related to mean species richness of the cells in which a specific species can be found |                                                                                                                                                                                                                                                                                                                                                                                                                                           | Calculated from Atlas data                                                                                | done   |                                            |
|                |                               |                | 23         | Whittakers beta diversity between cells                   |                                                                                                                                                                                                                                          |                                                                                                                                                                                                                                                                                                                                                                                                                                           | Calculated from Atlas data                                                                                | done   |                                            |
|                |                               |                | 24         | Co-occurrence                                             | Species with negative co-occurrence probability will tend to decline while those with positive co-occurrence probability will tend to expand                                                                                             |                                                                                                                                                                                                                                                                                                                                                                                                                                           | Calculated from Atlas data using cooccur R package                                                        | done   |                                            |
|                |                               |                | 25         | Site composition (beta diversity)                         |                                                                                                                                                                                                                                          |                                                                                                                                                                                                                                                                                                                                                                                                                                           | Calculated from Atlas data as gamma_atlas / alpha_sp                                                      | done   |                                            |
| H4             | Environmental                 | Species ?      | 26         | Land Cover                                                | being a generalist or a specialist towards specific land use types can impact a species tendency to decline or expand                                                                                                                    |                                                                                                                                                                                                                                                                                                                                                                                                                                           |                                                                                                           |        | I have the data but it's not processed yet |
|                |                               |                | 27         | Land Use change                                           | Being subject to land use change will potentially drive higher extinction rates                                                                                                                                                          |                                                                                                                                                                                                                                                                                                                                                                                                                                           |                                                                                                           |        | I have the data but it's not processed yet |
|                |                               |                | 28         | Topology/Elevation                                        | Fractal dimension of this?                                                                                                                                                                                                               |                                                                                                                                                                                                                                                                                                                                                                                                                                           |                                                                                                           |        | I have the data but it's not processed yet |
| H5             | Outside of the arena          | Arena          | 29         | Occurrence of species in surrounding area                 | If the species is present in areas surrounding the 'arena', metapopulation source-sink processes could lead to no change effects on those species                                                                                        |                                                                                                                                                                                                                                                                                                                                                                                                                                           | Calculated from BirdLife international global range maps OR <br>IUCN Range maps                           |        |                                            |
|                |                               |                | 30         | Species Richness in surrounding area                      |                                                                                                                                                                                                                                          |                                                                                                                                                                                                                                                                                                                                                                                                                                           | from BirdLife international range maps                                                                    |        |                                            |
|                |                               |                | 31         | Land/Sea/Both surrounding                                 | Determines isolation of the Arena and thus determines the species pool in general                                                                                                                                                        |                                                                                                                                                                                                                                                                                                                                                                                                                                           | Assigned from Atlas                                                                                       |        |                                            |
| Sensitivity H6 | Biases in Atlas data          | Species        | 32         | mean sampling effort                                      | Should not impact the true trajectory of the species but may be useful for predicting change if sampling effort differs across datasets                                                                                                  |                                                                                                                                                                                                                                                                                                                                                                                                                                           | Calculated from Atlas data (not for Japan because we have no measure of sampling effort)                  | done   |                                            |
|                |                               |                | 33         | mean irregularity of shape                                | smaller cells can harbour less species and species that tend to occurr in smaller cells may tend to decline rather than expand                                                                                                           |                                                                                                                                                                                                                                                                                                                                                                                                                                           | Calculated from Atlas data as mean(length) and mean(width) of each cell in which the species occurrs      | done   | Petr is not a fan of it here               |