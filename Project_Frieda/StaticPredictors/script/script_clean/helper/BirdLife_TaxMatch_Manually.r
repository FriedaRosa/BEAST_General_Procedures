## Script for 'quick & dirty' matching of species to data ##
## Duplicated species are not matched to a common taxonomy, but rather the same external data is matched to synonym species ##
## This keeps species names identical to those used in the original atlas data but enables adding external data to species ##



## Data

# The input file geodatabase from BirdLife International 2018
fgdb <-"c:/Users/wolke/OneDrive - CZU v Praze/Datasets/Raw/Distributions/Birds_of_the_world/BOTW.gdb" 



# Taxonomic data from BirdLife international
Tax <- st_read(fgdb, layer = "Taxonomic_checklist") %>% 
  dplyr::select(Order_, FamilyName, ScientificName, RL_Category, Synonyms) %>% 
  rename("Order" = "Order_") %>%
  mutate(Order = stringr::str_to_sentence(Order))
Tax[Tax == "<NA>"] = NA

# Atlas species names
spData <- read.csv("../../out/csv/SpeciesNamesData.csv") %>% pull(verbatim_name) # just a list of all species names from all atlases created somewhere else..


## Processing

# Reduce Taxonomic data from BirdLife 2018 to Species Names columns
Tax_red <- Tax %>%
    distinct(ScientificName, Synonyms) %>%
    rename("AtlasSp" = "ScientificName")
Tax_red$TaxID <- seq(1:nrow(Tax_red))
head(Tax_red)




# Quickly checking the matches..
sp_direct_match <- intersect(spData, Tax_red$AtlasSp) # 733
sp_syn_match <- intersect(spData, Tax_red$Synonyms) # 72


# Create Dataframe that will be filled for taxonomic matching
Atlas_sp <- data.frame(
    AtlasSp = spData,
    AtlasID = seq(1:length(spData)),
    match_type = NA,
    match = NA
) %>%
    group_by(AtlasSp) %>%
    mutate(
        match_type = case_when(
            AtlasSp %in% sp_direct_match ~ "direct",
            AtlasSp %in% sp_syn_match ~ "synonym",
            .default = NA
        )
    )




# =================================== Step 1: Direct Matches  ===================================== #

Atlas_merged <- left_join(Atlas_sp, Tax_red %>% filter(AtlasSp %in% sp_direct_match)) %>%
    group_by(AtlasSp) %>%
    mutate(
        match = case_when(is.na(TaxID) ~ 0, !is.na(TaxID) ~ 1),
        AtlasSp_new = case_when(match == 1 ~ AtlasSp, match == 0 ~ ""),
        ScientificName = case_when(match == 0 ~ NA, match == 1 ~ AtlasSp)
    )

Atlas_merged %>% filter(AtlasSp_new == "") %>% ungroup() %>% distinct(match_type)

Atlas_merged %>%
    filter(match == 1) %>%
    n_distinct() # 733 species with matching taxonomy. We can delete them from the matching process now

# Checks:
spData_unmmatched <- Atlas_merged %>%
    filter(match == 0) %>%
    pull(AtlasSp) # 108 species still missing
sp_matched <- Atlas_merged %>%
    filter(match == 1) %>%
    pull(AtlasSp)

syn_match <- intersect(spData_unmmatched, sp_syn_match) # of those 108, 67 species can be matched directly from the synonyms species

intersect(sp_syn_match, sp_matched) # 5 Species were matched but are also found in the synonyms column.

Atlas_merged %>% filter(AtlasSp %in% intersect(sp_syn_match, sp_matched))






# =================================== Step 2: Direct Synonyms =========================================== #
Atlas_synonyms <- Atlas_merged %>%
    select(AtlasID, AtlasSp, match_type, match) %>%
    filter(AtlasSp %in% syn_match)
Atlas_synonyms$Synonyms <- Atlas_synonyms$AtlasSp

Atlas_synonyms_merged <- left_join(Atlas_synonyms, Tax_red %>% filter(Synonyms %in% syn_match) %>% rename("ScientificName" = "AtlasSp")) %>%
    mutate(
        AtlasSp_new = ScientificName,
        match = 1
    )
head(Atlas_synonyms_merged)
head(Atlas_merged)
colSums(is.na(Atlas_synonyms_merged))
Atlas_synonyms_merged %>% filter(ScientificName == "")

Atlas_merged2 <- rbind(Atlas_merged %>% filter(!(AtlasSp %in% syn_match)), Atlas_synonyms_merged)

# Delete species from vector that have been matched already:
spData_unmmatched2 <- Atlas_merged2 %>%
    filter(is.na(TaxID)) %>%
    pull(AtlasSp)
Atlas_merged3 <- Atlas_merged2 %>%
    group_by(AtlasSp) %>%
    mutate(
        match_type =
            case_when(
                (AtlasSp %in% spData_unmmatched2) ~ "manually",
                .default = match_type
            )
    )

## Note: There is a script that can do a partial match on the concatenated strings in the original Synonyms column. Not needed here, but just in case.






# =================================== Step 3: Manual Matching  =========================================== #
# Lets continue with manual matching of names: (Note: Updated names were retrieved from Wikipedia on 21.04.2024)
unmatched <- Atlas_merged3 %>% filter(match_type == "manually")
unmatched$AtlasSp_new <- gsub("Sylvia", "Curruca", unmatched$AtlasSp)
unmatched$AtlasSp_new <- gsub("Carduelis cannabina", "Linaria cannabina", unmatched$AtlasSp_new)
unmatched$AtlasSp_new <- gsub("Carduelis chloris", "Chloris chloris", unmatched$AtlasSp_new)
unmatched$AtlasSp_new <- gsub("Linaria chloris", "Chloris chloris", unmatched$AtlasSp_new)
unmatched$AtlasSp_new <- gsub("Locustella pryeri", "Helopsaltes pryeri", unmatched$AtlasSp_new)
unmatched$AtlasSp_new <- gsub("Cyanecula svecica", "Luscinia svecica", unmatched$AtlasSp_new)
unmatched$AtlasSp_new <- gsub("Passer rutilans", "Passer cinnamomeus", unmatched$AtlasSp_new)
unmatched$AtlasSp_new <- gsub("Luscinia akahige", "Larvivora akahige", unmatched$AtlasSp_new)
unmatched$AtlasSp_new <- gsub("Gallirallus okinawae", "Hypotaenidia okinawae", unmatched$AtlasSp_new)
unmatched$AtlasSp_new <- gsub("Luscinia komadori", "Larvivora komadori", unmatched$AtlasSp_new)
unmatched$AtlasSp_new <- gsub("Hirundo daurica", "Cecropis daurica", unmatched$AtlasSp_new)
unmatched$AtlasSp_new <- gsub("Poecile varius", "Sittiparus varius", unmatched$AtlasSp_new)
unmatched$AtlasSp_new <- gsub("Sterna albifrons", "Sternula albifrons", unmatched$AtlasSp_new)
unmatched$AtlasSp_new <- gsub("Egretta intermedia", "Ardea intermedia", unmatched$AtlasSp_new)
unmatched$AtlasSp_new <- gsub("Ammodramus maritimus", "Ammospiza maritima", unmatched$AtlasSp_new)
unmatched$AtlasSp_new <- gsub("Falcipennis canadensis", "Canachites canadensis", unmatched$AtlasSp_new)
unmatched$AtlasSp_new <- gsub("Regulus calendula", "Corthylio calendula", unmatched$AtlasSp_new)
unmatched$AtlasSp_new <- gsub("Oporornis formosus", "Geothlypis formosa", unmatched$AtlasSp_new)
unmatched$AtlasSp_new <- gsub("Bonasa bonasia", "Tetrastes bonasia", unmatched$AtlasSp_new)
unmatched$AtlasSp_new <- gsub("Regulus ignicapillus", "Regulus ignicapilla", unmatched$AtlasSp_new)
unmatched$AtlasSp_new <- gsub("Parus caeruleus", "Cyanistes caeruleus", unmatched$AtlasSp_new)
unmatched$AtlasSp_new <- gsub("Miliaria calandra", "Emberiza calandra", unmatched$AtlasSp_new)
unmatched$AtlasSp_new <- gsub("Delichon urbica", "Delichon urbicum", unmatched$AtlasSp_new)
unmatched$AtlasSp_new <- gsub("Oporornis philadelphia", "Geothlypis philadelphia", unmatched$AtlasSp_new)
unmatched$AtlasSp_new <- gsub("Seiurus noveboracensis", "Parkesia noveboracensis", unmatched$AtlasSp_new)
unmatched$AtlasSp_new <- gsub("Coccothraustes vespertinus", "Hesperiphona vespertina", unmatched$AtlasSp_new)
unmatched$AtlasSp_new <- gsub("Hydropogne caspia", "Hydroprogne caspia", unmatched$AtlasSp_new)


# Back to Step 1: Direct match:
unmatched$ScientificName <- unmatched$AtlasSp_new
unmatched$TaxID <- NULL
unmatched$Synonyms <- NULL

setdiff(unmatched$ScientificName, Tax_red$AtlasSp) # Matched all but 2 species which are new. Great !
unmatched_merged <- left_join(unmatched, Tax_red %>% rename("ScientificName" = "AtlasSp"))
unmatched_merged$match <- 1










# Merge to final table: 
Atlas_merged4 <- rbind(Atlas_merged3 %>% filter(match_type != "manually"), unmatched_merged) %>%
  group_by(AtlasSp) %>%
  mutate()
 Atlas_merged4 %>% filter(match_type == "manually")






## Data Checks

colSums(is.na(Atlas_merged4))
intersect(spData, Atlas_merged4$AtlasSp)
unique(Atlas_merged4$ScientificName)
intersect(Atlas_merged4$ScientificName, Tax_red$AtlasSp)
dups <- Atlas_merged4$ScientificName[duplicated(Atlas_merged4$ScientificName)] # 30 duplicates
name_vector <- Atlas_merged4 %>% pull(ScientificName)
Tax_LookUp <- Atlas_merged4 %>% distinct(AtlasSp, ScientificName)
unique(Tax_LookUp$ScientificName)

## Save Output


# List of synonym species in the data in case we want to fix this later...
Atlas_merged4 %>% 
	filter(ScientificName %in% dups) %>% 
	write.csv("../../Documents/documentation_methods/SynonymsAtlasData.csv")
	
	
# The Look up table with synonyms from BirdLife
write.csv(Atlas_merged4, "../../out/csv/full_BirdLife2018_TaxonomicLookup.csv")