# Partial Taxonomic Match BOTW Tax
# =================================== Step 2: Synonyms partial match =============================== #
Synonym_strings <- merged2 %>%
    filter(str_detect(Synonyms, ";")) %>%
    pull(Synonyms)
Pattern_sp <- unique(Atlas_sp3$AtlasSp)
Pattern_sp_new <- gsub("Sylvia", "Curruca", Pattern_sp)

result_df <- data.frame(
    Match = Pattern_sp_new,
    Synonyms = NA,
    stringsAsFactors = FALSE
)

# Loop through each element of match_vector
synonyms_list <- list()
for (i in seq_along(Pattern_sp_new)) {
    match <- result_df[i, ]

    for (bl in seq_along(Synonym_strings)) {
        string.bl <- Synonym_strings[bl]
        synonyms <- string.bl[any(str_detect(match$Match, string.bl))]
        if (length(synonyms) == 0) {
            synonyms <- NA
        } else {
            synonyms <- string.bl
        }
        synonyms_list[[bl]] <- synonyms
    }
    synonyms2 <- unique(unlist(synonyms_list))
    result_df <- rbind(result_df, data.frame(Match = match$Match, Synonyms = synonyms2, stringsAsFactors = FALSE))
}

result_df
# ================================================================================================== #

head(result_df)