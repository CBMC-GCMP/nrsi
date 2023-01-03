

resolve.names <- function(database){ 
  
  print("Warning: This may take a while!")
#Retrieve best species matches from WoRMS and Fishbase      
sources <- c(worms = 9, fishbase = 155)

spp <- database |> 
  select(species) |> 
  unique()

resolved_names <- sources %>% 
  map(~ taxize::gnr_resolve(data_source_ids = .x, 
                            sci = spp$species,
                            best_match_only = T, 
                            fields = c("all"), 
                            canonical = T ))
## Part II:
#Leave only the correct results, and format it as data.frame

resolved_names_df <- resolved_names %>% 
  map(~ .x %>% 
        filter(!match_value %in% c("Could only match genus") & 
                 str_count(matched_name2, "\\w+") >= 2) %>% 
        select(user_supplied_name, matched_name2, taxon_id)) %>% 
  reduce(full_join, by = "user_supplied_name") %>% 
  set_names(c("user_supplied_name", "worms_sci_name",
              "worms_id", "fishbase_sci_name", "fishbase_id"))


## Part III:
#Filter absent values (NAs)

resolved_names <- resolved_names_df %>% 
  mutate(resolved_scientific_name = case_when(
    !is.na(worms_sci_name) ~ worms_sci_name,
    !is.na(fishbase_sci_name) ~ fishbase_sci_name,
    TRUE ~ user_supplied_name
  )) %>% 
  select(user_supplied_name, 
         resolved_scientific_name, 
         everything())%>% 
  rename(species= user_supplied_name) %>% 
  select(species, resolved_scientific_name)


valid_names <- resolved_names %>%
  mutate(
    New =  rfishbase::validate_names(species),
    correct_sp = ifelse(species == New, species, New)
  ) %>%
  select(species, correct_sp) 
  



db <- merge(database, valid_names, by="species", all.x=T) |>
  rename(old_species=species) |> 
  mutate(species= ifelse(old_species==correct_sp, old_species, correct_sp),
         species=ifelse(is.na(species), old_species, species)
         ) |> 
  select(-c(correct_sp, old_species)) |> 
  select(reef, depth, transect,genus, species, size, abundance)


}







