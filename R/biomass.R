

fish.biomass <- function(database){
  
  spp <- database |> 
    select(genus, species) |> 
    group_by(genus,species) |> 
    unique() |> 
    filter(!is.na(species))
  species=c("Zapteryx exasperata", "Abudefduf troschelii")
  
  
  test <- rfishbase::ecology(species)
  
fish_TL <-
  rfishbase::ecology(species_list =spp$species, 
                     fields = c("Species", "DietTroph")) |> 
  group_by(Species) |> 
  summarise(DietTroph=mean(DietTroph, na.rm = T))


  fish_ab <- rfishbase::length_weight(species_list =spp$species, 
                                      fields = c("Species", "a", "b")) |> 
    
            group_by(Species) |> 
    summarise(a=mean(a,na.rm = T),
              b=mean(b,na.rm = T))
  
  spp_traits <- merge(fish_TL, fish_ab, by="Species")
  
  
  
  spp2 <- merge(spp, spp_traits, by.x = "species", by.y = "Species") |> 
    group_by(genus) |> 
    mutate(
      mntroph = mean(DietTroph, na.rm = T),
      a_mean = mean(a, na.rm = T),
      b_mean = mean(b, na.rm = T)
    ) %>%
    ungroup() %>%
    mutate(
      trophall = ifelse(is.na(DietTroph), mntroph, DietTroph),
      a_all = ifelse(is.na(a), a_mean, a),
      b_all = ifelse(is.na(b), b_mean, b)
    ) %>%
    select(species,
           a = a_all, b = b_all, trophic_level = trophall)
  
  
  db <- merge(database, spp2, by="species", all.x = T) |> 
    mutate(a= as.numeric(a), 
           b= as.numeric(b), 
           biomass = (quantity * a * (size^b)))
  

  
}