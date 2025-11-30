
### TRAIT DATA UK GBIF ###

#-------------------------------------------------------------------------------
### CREATE FUNCTION WHICH GENERATES DF WITH TRAIT VALUES ### 

gettraitdata <- function(GBIFdf) {
  # Split GBIFdf into grid based on lat/long values
  GBIFdf$c.lat <- LETTERS[as.numeric(cut(as.numeric(GBIFdf$decimalLatitude), breaks = 10))]
  GBIFdf$c.long <- as.numeric(cut(GBIFdf$decimalLongitude, breaks = 10))
  
  # Paste lat/long to make grid cell identifiers
  GBIFdf$plot <- paste(GBIFdf$c.lat, GBIFdf$c.long, sep = "")
  
  # Create df with summed abundance for each species in each grid cell
  GBIFdf <- GBIFdf %>%
    select(species, year, plot) %>%
    filter(species != "") %>%
    group_by(plot, species, year) %>%
    summarise(abundance = n())
  
  ### TRAIT DATA ###
  
  traitlist <- c("seed mass", "whole plant height", "leaf area per leaf dry mass")
  species_list <- unique(GBIFdf$species)
  
  traitdf <- BIEN_trait_traitbyspecies(trait = traitlist, species = species_list) %>%
    select(scrubbed_species_binomial, trait_name, trait_value) %>%
    group_by(scrubbed_species_binomial, trait_name)
  traitdf$trait_value <- as.numeric(traitdf$trait_value)
  
  # Try to remove duplicates
  traitdf <- traitdf %>%
    group_by(scrubbed_species_binomial, trait_name) %>%
    summarise(meanvalue = mean(as.numeric(trait_value))) %>%
    pivot_wider(names_from = trait_name, values_from = meanvalue)
  
  # Match trait GBIFdf back onto big GBIFdfset based on species
  
  # Create seedmass column
  GBIFdf$SLA <- traitdf$`leaf area per leaf dry mass`[
    match(GBIFdf$species, traitdf$scrubbed_species_binomial)]
  
  # Create height column
  GBIFdf$Height <- traitdf$`whole plant height`[
    match(GBIFdf$species, traitdf$scrubbed_species_binomial)]
  
  # Create SLA column
  GBIFdf$Seed_mass <- traitdf$`seed mass`[
    match(GBIFdf$species, traitdf$scrubbed_species_binomial)]
  
  return(GBIFdf)  # Add return statement to output the modified GBIFdf
}


#-------------------------------------------------------------------------------

## IMPORT SITE DATA ##

silwood<-read.csv(
  file.path(data_path, "gbifdata", "silwoodplantdata2.csv"))
budworth<-read.csv(
  file.path(data_path, "gbifdata", "budworthplantdata2.csv"))
boothby<-read.csv(
  file.path(data_path, "gbifdata", "boothbyplantdata2.csv"))
knepp<-read.csv(
  file.path(data_path, "gbifdata", "kneppplantdata2.csv"))
uk<-read.csv(
  file.path(data_path, "gbifdata", "ukplantdata.csv"))

#-------------------------------------------------------------------------------

## GET TRAIT DATA FOR EACH SITE ##

silwoodtraitdf<-gettraitdata(silwood)
budworthtraitdf<-gettraitdata(budworth)
boothbytraitdf<-gettraitdata(boothby)
knepptraitdf<-gettraitdata(knepp)

#-------------------------------------------------------------------------------

## SAVE OUTPUTS AS CSV FILES ##
write.csv(silwoodtraitdf, 
          file = file.path(data_path,"gbifdata", "silwoodtraitdf2.csv"))
write.csv(budworthtraitdf, 
          file = file.path(data_path,"gbifdata", "budworthtraitdf2.csv"))
write.csv(boothbytraitdf, 
          file = file.path(data_path,"gbifdata", "boothbytraitdf2.csv"))
write.csv(knepptraitdf, 
          file = file.path(data_path,"gbifdata", "knepptraitdf2.csv"))


#-------------------------------------------------------------------------------

## APPLY TO UK-WIDE DATA ##

#UK-wide data isn't cooperating - do manually

# Split GBIFdf into grid based on lat/long values
uk$c.lat <- LETTERS[as.numeric(cut(as.numeric(uk$decimalLatitude), breaks = 10))]
uk$c.long <- as.numeric(cut(uk$decimalLongitude, breaks = 10))

# Paste lat/long to make grid cell identifiers
uk$plot <- paste(uk$c.lat, uk$c.long, sep = "")

# Create df with summed abundance for each species in each grid cell
uktraitdf <- uk %>%
  select(species, year, plot) %>%
  filter(species != "") %>%
  group_by(plot, species, year) %>%
  summarise(abundance = n())

#generate trait and species list
traitlist <- c("seed mass", "whole plant height", "leaf area per leaf dry mass")
ukspecies_list <- unique(uk$species)


#species list is 3723 long - keeps breaking everything. split into more manageable chunks
specieslist1<-ukspecies_list[1:500]
specieslist2<-ukspecies_list[501:1000]
specieslist3<-ukspecies_list[1001:1500]
specieslist4<-ukspecies_list[1501:2000]
specieslist5<-ukspecies_list[2001:2500]
#5 is problematic - break into smaller chunks
specieslist5a<-ukspecies_list[2001:2250]
specieslist5b<-ukspecies_list[2250:2500]
specieslist5a<-specieslist5a[-203] #index 5: 203 is broken
specieslist6<-ukspecies_list[2501:3000]
specieslist7<-ukspecies_list[3001:3500]
specieslist8<-ukspecies_list[3501:3723]


#run gradual function, matching 500 species onto the df at a time
traitdf<-data.frame()

#-------------------------------------------------------------------------------

## EXTRACT SEED MASS TRAIT DATA ##

gradualukseedmass<-function(specieslist_i){
  for (i in 1:length(specieslist_i)){
    species_i<-specieslist_i[i]
    traitdf_i<-BIEN_trait_traitbyspecies(
      trait="seed mass",
      species=species_i)
    traitdf_i<-traitdf_i%>%
      select(scrubbed_species_binomial, trait_name, trait_value)
    traitdf<-rbind(traitdf,traitdf_i)
  }
  
  traitdf$trait_value <- as.numeric(traitdf$trait_value)
  
  # Try to remove duplicates
  traitdf <- traitdf %>%
    group_by(scrubbed_species_binomial, trait_name) %>%
    summarise(meanvalue = mean(as.numeric(trait_value))) %>%
    pivot_wider(names_from = trait_name, values_from = meanvalue)
  
  
}

#apply gradual function to each list, resetting traitdf each time
seedmass1<-gradualukseedmass(specieslist1)
traitdf<-data.frame()
seedmass2<-gradualukseedmass(specieslist2)
traitdf<-data.frame()
seedmass3<-gradualukseedmass(specieslist3)
traitdf<-data.frame()
seedmass4<-gradualukseedmass(specieslist4)
traitdf<-data.frame()
seedmass5<-gradualukseedmass(specieslist5a)
traitdf<-data.frame()
seedmass5b<-gradualukseedmass(specieslist5b)
traitdf<-data.frame()
seedmass6<-gradualukseedmass(specieslist6)
traitdf<-data.frame()
seedmass7<-gradualukseedmass(specieslist7)
traitdf<-data.frame()
seedmass8<-gradualukseedmass(specieslist8)

seedmassdata<-rbind(seedmass1,seedmass2,seedmass3,seedmass4,seedmass5,seedmass5b,seedmass6,seedmass7,seedmass8)


#import add on data
seedmass<-seedmassdata%>%select(scrubbed_species_binomial,seed.mass)%>%
  rename(species=scrubbed_species_binomial,Seed_mass=seed.mass) %>%
  group_by(species,Seed_mass)%>%summarise(count=n())%>%select(!count)

#add onto uk data set
uktraits<-left_join(uk,seedmass,relationship = "many-to-many")


#-------------------------------------------------------------------------------

## REPEAT FOR SLA ##

gradualukSLA<-function(specieslist_i){
  for (i in 1:length(specieslist_i)){
    species_i<-specieslist_i[i]
    traitdf_i<-BIEN_trait_traitbyspecies(
      trait="leaf area per leaf dry mass",
      species=species_i)
    traitdf_i<-traitdf_i%>%
      select(scrubbed_species_binomial, trait_name, trait_value)
    traitdf<-rbind(traitdf,traitdf_i)
  }
  
  traitdf$trait_value <- as.numeric(traitdf$trait_value)
  
  # Try to remove duplicates
  traitdf <- traitdf %>%
    group_by(scrubbed_species_binomial, trait_name) %>%
    summarise(meanvalue = mean(as.numeric(trait_value))) %>%
    pivot_wider(names_from = trait_name, values_from = meanvalue)
}


SLA1<-gradualukSLA(specieslist1)
traitdf<-data.frame()
SLA2<-gradualukSLA(specieslist2)
traitdf<-data.frame()
SLA3<-gradualukSLA(specieslist3)
traitdf<-data.frame()
SLA4<-gradualukSLA(specieslist4)
traitdf<-data.frame()
SLA5a<-gradualukSLA(specieslist5a)
traitdf<-data.frame()
SLA5b<-gradualukSLA(specieslist5b)
traitdf<-data.frame()
SLA6<-gradualukSLA(specieslist6)
traitdf<-data.frame()
SLA7<-gradualukSLA(specieslist7)
traitdf<-data.frame()
SLA8<-gradualukSLA(specieslist8)

SLAdata<-rbind(SLA1,SLA2,SLA3,SLA4,SLA5a,SLA5b,SLA6,SLA7,SLA8)

uktraits2<-left_join(uktraits,SLAdata,relationship = "many-to-many")

#-------------------------------------------------------------------------------

## REPEAT FOR HEIGHT ##

gradualukheight<-function(specieslist_i){
  for (i in 1:length(specieslist_i)){
    species_i<-specieslist_i[i]
    traitdf_i<-BIEN_trait_traitbyspecies(
      trait="whole plant height",
      species=species_i)
    traitdf_i<-traitdf_i%>%
      select(scrubbed_species_binomial, trait_name, trait_value)
    traitdf<-rbind(traitdf,traitdf_i)
  }
  
  traitdf$trait_value <- as.numeric(traitdf$trait_value)
  
  # Try to remove duplicates
  traitdf <- traitdf %>%
    group_by(scrubbed_species_binomial, trait_name) %>%
    summarise(meanvalue = mean(as.numeric(trait_value))) %>%
    pivot_wider(names_from = trait_name, values_from = meanvalue)
}

height1<-gradualukheight(specieslist1)
traitdf<-data.frame()
height2<-gradualukheight(specieslist2)
traitdf<-data.frame()
height3<-gradualukheight(specieslist3)
traitdf<-data.frame()
SLA5b<-gradualukSLA(specieslist5a)
traitdf<-data.frame()
SLA5b<-gradualukSLA(specieslist5b)
traitdf<-data.frame()
height6<-gradualukheight(specieslist6)
traitdf<-data.frame()
height7<-gradualukheight(specieslist7)
traitdf<-data.frame()
height8<-gradualukheight(specieslist8)

heightdata<-rbind(height1,height2,height3,height4,height5a,height5b,height6,height7,height8)

#bind onto big df
uktraits3<-left_join(uktraits2,heightdata,relationship = "many-to-many")

#-------------------------------------------------------------------------------

## SAVE DATA ##
write.csv(uktraits3, 
          file = file.path(data_path,"gbifdata", "uktraitdf.csv"))

# end