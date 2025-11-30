### UK SITES SCBD CALCULATIONS ###

#-------------------------------------------------------------------------------
## CREATE FUNCTION ##

#create function that calculates SCBD in one step to run on subsetted data
calc_scbd<-function(x){
  temp <- sample2matrix(data.frame(x$plot, as.numeric(x$abundance), x$species))
  # Contributions to beta-diversity
  temp2<-beta.div(temp)$SCBD
  #convert named numeric vector to df
  data.frame(species=names(temp2),
             beta_contrib=temp2,row.names=NULL)
}

#-------------------------------------------------------------------------------

## IMPORT DATA ##

silwood<-read.csv(
  file.path(data_path, "gbifdata","silwoodtraitdf2.csv"))
budworth<-read.csv(
  file.path(data_path, "gbifdata","budworthtraitdf2.csv"))
boothby<-read.csv(
  file.path(data_path, "gbifdata","boothbytraitdf2.csv"))
knepp<-read.csv(
  file.path(data_path, "gbifdata","knepptraitdf2.csv"))
uk<-read.csv(
  file.path(data_path, "gbifdata","uktraitdf.csv"))

#-------------------------------------------------------------------------------

## APPLY FUNCTION TO EACH SITE AND YEAR ##

# SILWOOD #

#split by year
silwoody21<-silwood%>%filter(year==2021)
silwoody22<-silwood%>%filter(year==2022)
silwoody23<-silwood%>%filter(year==2023)

#apply function to each year
silwoodbetadivy1<-calc_scbd(silwoody21)%>%mutate(year=2021)
silwoodbetadivy2<-calc_scbd(silwoody22)%>%mutate(year=2022)
silwoodbetadivy3<-calc_scbd(silwoody23)%>%mutate(year=2023)

#join years
silwoodbetadiv<-rbind(silwoodbetadivy1,silwoodbetadivy2,silwoodbetadivy3)

#match onto original data by plot and species
silwoodtraitbetadata<-left_join(silwood,silwoodbetadiv)%>%select(plot,species,year,abundance,SLA,Height,Seed_mass,beta_contrib)

#-------------------------------------------------------------------------------

# BOOTHBY #

#split by year
boothbyy22<-boothby%>%filter(year=="2022")
boothbyy23<-boothby%>%filter(year=="2023")

#apply function to each year
boothbybetadivy22<-calc_scbd(boothbyy22)%>%mutate(year=2022)
#boothbybetadivy23<-calc_scbd(boothbyy23)%>%mutate(year=2023) #too little variation currently - redownload adn rerun all 2023 samples in a bit?

#join years
#boothbybetadiv<-rbind(boothbybetadivy22,boothbybetadivy23)

#match onto original data by plot and species
boothbytraitbetadata<-left_join(boothby,boothbybetadivy22)%>%select(plot,species,year,abundance,SLA,Height,Seed_mass,beta_contrib)

#-------------------------------------------------------------------------------

# BUDWORTH #

#apply function
budworthbetadiv<-calc_scbd(budworth)

#match onto original data by plot and species
budworthtraitbetadata<-left_join(budworth,budworthbetadiv)%>%select(plot,species,year,abundance,SLA,Height,Seed_mass,beta_contrib)

#-------------------------------------------------------------------------------

# KNEPP #

#split by year
kneppy22<-knepp%>%filter(year=="2022")
kneppy23<-knepp%>%filter(year=="2023")

#apply function to each year
kneppbetadivy22<-calc_scbd(kneppy22)%>%mutate(year=2022)
kneppbetadivy23<-calc_scbd(kneppy23)%>%mutate(year=2023)

#join years
kneppbetadiv<-rbind(kneppbetadivy22,kneppbetadivy23)

#match onto original data by plot and species
knepptraitbetadata<-left_join(knepp,kneppbetadiv)%>%select(plot,species,year,abundance,SLA,Height,Seed_mass,beta_contrib)

#-------------------------------------------------------------------------------

## EXPORT DATA ##

write.csv(silwoodtraitbetadata, 
          file = file.path(data_path,"gbifdata", "silwoodtraitbetadata.csv"))
write.csv(boothbytraitbetadata, 
          file = file.path(data_path,"gbifdata", "boothbytraitbetadata.csv"))
write.csv(budworthtraitbetadata, 
          file = file.path(data_path,"gbifdata", "budworthtraitbetadata.csv"))
write.csv(knepptraitbetadata, 
          file = file.path(data_path,"gbifdata", "knepptraitbetadata.csv"))

#-------------------------------------------------------------------------------

## ATTEMPT FOR THE WHOLE UK ##

#duplicate uk data
uktraits<-uk%>%select(species,SLA,height,Seed_mass)%>%
  distinct(species,SLA,height,Seed_mass,.keep_all = TRUE)

#prepare data - should have done this in pt 2 but i am stupid
ukprep <- uk %>%
  select(species, plot) %>%
  filter(species != "") %>%
  group_by(plot, species) %>%
  summarise(abundance = n())

#add traits back on
uk<-left_join(ukprep,uktraits,relationship = "many-to-many")

#apply function
ukbetadiv<-calc_scbd(uk)

#match onto original data by plot and species
uktraitbetadata<-left_join(uk,ukbetadiv)


write.csv(uktraitbetadata, 
          file = file.path(data_path,"gbifdata", "uktraitbetadata.csv"))

# end