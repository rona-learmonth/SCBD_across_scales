### UK plot PHYLOGENETIC DISTANCES ###


#-------------------------------------------------------------------------------

## IMPORT DATA ##

silwood<-read.csv(
  file.path(data_path, "gbifdata", "silwoodtraitbetadata.csv"))
boothby<-read.csv(
  file.path(data_path, "gbifdata", "boothbytraitbetadata.csv"))
budworth<-read.csv(
  file.path(data_path, "gbifdata", "budworthtraitbetadata.csv"))
knepp<-read.csv(
  file.path(data_path, "gbifdata", "knepptraitbetadata.csv"))
uk<-read.csv(
  file.path(data_path, "gbifdata", "uktraitbetadata.csv"))

#load phylogeny for vascular plants
tree <- read.tree(file=file.path(data_path,"Vascular_Plants_rooted.dated.tre"))

#-------------------------------------------------------------------------------

## CREATE FUNCTION FOR DATA PREPARATION ##

preparecommdata<-function(df){
  
  #create plotyear identifier
  df$plotyear<-paste(df$plot,df$year,sep=".")
  
  #change all species names to Genus_species to make them match
  df$species<-sub(" ", "_", df$species)
  
  #match my plant species onto this tree
  species.tree <- congeneric.merge(tree, unique(df$species))
  
  # create community matrix
  community<-df%>%select(plotyear,abundance,species)
  
  #remove header
  names(community)<-NULL
  
  #save to .txt file
  write.table(community,"newdf.txt",sep="\t",row.names=FALSE)
  #manually change file name, save as .txt
  #load into r as matrix
  comm.ds<-readsample("newdf.txt")
  
  #prune tree to only species in my list
  plottree<-prune.sample(comm.ds,tree)
  
  #change order of taxa in phylo to match dataset
  comm.ds<-comm.ds[,plottree$tip.label]
}

#-------------------------------------------------------------------------------

## PREPARE ALL DATASETS ##

comm.silwood<-preparecommdata(silwood)
comm.boothby<-preparecommdata(boothby)
comm.budworth<-preparecommdata(budworth)
comm.knepp<-preparecommdata(knepp)

#remove rogue species in uk
uk<-uk %>%
  mutate(across('species', str_replace, 'Polypodium interjectum � vulgare', 'Polypodium interjectum'))
comm.uk<-preparecommdata(uk)

#-------------------------------------------------------------------------------

## CALCULATING PHYLOGENETIC DISTANCES ##

# SILWOOD #

#make empty data frame
phylo_df_all <- data.frame(plotyear = character(), species = character(), phylo_dist = numeric(),
                           stringsAsFactors = FALSE)

#create species tree
species.tree <- congeneric.merge(tree, colnames(comm.silwood))

#apply for loop to calc and save phylogenetic distance for each species 
for (i in 1:nrow(comm.silwood)){ 
  
  #subset of each plot
  commrow = comm.silwood[i,] #split by row
  comm_i<-as.matrix(commrow[colSums(commrow !=0) > 0])
  #use rows with at least one non-zero value
  
  #if statement to avoid eg plot with only one species
  #which means not possible to compare phylodist to other species
  
  checkdimensions<-data.frame(comm_i)
  
  if (ncol(checkdimensions)>1 ){
    tree_i<-prune.sample(comm_i,species.tree)
    
    #create phylogenetic distance matrix
    phy.dist_i<-cophenetic.phylo(tree_i)
    
    #3. calculate mean distance from comm for each species
    meandist_i<-colMeans(phy.dist_i)
    
    #adding to main data frame
    #plot, species, and phylogenetic distance to other species at the plot
    phylo_dist_df_i <- data.frame(
      plotyear = row.names(commrow),
      species = names(meandist_i),
      phylo_dist = as.numeric(meandist_i),
      stringsAsFactors = FALSE)
    phylo_df_all <- rbind(phylo_df_all,phylo_dist_df_i)
    
  } 
  
  #progress bar
  if(i %% 10 == 0){cat(paste0("plot_", i, "_done", "\n"))}
}

#convert species names back from "_" to ""
phylo_df_all$species<-sub("_", " ", phylo_df_all$species)

#split plotyear back into plot and year
phylo_df_all<-separate(phylo_df_all, plotyear, 
                       into = c("plot", "year"), sep = "\\.")
phylo_df_all$year<-as.integer(phylo_df_all$year)

#join onto existing data
silwoodphylodist<-phylo_df_all
silwoodtraitphylodf<-left_join(silwood,silwoodphylodist)%>%
  select(plot,species,year,abundance,SLA,Height,Seed_mass,beta_contrib,phylo_dist)

silwoodtraitphylodf$site<-"Silwood"

#-------------------------------------------------------------------------------

# BOOTHBY #

phylo_df_all <- data.frame(plotyear = character(), species = character(), phylo_dist = numeric(),
                           stringsAsFactors = FALSE)

species.tree <- congeneric.merge(tree, colnames(comm.boothby))

for (i in 1:nrow(comm.boothby)){ 
  
  #subset of each plot
  commrow = comm.boothby[i,] #split by row
  comm_i<-as.matrix(commrow[colSums(commrow !=0) > 0])
  #use rows with at least one non-zero value
  
  #if statement to avoid eg plot with only one species
  #which means not possible to compare phylodist to other species
  
  checkdimensions<-data.frame(comm_i)
  
  if (ncol(checkdimensions)>1 ){
    tree_i<-prune.sample(comm_i,species.tree)
    
    #create phylogenetic distance matrix
    phy.dist_i<-cophenetic.phylo(tree_i)
    
    #3. calculate mean distance from comm for each species
    meandist_i<-colMeans(phy.dist_i)
    
    #adding to main data frame
    #plot, species, and phylogenetic distance to other species at the plot
    phylo_dist_df_i <- data.frame(
      plotyear = row.names(commrow),
      species = names(meandist_i),
      phylo_dist = as.numeric(meandist_i),
      stringsAsFactors = FALSE)
    phylo_df_all <- rbind(phylo_df_all,phylo_dist_df_i)
    
  } 
  
  #progress bar
  if(i %% 10 == 0){cat(paste0("plot_", i, "_done", "\n"))}
}

#convert species names back from "_" to ""
phylo_df_all$species<-sub("_", " ", phylo_df_all$species)

#split plotyear back into plot and year
phylo_df_all<-separate(phylo_df_all, plotyear, 
                       into = c("plot", "year"), sep = "\\.")
phylo_df_all$year<-as.integer(phylo_df_all$year)

#join onto existing data
boothbyphylodist<-phylo_df_all
boothby<-boothby%>%
  select(plot,species,year,abundance,SLA,Height,Seed_mass,beta_contrib)
boothbytraitphylodf<-left_join(boothby,boothbyphylodist)

boothbytraitphylodf$site<-"Boothby"

#-------------------------------------------------------------------------------

# BUDWORTH #

phylo_df_all <- data.frame(plotyear = character(), species = character(), phylo_dist = numeric(),
                           stringsAsFactors = FALSE)

species.tree <- congeneric.merge(tree, colnames(comm.budworth))

for (i in 1:nrow(comm.budworth)){ 
  
  #subset of each plot
  commrow = comm.budworth[i,] #split by row
  comm_i<-as.matrix(commrow[colSums(commrow !=0) > 0])
  #use rows with at least one non-zero value
  
  #if statement to avoid eg plot with only one species
  #which means not possible to compare phylodist to other species
  
  checkdimensions<-data.frame(comm_i)
  
  if (ncol(checkdimensions)>1 ){
    tree_i<-prune.sample(comm_i,species.tree)
    
    #create phylogenetic distance matrix
    phy.dist_i<-cophenetic.phylo(tree_i)
    
    #3. calculate mean distance from comm for each species
    meandist_i<-colMeans(phy.dist_i)
    
    #adding to main data frame
    #plot, species, and phylogenetic distance to other species at the plot
    phylo_dist_df_i <- data.frame(
      plotyear = row.names(commrow),
      species = names(meandist_i),
      phylo_dist = as.numeric(meandist_i),
      stringsAsFactors = FALSE)
    phylo_df_all <- rbind(phylo_df_all,phylo_dist_df_i)
    
  } 
  
  #progress bar
  if(i %% 10 == 0){cat(paste0("plot_", i, "_done", "\n"))}
}

#convert species names back from "_" to ""
phylo_df_all$species<-sub("_", " ", phylo_df_all$species)

#split plotyear back into plot and year
phylo_df_all<-separate(phylo_df_all, plotyear, 
                       into = c("plot", "year"), sep = "\\.")
phylo_df_all$year<-as.integer(phylo_df_all$year)

budworthphylodist<-phylo_df_all
budworthtraitphylodf<-left_join(budworth,budworthphylodist)%>%
  select(plot,species,year,abundance,SLA,Height,Seed_mass,beta_contrib,phylo_dist)

budworthtraitphylodf$site<-"Budworth"


#-------------------------------------------------------------------------------

# KNEPP #

phylo_df_all <- data.frame(plotyear = character(), species = character(), phylo_dist = numeric(),
                           stringsAsFactors = FALSE)

species.tree <- congeneric.merge(tree, colnames(comm.knepp))

for (i in 1:nrow(comm.knepp)){ 
  
  #subset of each plot
  commrow = comm.knepp[i,] #split by row
  comm_i<-as.matrix(commrow[colSums(commrow !=0) > 0])
  #use rows with at least one non-zero value
  
  #if statement to avoid eg plot with only one species
  #which means not possible to compare phylodist to other species
  
  checkdimensions<-data.frame(comm_i)
  
  if (ncol(checkdimensions)>1 ){
    tree_i<-prune.sample(comm_i,species.tree)
    
    #create phylogenetic distance matrix
    phy.dist_i<-cophenetic.phylo(tree_i)
    
    #3. calculate mean distance from comm for each species
    meandist_i<-colMeans(phy.dist_i)
    
    #adding to main data frame
    #plot, species, and phylogenetic distance to other species at the plot
    phylo_dist_df_i <- data.frame(
      plotyear = row.names(commrow),
      species = names(meandist_i),
      phylo_dist = as.numeric(meandist_i),
      stringsAsFactors = FALSE)
    phylo_df_all <- rbind(phylo_df_all,phylo_dist_df_i)
    
  } 
  
  #progress bar
  if(i %% 10 == 0){cat(paste0("plot_", i, "_done", "\n"))}
}

#convert species names back from "_" to ""
phylo_df_all$species<-sub("_", " ", phylo_df_all$species)

#split plotyear back into plot and year
phylo_df_all<-separate(phylo_df_all, plotyear, 
                       into = c("plot", "year"), sep = "\\.")
phylo_df_all$year<-as.integer(phylo_df_all$year)

kneppphylodist<-phylo_df_all
knepptraitphylodf<-left_join(knepp,kneppphylodist)%>%
  select(plot,species,year,abundance,SLA,Height,Seed_mass,beta_contrib,phylo_dist)

knepptraitphylodf$site<-"Knepp"


#-------------------------------------------------------------------------------

### CALCULATE STANDARDISED EFFECT SIZE FROM SPECIES MEAN PAIRWISE DISTANCES ###

#join dfs together
alldf<-rbind(silwoodtraitphylodf,boothbytraitphylodf,
             budworthtraitphylodf,knepptraitphylodf)
alldf$siteyear<-paste(alldf$site,alldf$year,sep=".")

#standardised mean phylogenetic distance = SMPD
#SMPD = (obs - mean(null)) / SD(null)

#calculate mpd by site
sitedata<-alldf%>%group_by(siteyear)%>%na.omit(phylo_dist)%>%
  summarise(mpd=mean(phylo_dist),sd.mpd=sd(phylo_dist))

#join siteyear mean adn sd back onto df
landscapedata<-left_join(alldf,sitedata)

#calculate csMSPD
landscapedata<-landscapedata%>%mutate(csMSPD=(phylo_dist-mpd)/sd.mpd)

#-------------------------------------------------------------------------------

## EXPORT DATA ##

write.csv(landscapedata, 
          file = file.path(data_path,"gbifdata","landscape_scale_data.csv"))


#-------------------------------------------------------------------------------

## REPEAT FOR WHOLE UK ##

phylo_df_all <- data.frame(plotyear = character(), species = character(), phylo_dist = numeric(),
                           stringsAsFactors = FALSE)

species.tree <- congeneric.merge(tree, colnames(comm.uk))

for (i in 1:nrow(comm.uk)){ 
  
  #subset of each plot
  commrow = comm.uk[i,] #split by row
  comm_i<-as.matrix(commrow[colSums(commrow !=0) > 0])
  #use rows with at least one non-zero value
  
  #if statement to avoid eg plot with only one species
  #which means not possible to compare phylodist to other species
  
  checkdimensions<-data.frame(comm_i)
  
  if (ncol(checkdimensions)>1 ){
    tree_i<-prune.sample(comm_i,species.tree)
    
    #create phylogenetic distance matrix
    phy.dist_i<-cophenetic.phylo(tree_i)
    
    #3. calculate mean distance from comm for each species
    meandist_i<-colMeans(phy.dist_i)
    
    #adding to main data frame
    #plot, species, and phylogenetic distance to other species at the plot
    phylo_dist_df_i <- data.frame(
      plotyear = row.names(commrow),
      species = names(meandist_i),
      phylo_dist = as.numeric(meandist_i),
      stringsAsFactors = FALSE)
    phylo_df_all <- rbind(phylo_df_all,phylo_dist_df_i)
    
  } 
  
  #progress bar
  if(i %% 10 == 0){cat(paste0("plot_", i, "_done", "\n"))}
}

#convert species names back from "_" to ""
phylo_df_all$species<-sub("_", " ", phylo_df_all$species)

#split plotyear back into plot and year
phylo_df_all<-separate(phylo_df_all, plotyear, 
                       into = c("plot"), sep = "\\.")

ukphylodist<-phylo_df_all
ukphylodist$year<-as.integer(2023)

#join onto existing data
uktraitphylodf<-left_join(uk,ukphylodist)%>%
  select(plot,species,year,abundance,SLA,height,Seed_mass,beta_contrib,phylo_dist)

uktraitphylodf$site<-"UK"


#-------------------------------------------------------------------------------
## STANDARDISED EFFECT SIZES - WHOLE UK ##

#standardised mean phylogenetic distance = SMPD
#SMPD = (obs - mean(null)) / SD(null)

#calculate mpd by site
ukphylonew<-uktraitphylodf%>%group_by(site)%>%na.omit(phylo_dist)%>%
  summarise(mpd=mean(phylo_dist),sd.mpd=sd(phylo_dist))

#join siteyear mean adn sd back onto df
uknewmetric_df<-left_join(uktraitphylodf,ukphylonew)

#calculate csMSPD
uknewmetric_df<-uknewmetric_df%>%mutate(csMSPD=(phylo_dist-mpd)/sd.mpd)



## SAVE UK DATA ##
write.csv(uknewmetric_df, 
          file = file.path(data_path,"gbifdata","ukfulldata.csv"))

# end