
### PHYLOGENETIC DISTANCES - csMSPD ###

#-------------------------------------------------------------------------------

#load data 
plantdata<-read.csv(
  file.path(data_path,"plantdata.csv"))%>%select(!X)%>%distinct()

#load phylogeny for vascular plants
tree <- read.tree(file=
                    file.path(data_path,"Vascular_Plants_rooted.dated.tre"))

#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------

## PREPARE COMMUNITY MATRIX ##

preparecommdata<-function(df){
  
  # create siteyear identifier
  df$Siteyear <- paste(df$Site, df$Year, sep=".")
  
  # Clean encoding issues
  df$Species <- iconv(df$Species, from = "UTF-8", to = "UTF-8", sub = "")
  df$Species <- gsub("\u00A0", " ", df$Species)   # remove non-breaking spaces
  df$Species <- trimws(df$Species)
  
  # change all species names to Genus_species format
  df$Species <- gsub(" ", "_", df$Species)
  
  # match to phylogeny
  species.tree <- congeneric.merge(tree, unique(df$Species))
  
  # create community matrix
  community <- df %>% select(Siteyear, Cover, Species)
  names(community) <- NULL
  
  write.table(community,"newdf.txt",sep="\t",row.names=FALSE)
  
  comm.ds <- readsample("newdf.txt")
  
  sitetree <- prune.sample(comm.ds, tree)
  
  comm.ds <- comm.ds[, sitetree$tip.label]
  
  return(comm.ds)
}


comm.site<-preparecommdata(plantdata)


#-------------------------------------------------------------------------------

## CALCULATING PHYLOGENETIC DISTANCES ##

#make empty data frame
phylo_df_all <- data.frame(Siteyear = character(), Species = character(), Phylo_dist = numeric(),
                           stringsAsFactors = FALSE)

#create species tree
species.tree <- congeneric.merge(tree, colnames(comm.site))

#apply for loop to calc and save phylogenetic distance for each species 
for (i in 1:nrow(comm.site)){ 
  
  #subset of each site
  commrow = comm.site[i,] #split by row
  comm_i<-as.matrix(commrow[colSums(commrow !=0) > 0])
  #use rows with at least one non-zero value
  
  #if statement to avoid eg site with only one species
  #which means not possible to compare phylodist to other species
  
  checkdimensions<-data.frame(comm_i)
  
  if (ncol(checkdimensions)>1 ){
    tree_i<-prune.sample(comm_i,species.tree)
    
    #create phylogenetic distance matrix
    phy.dist_i<-cophenetic.phylo(tree_i)
    
    #3. calculate mean distance from comm for each species
    meandist_i<-colMeans(phy.dist_i)
    
    #adding to main data frame
    #site, species, and phylogenetic distance to other species at the site
    phylo_dist_df_i <- data.frame(
      Siteyear = row.names(commrow),
      Species = names(meandist_i),
      Phylo_dist = as.numeric(meandist_i),
      stringsAsFactors = FALSE)
    phylo_df_all <- rbind(phylo_df_all,phylo_dist_df_i)
    
  } 
  
  #progress bar
  if(i %% 10 == 0){cat(paste0("Site_", i, "_done", "\n"))}
}

#convert species names back from "_" to ""
phylo_df_all$Species<-sub("_", " ", phylo_df_all$Species)

#split siteyear back into site and year
phylo_df_all<-separate(phylo_df_all, Siteyear, 
                       into = c("Site", "Year"), sep = "\\.")
phylo_df_all$Year<-as.integer(phylo_df_all$Year)

#join onto existing data
plantdata<-left_join(plantdata,phylo_df_all)


#-------------------------------------------------------------------------------

### CALCULATE STANDARDISED EFFECT SIZES FROM PHYLOGENETIC DISTANCES ###

#standardised mean phylogenetic distance = SMPD
#SMPD = (obs - mean(null)) / SD(null)

#calculate mpd by site
sitedata<-plantdata%>%group_by(Site,Year)%>%na.omit(Phylo_dist)%>%
  summarise(mpd=mean(Phylo_dist),sd.mpd=sd(Phylo_dist))%>%
  mutate(siteyear=paste(Site,Year,sep="."))

#join siteyear mean and sd back onto df
newmetric_df<-left_join(plantdata,sitedata)

#calculate csMSPD
newmetric_df<-newmetric_df%>%mutate(csMSPD=(Phylo_dist-mpd)/sd.mpd)%>%
  select(Site,Plot,Quadrant,Species,Cover,Year,SLA,Height,SeedMass,
         Introduced_status,Majortriad,Minortriad,Beta_contrib_major,
         Beta_contrib_minor,Beta_contrib_plot,Phylo_dist,csMSPD)


#-------------------------------------------------------------------------------


## SAVE OUTPUT ##
  #write.csv(plantdata,"completedata.csv")

write.csv(newmetric_df, 
          file = file.path(data_path,
                           "intermediates","trait_csmspd_plants.csv"))

#end
