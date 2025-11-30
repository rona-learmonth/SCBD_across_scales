### SPECIES CONTRIBUTIONS TO BETA DIVERSITY ###


#-------------------------------------------------------------------------------

## LOAD AND TIDY TRAIT DATA ##

traitdata<-read.csv(file.path(data_path,"intermediates","fractal_traits_mean.csv",header=TRUE))%>%
  select(Site,Plot,Quadrant,Species,Cover,Year,SLA,Height,SeedMass,Introduced_status)

#remove plots named A-E, 0 - silwood hexagons not in fractal arrangement
traitdata<-traitdata%>%filter(Plot!="A"&Plot!="B"&
                                Plot!="C"&Plot!="D"&
                                Plot!="E"&Plot!="F"&Plot!=""&
                                Plot!="C0"&
                                Plot!="0"&!is.na(Plot))%>%
  filter(!str_detect(Plot, "A"))%>%
  filter(!str_detect(Plot, "B"))%>%
  filter(!str_detect(Plot, "C"))%>%
  filter(!str_detect(Plot, "D"))

#remove * from plot names
traitdata$Plot<-str_replace(traitdata$Plot, '\\*', '')

#check plot names are sane
unique(traitdata$Plot)

#create major and minor triad identifiers - include block for boothby/knepp 

traitdata$Majortriad<-str_sub(traitdata$Plot,1,-3)
traitdata$Minortriad<-str_sub(traitdata$Plot,-2,-2)


#-------------------------------------------------------------------------------

## MAJOR TRIADS WITHIN EACH SITE = LARGE FRACTAL

#create function that calculates SCBD in one step to run on subsetted data
calc_scbdlarge<-function(x){
  temp <- sample2matrix(data.frame(x$Majortriad, as.numeric(x$Cover), x$Species))
  # Contributions to beta-diversity
  temp2<-beta.div(temp)$SCBD
  #convert named numeric vector to df
  data.frame(Species=names(temp2),
             Beta_contrib_major=temp2,row.names=NULL)
}

#create data list
traitdata_list_large=split(traitdata,list(traitdata$Site,traitdata$Year),drop=TRUE)

#drop silwood park.2021 - no variation in beta.div
traitdata_list_large$`Silwood Park.2021`=NULL

#apply function to list 
largebetalist<-map(traitdata_list_large,calc_scbdlarge)

#get site/year id for each 
largebetalist<-map2(largebetalist,names(largebetalist),~mutate(.x,ID=.y))

#make big df
largebetadf<-do.call("rbind",largebetalist)
ID<-as.data.frame(largebetadf[c('Site','Year')]<-
  str_split_fixed(largebetadf$ID,"[.]",2))

#remove ID column
largebetadf<-largebetadf%>%select(Species,Beta_contrib_major,Site,Year)

#make year format match
largebetadf$Year<-as.factor(largebetadf$Year)
traitdata$Year<-as.factor(traitdata$Year)

#import trait data then match by site and species
traitbetadata<-left_join(traitdata,largebetadf)


#-------------------------------------------------------------------------------

### MINOR TRIADS WITHIN EACH MAJOR TRIAD ###

#create function that calculates SCBD in one step to run on subsetted data
calc_scbdmedium<-function(x){
  temp <- sample2matrix(data.frame(x$Minortriad, as.numeric(x$Cover), x$Species))
  # Contributions to beta-diversity
  temp2<-beta.div(temp)$SCBD
  #convert named numeric vector to df
  data.frame(Species=names(temp2),
             Beta_contrib_minor=temp2,row.names=NULL)
}

#create data list
traitdata_list_medium=split(traitdata,list(traitdata$Site,traitdata$Year,traitdata$Majortriad),drop=TRUE)

#remove df with no beta.div variation
  #e.g. in knepp 2022 only one minor triad per block sampled
traitdata_list_medium$Budworth.2021.1=NULL
traitdata_list_medium$`Enez forest.2021.1`=NULL
traitdata_list_medium$`Enez forest.2021.3`=NULL
traitdata_list_medium$Ordu_urban.2021.1=NULL
traitdata_list_medium$`Silwood Park.2021.2`=NULL
traitdata_list_medium$Budworth.2021.3=NULL
traitdata_list_medium$Ordu_urban.2021.3=NULL
traitdata_list_medium$Knepp.2022.M2=NULL
traitdata_list_medium$Knepp.2022.M3=NULL
traitdata_list_medium$Knepp.2022.N2=NULL
traitdata_list_medium$Knepp.2022.N3=NULL
traitdata_list_medium$Knepp.2022.SX2=NULL
traitdata_list_medium$Knepp.2022.SX3=NULL
traitdata_list_medium$Knepp.2022.SY2=NULL
traitdata_list_medium$Knepp.2022.SY3=NULL
traitdata_list_medium$Knepp.2022.SZ2=NULL
traitdata_list_medium$Knepp.2022.SZ3=NULL
traitdata_list_medium$Boothby.2022.S3=NULL
traitdata_list_medium$Boothby.2022.C2=NULL
traitdata_list_medium$rhf.2019.10=NULL

#apply function to list 
mediumbetalist<-map(traitdata_list_medium,calc_scbdmedium)

#give each row a site/year/majortriad id
mediumbetalist<-map2(mediumbetalist,names(mediumbetalist),~mutate(.x,ID=.y))

#make big df
mediumbetadf<-do.call("rbind",mediumbetalist)

#split id into columns
ID<-mediumbetadf[c('Site','Year','Majortriad')]<-
  str_split_fixed(mediumbetadf$ID,"[.]",3)

#remove ID column
mediumbetadf<-mediumbetadf%>%select(Species,Beta_contrib_minor,Site,Year,Majortriad)

#make year format match
mediumbetadf$Year<-as.factor(mediumbetadf$Year)

#import trait data then match by site and species
traitbetadata2<-left_join(traitbetadata,mediumbetadf)


#-------------------------------------------------------------------------------

### PLOT LEVEL WITHIN EACH MINOR TRIAD ###

#create function that calculates SCBD in one step to run on subsetted data
calc_scbdsmall<-function(x){
  temp <- sample2matrix(data.frame(x$Plot, as.numeric(x$Cover), x$Species))
  # Contributions to beta-diversity
  temp2<-beta.div(temp)$SCBD
  #convert named numeric vector to df
  data.frame(Species=names(temp2),
             Beta_contrib_plot=temp2,row.names=NULL)
}

#create data list of each minor within each major
traitdata_list_small=split(traitdata,list(traitdata$Site,traitdata$Year,traitdata$Majortriad,traitdata$Minortriad),drop=TRUE)


#remove minor triads which do not contain at least two plots
#because of boothby monocultures, need to include a -must have 2 or more species- condition
newlist <- list()

for (i in seq_along(traitdata_list_small)) {
  df_i <- traitdata_list_small[[i]]
  if (length(unique(df_i$Plot)) >= 2 &&
      length(unique(df_i$Species)) >= 2) {
    df_name <- names(traitdata_list_small)[i]
    newlist[[df_name]] <- df_i
  }
}

# Remove null dataframes
newlist <- newlist[!sapply(newlist, is.null)]

#calculate scbd for all groups with beta.div variance
smallbetalist<-map(newlist,calc_scbdsmall)

#give each row a site/year/plot id
smallbetalist<-map2(smallbetalist,names(smallbetalist),~mutate(.x,ID=.y))

#make big df
smallbetadf<-do.call("rbind",smallbetalist)

#split id into columns
ID<-smallbetadf[c('Site','Year','Majortriad','Minortriad')]<-
  str_split_fixed(smallbetadf$ID,"[.]",4)

#remove ID column
smallbetadf<-smallbetadf%>%select(Species,Beta_contrib_plot,Site,Year,Majortriad,Minortriad)

#make year format match
smallbetadf$Year<-as.factor(smallbetadf$Year)

#import trait data then match by site, year, majortriad, minortriad and species
traitbetadata3<-left_join(traitbetadata2,smallbetadf)


#save to excel 
write.csv(traitbetadata3,
          file = file.path(data_path,"plantdata.csv"))

#end

