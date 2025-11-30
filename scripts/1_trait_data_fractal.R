### EXTRACTING TRAIT VALUES FROM BIEN ###



#-------------------------------------------------------------------------------

## LOAD AND MAKE SURE COLUMN NAMES MATCH ACROSS ALL SITES AND YEARS ##


#load all the data and make all columns match
boothby22<-read_excel(
  file.path(data_path,"boothby22.xlsx")) %>%
  dplyr::select(Site,Plot,Quadrant,Species,Cover,Year)
boothby23<-read.csv(
  file.path(data_path,"boothby23.csv")) %>%
  dplyr::select(Site,Plot,Quadrant,Species,Cover,Year)

budworth21<-read_excel(
  file.path(data_path,"budworth21.xlsx")) %>%
  dplyr::select(Site,Plot,Quadrant,Species,Cover,Year)

enez21 <- read_excel(
  file.path(data_path,"enez21.xlsx"))%>%
    dplyr::select(Site,Plot,Quadrant,Species,Cover,Year)

knepp22 <- read_excel(
  file.path(data_path,"knepp22.xlsx"))%>%
  mutate(Plot=paste(Block,Plot,sep=""))%>%
  dplyr::select(Site,Plot,Quadrant,Species,Cover,Year)
knepp23 <- read_excel(
  file.path(data_path,"knepp23.xlsx"))%>%
  mutate(Plot=paste(Block,Plot,sep=""))%>%
  dplyr::select(Site,Plot,Quadrant,Species,Cover,Year)

ordu21 <- read_excel(
  file.path(data_path,"ordu21.xlsx"))%>%
  dplyr::select(Site,Plot,Quadrant,Species,Cover,Year)

archbold21 <- read_excel(
  file.path(data_path,"archbold21.xlsx")) %>%
  mutate(Site="Archbold",Year="2021")%>%mutate_all(~gsub("Quad_", "", .))%>%
  dplyr::select(Site,Site_naming_scheme,Quadrat,Species,Cover,Year)%>%
  rename(Plot=Site_naming_scheme, Quadrant=Quadrat)

reinischikogel21 <- read_excel(
  file.path(data_path,"reinischkogel21.xlsx"))%>%
  mutate(Quadrant=as.factor(match(Block, LETTERS)))%>%
  dplyr::select(Site,Plot,Quadrant,Species,Cover,Year)

rhf17 <- read.csv(
  file.path(data_path,"rhf_17.csv"))%>%
  rename(Plot=Site)%>%mutate(Site="rhf")%>%
  mutate(Year=2017)%>%
  dplyr::select(Site,Plot,Quadrant,Species,Cover,Year)
rhf18 <- read.csv(
  file.path(data_path,"rhf_18.csv"))%>%
  rename(Plot=Plot_id)%>%mutate(Site="rhf")%>%
  mutate(Year=2018)%>%
  dplyr::select(Site,Plot,Quadrant,Species,Cover,Year)
rhf19 <- read.csv(
  file.path(data_path,"rhf_19.csv"))%>%
  rename(Plot=Plot_id)%>%mutate(Site="rhf")%>%
  mutate(Year=2019)%>%
  dplyr::select(Site,Plot,Quadrant,Species,Cover,Year)
rhf20 <- read.csv(
  file.path(data_path,"rhf_20.csv"))%>%
  rename(Plot=Plot_id)%>%mutate(Site="rhf")%>%
  mutate(Year=2020)%>%
  dplyr::select(Site,Plot,Quadrant,Species,Cover,Year)
rhf21 <- read.csv(
  file.path(data_path,"rhf_21.csv"))%>%
  rename(Plot=Plot_id)%>%mutate(Site="rhf")%>%
  mutate(Year=2021)%>%
  dplyr::select(Site,Plot,Quadrant,Species,Cover,Year)
silwood21 <- read_excel(
  file.path(data_path,"silwood21.xlsx"))%>%
  rename(Plot=Site)%>%mutate(Site="Silwood Park")%>%
  dplyr::select(Site,Plot,Quadrant,Species,Cover,Year)
silwood22 <- read_excel(
  file.path(data_path,"silwood22.xlsx"))%>%
  dplyr::select(Site,Plot,Quadrant,Species,Cover,Year)
silwood23<-read.csv(
  file.path(data_path,"silwood23.csv"))%>%
  dplyr::select(Site,Plot,Quadrant,Species,Cover,Year)

#need to make top level of rhf into a blocl
rhf<-rbind(rhf18,rhf19,rhf20,rhf21)
rhf$Block <- substr(rhf$Plot, 1, nchar(rhf$Plot) - 3)

#create one big df
alldata<-rbind(boothby22,boothby23,budworth21,enez21,knepp22,knepp23,ordu21,archbold21,
               reinischikogel21,rhf17,rhf18,rhf19,rhf20,rhf21,silwood21,silwood22,silwood23)

#separate into genus and species
speciesonly_df<-alldata%>%filter(!grepl(" sp.",Species)|Species=="Prunus spinosa")%>%
  filter(Species!="grass"&!grepl("ceae",Species)
         &Species!="Caloboletus calopus"&Species!="dark purple Brassica"&
           Species!="Nemophila"&Species!="Amanita rubescens"&!grepl("Cladonia",Species))

#rename lots of bad spelling
speciesonly_df$Species[speciesonly_df$Species=="Rubus fructicosus"]<-"Rubus fruticosus"
speciesonly_df$Species[speciesonly_df$Species=="Rubus fruticosus agg."]<-"Rubus fruticosus"
speciesonly_df$Species[speciesonly_df$Species=="Chamaenerion augustifolium"]<-"Chamaenerion angustifolium"
speciesonly_df$Species[speciesonly_df$Species=="Geranium robertianium"]<-"Geranium robertianum"
speciesonly_df$Species[speciesonly_df$Species=="Lachnocaulon beyrichiana"]<-"Lachnocaulon beyrichianum"
speciesonly_df$Species[speciesonly_df$Species=="Vicia tetraspermae"]<-"Vicia tetrasperma"
speciesonly_df$Species[speciesonly_df$Species=="Vicia fava"]<-"Vicia faba"
speciesonly_df$Species[speciesonly_df$Species=="Fallopia convolvulvus"]<-"Fallopia convolvulus"
speciesonly_df$Species[speciesonly_df$Species=="Succisa pratensid"]<-"Succisa pratensis"
speciesonly_df$Species[speciesonly_df$Species=="Deschampsia caespitosa"]<-"Deschampsia cespitosa"
speciesonly_df$Species[speciesonly_df$Species=="Sorbus acuparia"]<-"Sorbus aucuparia"
speciesonly_df$Species[speciesonly_df$Species=="Trifolium campastre"]<-"Trifolium campestre"
speciesonly_df$Species[speciesonly_df$Species=="Lotus pendunculatus"]<-"Lotus pedunculatus"
speciesonly_df$Species[speciesonly_df$Species=="Circaea lutetiansa"]<-"Circaea lutetiana"
speciesonly_df$Species[speciesonly_df$Species=="Artemisia vulgari"]<-"Artemisia vulgaris"
speciesonly_df$Species[speciesonly_df$Species=="Rhynchosopra megalocarpa"]<-"Rhynchospora megalocarpa"
speciesonly_df$Species[speciesonly_df$Species=="Fuirena scirpoides"]<-"Fuirena scirpoidea"
speciesonly_df$Species[speciesonly_df$Species=="Lachnocaulon beyrichianum"]<-"Lachnocaulon beyrichiana"
speciesonly_df$Species[speciesonly_df$Species=="Campanula ranunculoides"]<-"Campanula rapunculoides"
speciesonly_df$Species[speciesonly_df$Species=="Rubus ideaus"]<-"Rubus idaeus"
speciesonly_df$Species[speciesonly_df$Species=="Lathyrus paviflorus"]<-" Lathyrus pauciflorus"
speciesonly_df$Species[speciesonly_df$Species=="Taraxacum officinalis"]<-"Taraxacum officinale"
speciesonly_df$Species[speciesonly_df$Species=="Mainthemum stellatum"]<-"Maianthemum stellatum"
speciesonly_df$Species[speciesonly_df$Species=="Physocarpous malvaceus"]<-"Physocarpus malvaceus"
speciesonly_df$Species[speciesonly_df$Species=="Mainthemum racemosum"]<-"Maianthemum racemosum"
speciesonly_df$Species[speciesonly_df$Species=="Circium undulatum"]<-"Cirsium undulatum"
speciesonly_df$Species[speciesonly_df$Species=="Paxistima myrsinitis"]<-"Paxistima myrsinites"
speciesonly_df$Species[speciesonly_df$Species=="Artemesia tridentata"]<-"Artemisia tridentata"
speciesonly_df$Species[speciesonly_df$Species=="Mitellla stauropetala"]<-"Mitella stauropetala"
speciesonly_df$Species[speciesonly_df$Species=="Comandra umbellatum"]<-"Comandra umbellata"
speciesonly_df$Species[speciesonly_df$Species=="Castillea linariifolia"]<-"Castilleja linariifolia"
speciesonly_df$Species[speciesonly_df$Species=="Cerocarpous ledifolius"]<-"Cercocarpus ledifolius"
speciesonly_df$Species[speciesonly_df$Species=="Rudebeckia occidentalis"]<-"Rudbeckia occidentalis"
speciesonly_df$Species[speciesonly_df$Species=="Aster engelmanii"]<-"Aster engelmannii"
speciesonly_df$Species[speciesonly_df$Species=="Koaleria macrantha"]<-"Koeleria macrantha"
speciesonly_df$Species[speciesonly_df$Species=="Delphinium nutalianum"]<-"Delphinium nuttallianum"
speciesonly_df$Species[speciesonly_df$Species=="Lomatium greyi"]<-"Lomatium grayi"
speciesonly_df$Species[speciesonly_df$Species=="Clatonia perfoliata"]<-"Claytonia perfoliata"
speciesonly_df$Species[speciesonly_df$Species=="Chrysothamnus vicidiflorus"]<-"Chrysothamnus viscidiflorus"
speciesonly_df$Species[speciesonly_df$Species=="Ipomopsis agregatta"]<-"Ipomopsis aggregata"
speciesonly_df$Species[speciesonly_df$Species=="Physocarpos malvaceus"]<-"Physocarpus malvaceus"
speciesonly_df$Species[speciesonly_df$Species=="Thallictrum fendlerii"]<-"Thalictrum fendleri"
speciesonly_df$Species[speciesonly_df$Species=="Amelenchier alnifolia"]<-"Amelanchier alnifolia"
speciesonly_df$Species[speciesonly_df$Species=="Arrhenatherium elatius"]<-"Arrhenatherum elatius"
speciesonly_df$Species[speciesonly_df$Species=="Holcus molis"]<-"Holcus mollis"
speciesonly_df$Species[speciesonly_df$Species=="Impatiens grandiflora"]<-"Impatiens glandulifera"
speciesonly_df$Species[speciesonly_df$Species=="Luzulla campestris"]<-"Luzula campestris"
speciesonly_df$Species[speciesonly_df$Species=="Fetusca rubra"]<-"Festuca rubra"
speciesonly_df$Species[speciesonly_df$Species=="Ranculus repens"]<-"Ranunculus repens"
speciesonly_df$Species[speciesonly_df$Species=="Jacobea vulgaris"]<-"Jacobaea vulgaris"
speciesonly_df$Species[speciesonly_df$Species=="Linium teniufolium"]<-"Linum tenuifolium"
speciesonly_df$Species[speciesonly_df$Species=="Tristenum flavescens"]<-"Trisetum flavescens"
speciesonly_df$Species[speciesonly_df$Species=="Angelica silvestris"]<-"Angelica sylvestris"
speciesonly_df$Species[speciesonly_df$Species=="Engeron canadensis"]<-"Erigeron canadensis"
speciesonly_df$Species[speciesonly_df$Species=="Andropogon brachystachys"]<-"Andropogon brachystachyus"
speciesonly_df$Species[speciesonly_df$Species=="Polygonella polygama"]<-"Polygonum polygamum"
speciesonly_df$Species[speciesonly_df$Species=="Polygonella basiramium"]<-"Polygonum basiramia"
speciesonly_df$Species[speciesonly_df$Species=="Polygonum basiramium"]<-"Polygonum basiramia"
speciesonly_df$Species[speciesonly_df$Species=="Xyris stenotera"]<-"Xyris elliottii"
speciesonly_df$Species[speciesonly_df$Species=="Andropogon virginicus"]<-"Andropogon virginicum"
speciesonly_df$Species[speciesonly_df$Species=="Polygonella robusta"]<-"Polygonum nesomii"
speciesonly_df$Species[speciesonly_df$Species=="Aristida beyrichiana"]<-"Aristida stricta"
speciesonly_df$Species[speciesonly_df$Species=="Lachnocaulon beyrichiana"]<-"Lachnocaulon beyrichianum"
speciesonly_df$Species[speciesonly_df$Species=="Andropogon capillipes"]<-"Anatherum capillipes"
speciesonly_df$Species[speciesonly_df$Species=="Polygonella basiramia"]<-"Polygonum basiramium"
speciesonly_df$Species[speciesonly_df$Species=="Poa trivalis"]<-"Poa trivialis"
speciesonly_df$Species[speciesonly_df$Species=="Lachnanthes caroliniana"]<-"Lachnanthes caroliana"
speciesonly_df$Species[speciesonly_df$Species=="Andropogon virginicum"]<-"Andropogon virginicus"
speciesonly_df$Species[speciesonly_df$Species=="Carex geyerii"]<-"Carex geyeri"
speciesonly_df$Species[speciesonly_df$Species=="Achnatherum hymenoids"]<-"Achnatherum hymenoides"
speciesonly_df$Species[speciesonly_df$Species=="Lachnocaulon beyrichianum"]<-"Paepalanthus beyrichianus"
speciesonly_df$Species[speciesonly_df$Species=="Meliotus officinalis"]<-"Melilotus officinalis"

#save species df as excel file
  #write.xlsx(speciesonly_df,"invasivelist.xlsx")
#-------------------------------------------------------------------------------

## ASSIGN NATIVE VS INTRODUCED STATUS ##

#open csv with manually assigned invasive status from Kew POWO
nativestatus<-read.csv(
  file.path(data_path,"specieslist.csv"))

#first match uk only
ukonlystatus<-nativestatus%>%filter(Site=="uk"|Site=="Boothby"|Site=="Silwood Park"| 
                                      Site=="Knepp"|Site=="Budworth")%>%dplyr::select(!Site)
ukonlystatusdf<-speciesonly_df%>%filter(Site=="Boothby"|Site=="Silwood Park"| 
                                      Site=="Knepp"|Site=="Budworth")%>%
  left_join(ukonlystatus,relationship = "many-to-many")

#then match non-uk sites
nonukstatus<-nativestatus%>%filter(Site=="Ordu_urban"|Site=="Enez forest"|Site=="Archbold"| 
                                     Site=="Reinischkogel"|Site=="rhf")
nonukstatusdf<-speciesonly_df%>%filter(Site=="Ordu_urban"|Site=="Enez forest"|Site=="Archbold"| 
                                     Site=="Reinischkogel"|Site=="rhf")%>%
  left_join(nonukstatus,relationship = "many-to-many")

speciesonly_df<-rbind(ukonlystatusdf,nonukstatusdf)

#-------------------------------------------------------------------------------

## EXTRACT TRAIT DATA ##

#calculate three major traits for each species in list
species_list<-unique(speciesonly_df$Species)

#strip non-ASCII characters
species_list <- iconv(species_list, from = "", to = "ASCII", sub = "")
species_list <- gsub("[^[:alnum:] [:punct:]]", "", species_list)


heightdf<-BIEN_trait_mean(species=species_list, trait="whole plant height")
SLAdf<-BIEN_trait_mean(species=species_list, trait="leaf area per leaf dry mass")
seedmassdf<-BIEN_trait_mean(species=species_list, trait="seed mass")

height<-heightdf%>%dplyr::select(species,mean_value)%>%rename(Height=mean_value)
SLA<-SLAdf%>%dplyr::select(species,mean_value)%>%rename(SLA=mean_value)
seedmass<-seedmassdf%>%dplyr::select(species,mean_value)%>%
  rename(SeedMass=mean_value)

traits_mean<-SLAdf%>%dplyr::select(species,mean_value)%>%
  rename(SLA=mean_value)%>%
  left_join(height)%>%left_join(seedmass)%>%
  rename(Species=species)


  #write.csv(height,"intermediates/height_traitmean.csv")
  #write.csv(SLA,"intermediates/SLA_traitmean.csv")
  #write.csv(seedmass,"intermediates/seedmass_traitmean.csv")

traitdata<-speciesonly_df%>%left_join(traits_mean,relationship="many-to-many")


## SAVE TRAIT DATA ##

write.csv(traitdata,
  file=file.path(data_path,"intermediates","fractal_traits_mean.csv"))

#end