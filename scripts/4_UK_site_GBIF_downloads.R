
### DOWNLOAD UK AND LANDSCAPE SCALE DATA FROM GBIF ###

#-------------------------------------------------------------------------------

user<-"realrona"
pwd<-"GRACE-4virtue"
email<-"rel22@ic.ac.uk"

#-------------------------------------------------------------------------------
### SILWOOD ###

#search, check data exists
occ_search(kingdomKey =1,country="GB",hasCoordinate = TRUE,
           decimalLatitude="51.3493,51.4383",
           decimalLongitude="-0.6919,-0.6025",
           year="2021,2023",limit=100)

#download all plant data within coord range
silwood_download<-occ_download(pred_lte("decimalLatitude",51.4383),
                            pred_gte("decimalLatitude",51.3493),
                            pred_or(pred("year",2023),pred("year",2022),pred("year",2021)),
                            pred_lte("decimalLongitude",-0.6025),
                            pred_gte("decimalLongitude",-0.6919),
                            format="SIMPLE_CSV",
                            user="realrona",
                            pwd="GRACE-4virtue",
                            email="rel22@ic.ac.uk")

#check wait time
occ_download_wait(silwood_download)

#save data
d<-occ_download_get(silwood_download)%>%occ_download_import()

#limit to plants (didnt work in download)
d<-d%>%filter(kingdom=="Plantae")

#save as csv file
write.csv(d, 
          file = file.path(data_path,"gbifdata","silwoodplantdata2.csv"))


#-------------------------------------------------------------------------------
### KNEPP ###

#search, check data exists
occ_search(kingdomKey =1,country="GB",hasCoordinate = TRUE,
           decimalLatitude="50.9389,51.0279",
           decimalLongitude="-0.4350,-0.3102",
           year="2022,2023",limit=100)

#download all plant data within coord range
knepp_download<-occ_download(pred_lte("decimalLatitude",51.0279),
                            pred_gte("decimalLatitude",50.9389),
                            pred_or(pred("year",2023),pred("year",2022)),
                            pred_lte("decimalLongitude",-0.3102),
                            pred_gte("decimalLongitude",-0.4350),
                            format="SIMPLE_CSV",
                            user="realrona",
                            pwd="GRACE-4virtue",
                            email="rel22@ic.ac.uk")

#check wait time
occ_download_wait(knepp_download)

#save data
d<-occ_download_get(knepp_download)%>%occ_download_import()

d<-d%>%filter(kingdom=="Plantae")

#save as csv file
write.csv(d, 
          file = file.path(data_path,"gbifdata","kneppplantdata2.csv"))


#-------------------------------------------------------------------------------
### BOOTHBY ###

#search, check data exists
occ_search(kingdomKey =1,country="GB",hasCoordinate = TRUE,
           decimalLatitude="52.9498,53.0388",
           decimalLongitude="-0.5961,-0.5067",
           year="2022,2023",limit=100)

#download all plant data within coord range
boothby_download<-occ_download(pred_lte("decimalLatitude",53.0388),
                             pred_gte("decimalLatitude",52.9498),
                             pred_or(pred("year",2023),pred("year",2022)),
                             pred_lte("decimalLongitude",-0.5067),
                             pred_gte("decimalLongitude",-0.5961),
                             format="SIMPLE_CSV",
                             user="realrona",
                             pwd="GRACE-4virtue",
                             email="rel22@ic.ac.uk")

#check wait time
occ_download_wait(boothby_download)

#save data
d<-occ_download_get(boothby_download)%>%occ_download_import()

d<-d%>%filter(kingdom=="Plantae")

#save as csv file
write.csv(d, 
          file = file.path(data_path,"gbifdata","boothbyplantdata2.csv"))


#-------------------------------------------------------------------------------
### BUDWORTH ###

#search, check data exists
occ_search(kingdomKey =1,country="GB",hasCoordinate = TRUE,
           decimalLatitude="53.2274,53.3164",
           decimalLongitude="-2.5710,-2.4200",
           year="2022",limit=100)


#download all plant data within coord range
budworth_download<-occ_download(pred_lte("decimalLatitude",53.3164),
                               pred_gte("decimalLatitude",53.2274),
                               pred_gte("year",2013),
                               pred_lte("decimalLongitude",-2.4200),
                               pred_gte("decimalLongitude",-2.5710),
                               format="SIMPLE_CSV",
                               user="realrona",
                               pwd="GRACE-4virtue",
                               email="rel22@ic.ac.uk")

#check wait time
occ_download_wait(budworth_download)

#save data
d<-occ_download_get(budworth_download)%>%occ_download_import()

d<-d%>%filter(kingdom=="Plantae")

#save as csv file
write.csv(d, 
          file = file.path(data_path,"gbifdata","budworthplantdata2.csv"))

#-------------------------------------------------------------------------------
### COMPLETE UK ###

#search, check data exists
occ_search(kingdomKey =1,country="GB",hasCoordinate = TRUE,
           year="2023",limit=100)

#download all plant data within coord range
uk_download<-occ_download(pred("country","GB"),
                                pred("year",2022),
                                format="SIMPLE_CSV",
                                user="realrona",
                                pwd="GRACE-4virtue",
                                email="rel22@ic.ac.uk")

#check wait time
occ_download_wait(uk_download)

#save data
d<-occ_download_get(uk_download)%>%occ_download_import()

d<-d%>%filter(kingdom=="Plantae")

#save as csv file
write.csv(d, 
          file = file.path(data_path,"gbifdata","ukplantdata.csv"))

#end
