### ALL MODELS AND FIGURES ###

#-------------------------------------------------------------------------------
#set up


#-------------------------------------------------------------------------------


fractaldata<-read.csv(
  file.path(data_path, "intermediates","trait_csmspd_plants.csv"))%>%
  select(!X)%>%
  filter(Species!="Taxus baccata" & Species!="Pteridium aquilinum")

# site data
landscapedata<-read.csv(
  file.path(data_path,"gbifdata","landscape_scale_data.csv"))%>%
  select(!X)

ukdata<-read.csv(
  file.path(data_path,"gbifdata","ukfulldata.csv"))

# ASSIGN NATIVE VS INTRODUCED STATUS #
nativestatus <- read.csv(
  file.path(data_path,"specieslist.csv"))

# first match uk only
ukonlystatus <- nativestatus %>%
  filter(Site == "uk" | Site == "Boothby" | Site == "Silwood Park" |
           Site == "Knepp" | Site == "Budworth") %>%
  select(!Site) %>%
  rename(species = Species)

landscapescbd <- landscapedata %>% left_join(ukonlystatus, relationship = "many-to-many")

ukscbd <- ukdata %>% left_join(ukonlystatus, relationship = "many-to-many")
ukscbd$Introduced_status[ukscbd$Introduced_status=="introduced"]<-"Introduced"

#-------------------------------------------------------------------------------
# CALCULATE SPECIES RICHNESS FOR BETA REG MODELS
#-------------------------------------------------------------------------------

#small fractal
nspeciesper_smallfractal<-fractaldata%>%
  group_by(Site,Year,Plot)%>%
  dplyr::select(Species)%>%distinct()%>%
  summarise(small_species_n=n())%>%
  ungroup()

#mdium fractal
nspeciesper_medfractal<-fractaldata%>%
  group_by(Site,Year,Majortriad,Minortriad)%>%
  dplyr::select(Species)%>%distinct()%>%
  summarise(med_species_n=n())%>%
  ungroup()

#large fractal
nspeciesper_largefractal<-fractaldata%>%
  group_by(Site,Year,Majortriad)%>%
  dplyr::select(Species)%>%distinct()%>%
  summarise(large_species_n=n())%>%
  ungroup()

#landscape
nspeciesper_landscape<-landscapedata%>%
  group_by(site,year,plot)%>%
  dplyr::select(species)%>%distinct()%>%
  summarise(species_richness=n())%>%
  ungroup()

#uk
nspeciesper_ukgrid<-ukdata%>%
  group_by(plot)%>%
  dplyr::select(species)%>%distinct()%>%
  summarise(species_richness=n())%>%
  ungroup()


#add each count to modelling data
sprich_data<-fractaldata%>%left_join(nspeciesper_smallfractal,relationship="many-to-many")%>%
  left_join(nspeciesper_medfractal,relationship="many-to-many")%>%
  left_join(nspeciesper_largefractal,relationship="many-to-many")


#-------------------------------------------------------------------------------

## FRACTALS - LARGE ##

#z-scale continuous variables
sprich_data$scale.csMSPD<-as.numeric(scale(sprich_data$csMSPD))
sprich_data$scale.SLA<-as.numeric(scale(sprich_data$SLA))
sprich_data$scale.Height<-as.numeric(scale(sprich_data$Height))
sprich_data$scale.log.seedmass<-as.numeric(scale(log(sprich_data$SeedMass)))

#relevel introduction status 
sprich_data$Introduced_status<-as.factor(sprich_data$Introduced_status)
sprich_data <- within(sprich_data, Introduced_status <- relevel(Introduced_status, ref = "Native"))

#try a maximal model
#remove na values
fractallarge<-sprich_data%>%na.omit(Beta_contrib_major)%>%
  rename(species_richness=large_species_n)

#build model
largemod<-betareg(Beta_contrib_major~
                    scale.SLA*Introduced_status+
                    scale.Height*Introduced_status+
                    scale.csMSPD*Introduced_status+
                    scale.log.seedmass*Introduced_status+
                    species_richness+Site,
                  na.action=na.fail,data=fractallarge)

#dredge model subsets
largemodels<-model.avg(dredge(largemod))
largemod2<-dredge(largemod)


vif(largemod)
largemodsum<-summary(model.avg(dredge(largemod),subset=delta<4,fit=TRUE))

#get AIC importance values
largemodsw<-sw(model.avg(largemod2,subset=delta<4,fit=TRUE))


#-------------------------------------------------------------------------------

## FRACTALS MEDIUM ##

#try a maximal model
fractalmedium <- sprich_data%>%select(scale.Height,scale.SLA,scale.csMSPD,scale.log.seedmass,med_species_n,
                                      Introduced_status,Species,Site,Plot,Quadrant,Cover,Year,Majortriad,
                                      Minortriad,Beta_contrib_minor)%>%na.omit()
fractalmedium<-fractalmedium%>%filter(Beta_contrib_minor!=0)%>%
  rename(species_richness=med_species_n)

#build model
mediummod<-betareg(Beta_contrib_minor~
                     scale.SLA*Introduced_status+
                     scale.Height*Introduced_status+
                     scale.csMSPD*Introduced_status+
                     scale.log.seedmass*Introduced_status+
                     species_richness+Site,
                   na.action=na.fail,data=fractalmedium)

#dredge model subsets
medmodels<-model.avg(dredge(mediummod))
mediummod2<-dredge(mediummod)


mediummodsum<-summary(model.avg(mediummod2,subset=delta<4,fit=TRUE))
vif(mediummod)
#get AIC importance values
mediummodsw<-(model.avg(mediummod2,subset=delta<4,fit=TRUE))

#-------------------------------------------------------------------------------

## FRACTALS SMALL ##

fractalsmall<-sprich_data%>%na.omit(Beta_contrib_plot)

#filter for 0 values
fractalsmall<-fractalsmall%>%filter(Beta_contrib_plot!=0)%>%
  rename(species_richness=small_species_n)

#relevel introduction status 
fractalsmall <- within(fractalsmall, Introduced_status <- relevel(Introduced_status, ref = "Native"))

#build model
smallmod<-betareg(Beta_contrib_plot~
                    scale.SLA*Introduced_status+
                    scale.Height*Introduced_status+
                    scale.csMSPD*Introduced_status+
                    scale.log.seedmass*Introduced_status+
                    species_richness+Site,
                  na.action=na.fail,data=fractalsmall)

#dredge model subsets
smallmodels<-model.avg(dredge(smallmod))
vif(smallmod)
smallmod2<-dredge(smallmod)
smallmodsum<-summary(model.avg(smallmod2,subset=delta<4,fit=TRUE))

#get AIC importance values
smallmodsw<-sw(model.avg(smallmod2,subset=delta<4,fit=TRUE))

#-------------------------------------------------------------------------------
# LANDSCAPE SCALE

#landscape
landscapedata<-landscapedata%>%
  left_join(nspeciesper_landscape,relationship="many-to-many")%>%
  left_join(ukonlystatus, relationship = "many-to-many")

#remove unknown introducedstatus
landscapedata<-landscapedata%>%na.omit(Introduced_status)

#z-scale continuous variables
landscapedata$scale.csMSPD<-as.numeric(scale(landscapedata$csMSPD))
landscapedata$scale.SLA<-as.numeric(scale(landscapedata$SLA))
landscapedata$scale.Height<-as.numeric(scale(landscapedata$Height))
landscapedata$scale.log.seedmass<-as.numeric(scale(log(landscapedata$Seed_mass)))

#relevel introduction status 
landscapedata$Introduced_status<-as.factor(landscapedata$Introduced_status)
landscapedata <- within(landscapedata, Introduced_status <- relevel(Introduced_status, ref = "Native"))

#build model
landscapemod<-betareg(beta_contrib~
                        scale.SLA*Introduced_status+
                        scale.Height*Introduced_status+
                        scale.log.seedmass*Introduced_status+
                        scale.csMSPD*Introduced_status+
                        species_richness+site,
                      na.action=na.fail,data=landscapedata)

#dredge model subsets
landscapemodels<-model.avg(dredge(landscapemod))
landscapemod2<-dredge(landscapemod)

vif(landscapemod)
landscapemodsum<-summary(model.avg(landscapemod2,subset=delta<4,fit=TRUE))

#get AIC importance values
landscapemodsw<-sw(model.avg(landscapemod2,subset=delta<4,fit=TRUE))

#-------------------------------------------------------------------------------
## UK DATA ##

ukdata<-ukscbd%>%
  left_join(nspeciesper_ukgrid,relationship="many-to-many")

#remove unknown introducedstatus
ukdata<-ukdata%>%na.omit(Introduced_status)
ukdata<-ukdata%>%na.omit(beta_contrib)

#z-scale continuous variables
ukdata$scale.csMSPD<-as.numeric(scale(ukdata$csMSPD))
ukdata$scale.SLA<-as.numeric(scale(ukdata$SLA))
ukdata$scale.Height<-as.numeric(scale(ukdata$height))
ukdata$scale.log.seedmass<-as.numeric(scale(log(ukdata$Seed_mass)))


#relevel introduction status 
ukdata$Introduced_status<-as.factor(ukdata$Introduced_status)
ukdata$Introduced_status[ukdata$Introduced_status=="introduced"]<-"Introduced"
ukdata <- within(ukdata, Introduced_status <- relevel(Introduced_status, ref = "Native"))

#build model
ukmod<-betareg(beta_contrib~
                 scale.SLA*Introduced_status+
                 scale.Height*Introduced_status+
                 scale.log.seedmass*Introduced_status+
                 scale.csMSPD*Introduced_status+
                 species_richness,
               na.action=na.fail,data=ukdata)

ukmodels<-model.avg(dredge(ukmod))
ukmod2<-dredge(ukmod)

vif(ukmod)
ukmodsum<-summary(model.avg(ukmod2,subset=delta<4,fit=TRUE))

#get AIC importance values
ukmodsw<-sw(model.avg(ukmod2,subset=delta<4,fit=TRUE))

#-------------------------------------------------------------------------------
# SAVE MODELS

saveRDS(smallmod,
        file = file.path(models_path,"small_mod_species_richness.RDS"))
saveRDS(mediummod,
        file = file.path(models_path,"medium_mod_species_richness.RDS"))
saveRDS(largemod,
        file = file.path(models_path,"large_mod_species_richness.RDS"))
saveRDS(landscapemod,
        file = file.path(models_path,"landscape_mod_species_richness.RDS"))
saveRDS(ukmod,
        file = file.path(models_path,"uk_mod_species_richness.RDS"))

#-------------------------------------------------------------------------------

## JOIN AND PREP MODEL OUTPUTS ##

smallmod2 <-
  broom::tidy(smallmod) %>% 
  filter(!grepl("^Site", term)) %>% 
  filter(term != "(Intercept)" & term != "(phi)") %>% 
  mutate(model = "Small \nfractal")

mediummod2 <-
  broom::tidy(mediummod) %>% 
  filter(!grepl("^Site", term)) %>% 
  filter(term != "(Intercept)" & term != "(phi)") %>% 
  mutate(model = "Medium \nfractal")

largemod2 <-
  broom::tidy(largemod) %>% 
  filter(!grepl("^Site", term)) %>% 
  filter(term != "(Intercept)" & term != "(phi)") %>% 
  mutate(model = "Large \nfractal")

landscapemod2 <-
  broom::tidy(landscapemod) %>% 
  filter(!grepl("^site", term)) %>% 
  filter(term != "(Intercept)" & term != "(phi)") %>% 
  mutate(model = "Landscape \nScale")

ukmod2 <-
  broom::tidy(ukmod) %>% 
  filter(!grepl("^site", term)) %>% 
  filter(term != "(Intercept)" & term != "(phi)") %>% 
  mutate(model = "UK")


#bind tidied models 
allmods<-rbind(smallmod2,mediummod2,largemod2,landscapemod2,ukmod2)

#rename wonky variables
allmods$term[allmods$term=="scale.csMSPD"]<-"scale.csMSPD"
allmods$term[allmods$term=="scale.log.seedmass"]<-"scale
log(Seed Mass)"
allmods$term[allmods$term=="Introduced_statusIntroduced:scale.csMSPD"]<-"scale.csMSPD x
Introduced"
allmods$term[allmods$term=="scale.SLA:Introduced_statusIntroduced"]<-"scale.SLA x
Introduced"
allmods$term[allmods$term=="Introduced_statusIntroduced:scale.Height"]<-"scale.Height x
Introduced"
allmods$term[allmods$term=="Introduced_statusIntroduced:scale.log.seedmass"]<-"scale
log(Seed Mass)
x Introduced"
allmods$term[allmods$term=="Introduced_statusIntroduced"]<-"Introduced"
allmods$term[allmods$term=="species_richness"]<-"Species
richness"

#-------------------------------------------------------------------------------

## MAKE FIGURE ##
#viridis broken - make colour palette
colviridis<-c("#fde725","#5ec962","#21918c","#3b528b","#440154")

species_rich_mainplot<-dwplot(allmods,dot_args = list(size=3),
                              vline = geom_vline(
                                xintercept = 0,
                                colour = "grey60",
                                linetype = 2)) +
  scale_colour_manual(values=colviridis,breaks = c("Small \nfractal", "Medium \nfractal","Large \nfractal","Landscape \nScale","UK"),
                      labels = c("Small \nfractal", "Medium \nfractal","Large \nfractal","Landscape \nScale","UK"))+
  theme_classic() + 
  # Setting base_size for fit the theme
  # No need to set base_size in most usage
  xlab("Coefficient Estimate") + ylab("") +
  geom_vline(xintercept = 0,
             colour = "grey60",
             linetype = 2) +
  theme(
    legend.position = "top",
    legend.justification = c(0, 0),
    legend.background = element_rect(colour = "grey60"),
    legend.title = element_blank())+
  theme(text = element_text(size=13),
        axis.title.y = element_text(margin = margin(r = 20)))+
  ylab("Species-\u03B2")

species_rich_mainplot

ggsave(species_rich_mainplot, 
       filename = file.path(figures_path,"mainplot_species_richness.pdf"),
       device = "pdf",
       height = 8, width = 7, units = "in")

#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------

### PARTIAL REGRESSION seed mass PLOTS ###

#- make model for all factors but focal fixed and interaction #

## SMALL FRACTAL ##

#create df with only rows with contain all values
smalldatanona <- sprich_data %>%
  drop_na(Beta_contrib_plot, SLA, SeedMass, Height, Introduced_status, csMSPD)%>%
  filter(Introduced_status!="")
smalldatanona$Beta_contrib_plot <-smalldatanona$Beta_contrib_plot+0.00000000000000000001

#create model excluding seed mass
smallmspdmod<-betareg(Beta_contrib_plot ~
                        scale.csMSPD*Introduced_status+
                        scale.SLA*Introduced_status+
                        scale.Height*Introduced_status+
                        small_species_n+Site,
                      data=smalldatanona)
#save residuals
smallmspdresid<-resid(smallmspdmod)

#add residuals to df
smalldatanona$corrected.scbd<-smallmspdresid



## MEDIUM FRACTAL ##

#create df with only rows with contain all values
mediumdatanona<-sprich_data%>%drop_na(Beta_contrib_minor,SLA,SeedMass,Height,Introduced_status,csMSPD)%>%
  filter(Introduced_status!="")
mediumdatanona$Beta_contrib_minor<-mediumdatanona$Beta_contrib_minor+0.00000000000000000001

#create model excluding seed mass
mediummspdmod<-betareg(Beta_contrib_minor~
                         scale.csMSPD*Introduced_status+
                         scale.SLA*Introduced_status+
                         scale.Height*Introduced_status+
                         med_species_n+Site,
                       data=mediumdatanona)
#save residuals
mediummspdresid<-resid(mediummspdmod)

#add residuals to df
mediumdatanona$corrected.scbd<-mediummspdresid


## LARGE TRIAD ##
#create df with only rows with contain all values
largedatanona<-sprich_data%>%
  drop_na(Beta_contrib_major,SLA,scale.log.seedmass,Height,Introduced_status,csMSPD)%>%
  filter(Introduced_status!="")

#create model excluding seed mass
largemspdmod<-betareg(Beta_contrib_major~
                        scale.csMSPD*Introduced_status+
                        scale.SLA*Introduced_status+
                        scale.Height*Introduced_status+
                        large_species_n+Site,
                      data=largedatanona)
#save residuals
largemspdresid<-resid(largemspdmod)

#add residuals to df
largedatanona$corrected.scbd<-largemspdresid


## LANDSCAPE SCALE ##

#create df with only rows with contain all values
landscapedatanona<-landscapedata%>%
  drop_na(beta_contrib,SLA,Seed_mass ,Height,Introduced_status,csMSPD)

#create model excluding seed mass
landscapemspdmod<-betareg(beta_contrib~
                            scale.csMSPD*Introduced_status+
                            scale.SLA*Introduced_status+
                            scale.Height*Introduced_status+
                            species_richness+Site,
                          data=landscapedatanona)
#save residuals
landscapemspdresid<-resid(landscapemspdmod)

#add residuals to df
landscapedatanona$corrected.scbd<-landscapemspdresid


## UK ##

#create df with only rows with contain all values
uknona<-ukdata%>%drop_na(beta_contrib,SLA,Seed_mass ,height,Introduced_status,csMSPD)

#create model excluding seed mass
ukmspdmod<-betareg(beta_contrib~
                     scale.csMSPD*Introduced_status+
                     scale.SLA*Introduced_status+
                     scale.Height*Introduced_status+
                     species_richness+Site,
                   data=uknona)
#save residuals
ukmspdresid<-resid(ukmspdmod)

#add residuals to df
uknona$corrected.scbd<-ukmspdresid

#make axis limits
x_limits <- c(-2, 4)
y_limits <- c(-8, 2.5)

#make plots
smallplot<-ggplot(data=smalldatanona,aes(x=(scale.log.seedmass),y=(corrected.scbd),col=Introduced_status))+
  geom_point(size=0.8,alpha=0.4)+
  scale_color_manual(values=c("#5ec962","#440154"))+
  geom_smooth(method="lm",formula=y~x,se=FALSE)+
  theme_classic()+
  xlab("z-scale(log(Seed Mass (g)))")+
  ylab("Corrected (z-scale) Species-\u03B2")+ 
  guides(color=guide_legend(title="Status"))+
  ggtitle("Small fractal")+
  scale_x_continuous(limits = x_limits) +
  scale_y_continuous(limits = y_limits)

mediumplot<-ggplot(data=mediumdatanona,aes(x=(scale.log.seedmass),y=(corrected.scbd),col=Introduced_status))+
  geom_point(size=0.8,alpha=0.4)+
  scale_color_manual(values=c("#5ec962","#440154"))+
  geom_smooth(method="lm",formula=y~x,se=FALSE)+
  theme_classic()+
  xlab("z-scale(log(Seed Mass (g)))")+
  ylab("Corrected (z-scale) Species-\u03B2")+ 
  guides(color=guide_legend(title="Status"))+
  ggtitle("Medium fractal")+
  scale_x_continuous(limits = x_limits) +
  scale_y_continuous(limits = y_limits)

largeplot<-ggplot(data=largedatanona,aes(x=(scale.log.seedmass),y=(corrected.scbd),col=Introduced_status))+
  geom_point(size=0.8,alpha=0.4)+
  scale_color_manual(values=c("#5ec962","#440154"))+
  geom_smooth(method="lm",formula=y~x,se=FALSE)+
  theme_classic()+
  xlab("z-scale(log(Seed Mass (g)))")+
  ylab("Corrected (z-scale) Species-\u03B2")+   
  guides(color=guide_legend(title="Status"))+
  ggtitle("Large fractal")+
  scale_x_continuous(limits = x_limits) +
  scale_y_continuous(limits = y_limits)

landscapeplot<-ggplot(data=landscapedatanona,aes(x=(scale.log.seedmass),y=(corrected.scbd),col=Introduced_status))+
  geom_point(size=0.8,alpha=0.4)+
  scale_color_manual(values=c("#5ec962","#440154"))+
  geom_smooth(method="lm",formula=y~x,se=FALSE)+
  theme_classic()+
  xlab("z-scale(log(Seed Mass (g)))")+
  ylab("Corrected (z-scale) Species-\u03B2")+ 
  guides(color=guide_legend(title="Status"))+
  ggtitle("Landscape \n scale")+
  scale_x_continuous(limits = x_limits) +
  scale_y_continuous(limits = y_limits)

ukplot<-ggplot(data=uknona,aes(x=(scale.log.seedmass),y=(corrected.scbd),col=Introduced_status))+
  geom_point(size=0.8,alpha=0.4)+
  scale_color_manual(values=c("#5ec962","#440154"))+
  geom_smooth(method="lm",formula=y~x,se=FALSE)+
  theme_classic()+
  xlab("z-scale(log(Seed Mass (g)))")+
  ylab("Corrected (z-scale) Species-\u03B2")+ 
  guides(color=guide_legend(title="Status"))+
  ggtitle("UK")+
  scale_x_continuous(limits = x_limits) +
  scale_y_continuous(limits = y_limits)


#join plots into one compound plot
partial_regression_seedmass<-ggarrange(smallplot, mediumplot, largeplot, landscapeplot,ukplot,
                                       align='v', labels=NULL,
                                       common.legend = T,legend="bottom")

ggsave(partial_regression_seedmass, 
       filename = file.path(figures_path,"partial_regression_seedmass_speciesrichness.pdf"),
       device = "pdf",
       height = 9, width = 8, units = "in")

