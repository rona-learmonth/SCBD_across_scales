## Effect of standardised rank SCBD 
# Supplementary Materials 

rm(list=ls())

originaldata<-read.csv(
  file.path(data_path, "intermediates","trait_csmspd_plants.csv"))%>%
  select(!X)%>%
  filter(Species!="Taxus baccata" & Species!="Pteridium aquilinum")

# site data
landscapedata<-read.csv(
  file.path(data_path, "gbifdata","landscape_scale_data.csv"))%>%select(!X)

ukdata<-read.csv(
  file.path(data_path, "gbifdata","ukfulldata.csv"))

# ASSIGN NATIVE VS INTRODUCED STATUS #
nativestatus <- read.csv(
  file.path(data_path, "specieslist.csv"))

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
# A) CALCULATE RELATIVIZED RANK SCBD
#-------------------------------------------------------------------------------

relativize_scbd <- function(data, scbd_col, group_vars) {
  scbd_sym <- rlang::sym(scbd_col)
  rel_col  <- paste0(scbd_col, "_relrank")
  
  data %>%
    group_by(across(all_of(group_vars)), Species, Year) %>%
    summarise(
      SCBD = mean(!!scbd_sym, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    group_by(across(all_of(group_vars)), Year) %>%
    mutate(
      rank     = rank(SCBD, ties.method = "average"),
      max_rank = max(rank, na.rm = TRUE),          # CHANGED: use max(rank) to guarantee top = 1
      !!rel_col := rank / max_rank
    ) %>%
    ungroup() %>%
    select(all_of(group_vars), Year, Species, !!rel_col)
}


# -----------------------------------------------------------------------------

# Small fractal: group by Site + Minortriad
rel_small  <- relativize_scbd(originaldata, "Beta_contrib_plot",  c("Site", "Minortriad"))  # CHANGED

# Medium fractal: group by Site + Majortriad
rel_medium <- relativize_scbd(originaldata, "Beta_contrib_minor", c("Site", "Majortriad"))  # CHANGED

# Large fractal: group by Site
rel_large  <- relativize_scbd(originaldata, "Beta_contrib_major", c("Site"))               # CHANGED

# Join back — ensure join keys match the grouping variables used above
originaldata_rel <- originaldata %>%
  left_join(rel_small,  by = c("Site", "Minortriad", "Year", "Species")) %>%   # CHANGED
  left_join(rel_medium, by = c("Site", "Majortriad", "Year", "Species")) %>%  # CHANGED
  left_join(rel_large,  by = c("Site", "Year", "Species"))                   # CHANGED

# Landscape and UK scale (no change in logic, but use max_rank approach)
landscapescbd <- landscapescbd %>%
  group_by(site, year) %>%
  mutate(
    rank     = rank(beta_contrib, ties.method = "average"),
    max_rank = max(rank, na.rm = TRUE),   # CHANGED: use max(rank)
    relrank  = rank / max_rank
  ) %>%
  ungroup()

ukscbd <- ukscbd %>%
  group_by(site, year) %>%
  mutate(
    rank     = rank(beta_contrib, ties.method = "average"),
    max_rank = max(rank, na.rm = TRUE),   # CHANGED: use max(rank)
    relrank  = rank / max_rank
  ) %>%
  ungroup()

#-------------------------------------------------------------------------------
# Build plotting dataset (new scale names)
#-------------------------------------------------------------------------------

plotdata <- bind_rows(
  originaldata_rel %>%
    transmute(Site, Scale = "Small_fractal",
              Raw_SCBD = Beta_contrib_plot,
              RelRank  = Beta_contrib_plot_relrank),            
  originaldata_rel %>%
    transmute(Site, Scale = "Medium_fractal",
              Raw_SCBD = Beta_contrib_minor,
              RelRank  = Beta_contrib_minor_relrank),
  originaldata_rel %>%
    transmute(Site, Scale = "Large_fractal",
              Raw_SCBD = Beta_contrib_major,
              RelRank  = Beta_contrib_major_relrank)
)

landscape_plotdata <- landscapescbd %>%
  transmute(Site = site, Scale = "Landscape",
            Raw_SCBD = beta_contrib,
            RelRank  = relrank)
landscape_plotdata$Site[landscape_plotdata$Site == 'Silwood'] <- 'Silwood Park'


uk_plotdata <- ukscbd %>%
  transmute(Site = site, Scale = "UK",
            Raw_SCBD = beta_contrib,
            RelRank  = relrank)

plotdata_all <- bind_rows(plotdata, landscape_plotdata, uk_plotdata)

# **Set factor order and labels**
plotdata_all$Scale <- factor(
  plotdata_all$Scale,
  levels = c("Small_fractal", "Medium_fractal", "Large_fractal", "Landscape", "UK"),
  labels = c("Small \n fractal", "Medium \n fractal", "Large \n fractal", "Landscape", "UK")
)

#-------------------------------------------------------------------------------
# PLOTTING FUNCTION FOR RELATIVE RANK
#-------------------------------------------------------------------------------

all_sites <- c(
  "Boothby","Budworth","Knepp","Silwood Park","Enez forest","Ordu_urban",
  "Archbold","Reinischkogel","rhf","Silwood","UK"
)

# pick *your* colours here — example:
site_cols <- c(
  Boothby = "#E69F00",
  Budworth = "#56B4E9",
  Knepp = "#009E73",
  `Silwood Park` = "#F0E442",
  `Enez forest` = "#0072B2",
  Ordu_urban = "#D55E00",
  Archbold = "#CC79A7",
  Reinischkogel = "#F564E3",
  rhf = "#B2DF8A",
  Silwood = "#A67C00",     # landscape "Silwood"
  UK = "grey40"            # ensure UK is grey
)


make_plot <- function(scale_name) {
  ggplot(filter(plotdata_all, Scale == scale_name),
         aes(x = RelRank, y = Raw_SCBD, color = Site)) +
    geom_smooth(se = FALSE) +
    scale_color_manual(values = site_cols, drop = FALSE) +   # <– important!
    theme_classic(base_size = 12) +
    labs(
      x = "Relativized rank \n SCBD (0–1)",
      y = "Raw SCBD",
      color = "Site",
      title = scale_name
    ) +
    theme(
      legend.position = "none",
      plot.title = element_text(hjust = 0.5, face = "bold")
    )
}

p_small      <- make_plot("Small \n fractal")
p_medium     <- make_plot("Medium \n fractal")
p_large      <- make_plot("Large \n fractal")
p_landscape  <- make_plot("Landscape")
p_uk         <- make_plot("UK")

legend <- get_legend(
  ggplot(filter(plotdata_all, Scale == "Small \n fractal"),
         aes(x = RelRank, y = Raw_SCBD, color = Site)) +
    geom_point(alpha = 0.7, size = 2) +
    scale_color_manual(values = site_cols, drop = FALSE) +
    theme_classic(base_size = 12) +
    theme(legend.position = "bottom")
)


combined_plot <- plot_grid(
  plot_grid(p_small, p_medium, p_large, p_landscape, p_uk,
            nrow = 1, labels = c("A", "B", "C", "D", "E"), label_size = 14),
  legend,
  ncol = 1,
  rel_heights = c(1, 0.12)
)


ggsave(combined_plot,
       filename = file.path(figures_path,"SI_raw_vs_relativised_rank_scbd.pdf",
       device="pdf",
       height=8,width=10,units="in"))

