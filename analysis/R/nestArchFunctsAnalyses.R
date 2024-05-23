####################################################################################################################
## Author: GREG CHISM
## Date: July 2023
## email: gchism94@gmail.com
## Project: Nest shape influences colony organization in ants: spatial distribution and connectedness of colony members differs from that predicted by random movement and is affected by available space
## Title: Calculating colony member proportions in nest sections, all paper analyses and figures 
####################################################################################################################

# This code is to replicate all analyses and figures for the "Nest shape influences colony organization in ants: spatial distribution and connectedness of colony members differs from that predicted by random movement and is affected by available space"

# READ ME: 
# CODE BELOW READS IN PROCESSED DATA FROM THE FOLLOWING SCRIPTS:
# (1) binsWorking.R - Binning data and then calculating the proportions of each in the binned sections
# (2) distanceFunctions.R - Distance to the nest entrance for all data, mobile colony member distance to the brood center, worker distance to all nest sections but their own
# (3) fidelityZonesFunctions.R - Paint-marked worker site fidelity zones

if (!require(pacman)) install.packages('pacman') # Download package with function to load multiple packaged at once
####################################################################################################################
# Loading required packages for code below  
# p_load() will download packages that aren't in system library
####################################################################################################################

pacman::p_load(assertthat,
               colorspace,
               cowplot,
               ggdist,
               ggpointdensity,
               ggpubr,
               ggside,
               ggthemes,
               gt,
               here,
               lme4,
               lmerTest,
               scales,
               sjPlot,
               tidyverse,
               viridis,
               webshot)

pacman::p_load_gh('BoulderCodeHub/CRSSIO',
                  'Ifeanyi55/Convert2Docx')

#Convert2Docx::install_engine()

webshot::install_phantomjs(force = TRUE)

####################################################################################################################
# IMPORT ALL NECESSARY DATASETS 
# This code imports all necessary data sets for the script below
####################################################################################################################

# PROCESSED DATASET FROM binsWorking.R
# Full workers dataset
FullDataCoordWorkersRD1_RD2 <- read.csv(here("analysis", "data", "processed", "FullDataCoordWorkersRD1_RD2.csv"), row.names = NULL) 

# Workers dataset high density
FullDataCoordWorkers <- read.csv(here("analysis", "data", "processed", "FullDataCoordWorkers.csv"))

# Workers dataset low density
FullDataCoordWorkersRD2 <- read.csv(here("analysis", "data", "processed", "FullDataCoordWorkersRD2.csv"))

# Full workers dataset
FullDataCoordBroodRD1_RD2 <- read.csv(here("analysis", "data", "processed", "FullDataCoordBroodRD1_RD2.csv"))

# Workers dataset high density
FullDataCoordBrood <- read.csv(here("analysis", "data", "processed", "FullDataCoordBrood.csv"))

# Workers dataset low density
FullDataCoordBroodRD2 <- read.csv(here("analysis", "data", "processed", "FullDataCoordBroodRD2.csv"))

# Full workers dataset
FullDataCoordQueenRD1_RD2 <- read.csv(here("analysis", "data", "processed", "FullDataCoordQueenRD1_RD2.csv"))

# Workers dataset high density
FullDataCoordQueen <- read.csv(here("analysis", "data", "processed", "FullDataCoordQueen.csv"))

# Workers dataset low density
FullDataCoordQueenRD2 <- read.csv(here("analysis", "data", "processed", "FullDataCoordQueenRD2.csv"))

# Workers dataset low density
FullDataCoordAlates <- read.csv(here("analysis", "data", "processed", "FullDataCoordAlates.csv"))

# PROCESSED DATASET FROM distanceFunctions.R
# Full distance to entrance workers dataset
WorkerDistScaledRD1_RD2 <- read.csv(here("analysis", "data", "processed", "WorkerDistScaledRD1_RD2.csv"))

# Workers high density distance to entrance dataset
WorkerDistScaled <- read.csv(here("analysis", "data", "processed", "WorkerDistScaled.csv"))

# Workers low density distance to entrance dataset
WorkerDistScaledRD2 <- read.csv(here("analysis", "data", "processed", "WorkerDistScaledRD2.csv"))

# Full distance to entrance brood dataset
BroodDistScaledRD1_RD2 <- read.csv(here("analysis", "data", "processed", "BroodDistScaledRD1_RD2.csv"))

# Brood high density distance to entrance dataset
BroodDistScaled <- read.csv(here("analysis", "data", "processed", "BroodDistScaled.csv"))

# Brood low density distance to entrance dataset
BroodDistScaledRD2 <- read.csv(here("analysis", "data", "processed", "BroodDistScaledRD2.csv"))

# Full distance to entrance queens dataset
QueenDistScaledRD1_RD2 <- read.csv(here("analysis", "data", "processed", "QueenDistScaledRD1_RD2.csv"))

# Queens high density distance to entrance dataset
QueenDistScaled <- read.csv(here("analysis", "data", "processed", "QueenDistScaled.csv"))

# Queens low density distance to entrance dataset
QueenDistScaledRD2 <- read.csv(here("analysis", "data", "processed", "QueenDistScaledRD2.csv"))

# Alates low density distance to entrance dataset
AlateDistScaledRD2 <- read.csv(here("analysis", "data", "processed", "AlateDistScaledRD2.csv"))

# Full distance to entrance brood dataset
BroodCentDistWorkersRD1_RD2 <- read.csv(here("analysis", "data", "processed", "BroodCentDistWorkersRD1_RD2.csv"))

# Brood high density distance to entrance dataset
BroodCentDistWorkersRD1 <- read.csv(here("analysis", "data", "processed", "BroodCentDistWorkersRD1.csv"))

# Brood low density distance to entrance dataset
BroodCentDistWorkersRD2 <- read.csv(here("analysis", "data", "processed", "BroodCentDistWorkersRD2.csv"))

# Full distance to entrance brood dataset
BroodCentDistQueensRD1_RD2 <- read.csv(here("analysis", "data", "processed", "BroodCentDistQueensRD1_RD2.csv"))

# Brood high density distance to entrance dataset
BroodCentDistQueensRD1 <- read.csv(here("analysis", "data", "processed", "BroodCentDistQueensRD1.csv"))

# Brood low density distance to entrance dataset
BroodCentDistQueensRD2 <- read.csv(here("analysis", "data", "processed", "BroodCentDistQueensRD2.csv"))

# Brood low density distance to entrance dataset
BroodCentDistAlatesRD2 <- read.csv(here("analysis", "data", "processed", "BroodCentDistAlatesRD2.csv"))

# Worker SFZ distance to entrance dataset
WorkerDistScaledRD1_RD2SFZWorking <- read.csv(here("analysis", "data", "processed", "WorkerDistScaledRD1_RD2SFZWorking.csv"))

# Worker SFZ distance to brood center dataset
BroodCentDistWorkersSFZ <- read.csv(here("analysis", "data", "processed", "BroodCentDistWorkersSFZ.csv"))

# NULL BINS REFERENCE (EMPIRICAL)
BinsNullFull <- read.csv(here("analysis", "data", "RefData", "BinsNullFull.csv"))

# REFERENCE COORDINATES FOR CORNERS (EMPIRICAL)
CornerFull <- read.csv(here("analysis", "data", "RefData", "CornerFull.csv"))

# NEST AREA REFERENCES
NestAreaFull <- read.csv(here("analysis", "data", "RefData", "NestAreaFull.csv"))

####################################################################################################################
# COLONY CENSUS DATA TABLE
# The below script produces a census data table for the experimental colonies
# Specifically, the number of workers, brood (avg), queen(s), and alates (avg) are produced
# In addition, the experimental nest area used is given
####################################################################################################################
workerCount <- WorkerDistScaledRD1_RD2 |>
  group_by(Colony, Nest, Day) |>
  summarise(count = n(), .groups = "drop") |>
  group_by(Colony) |>
  summarise(num_workers = mean(count),
            sd_workers = sd(count))

queenCount <- QueenDistScaledRD1_RD2 |>
  group_by(Colony, Nest, Day) |>
  summarise(count = n(), .groups = "drop") |>
  group_by(Colony) |>
  summarise(num_queens = mean(count),
            sd_queens = sd(count))

broodCount <- BroodDistScaledRD1_RD2 |>
  group_by(Colony, Nest, Day) |>
  summarise(count = n(), .groups = "drop") |>
  group_by(Colony) |>
  summarise(num_brood = mean(count),
            sd_brood = sd(count))

alateCount <- AlateDistScaledRD2 |>
  group_by(Colony, Nest, Day) |>
  summarise(count = n(), .groups = "drop") |>
  group_by(Colony) |>
  summarise(num_alates = mean(count),
            sd_alates = sd(count))

colonyCensus <- NestAreaFull |>
  select(-Diameter) |>
  rename(colony = Colony, 
         num_ants = Number.ants, 
         area = Area) |>
  left_join(workerCount, by = c("colony" = "Colony")) |>
  left_join(queenCount, by = c("colony" = "Colony")) |>
  left_join(broodCount, by = c("colony" = "Colony")) |>
  left_join(alateCount, by = c("colony" = "Colony")) |>
  mutate(
    ants = if_else(is.na(num_ants), "-", sprintf("%.0f", num_ants)),
    area = if_else(is.na(area), "-", sprintf("%.2f", area)),
    num_workers = if_else(is.na(num_workers) | is.na(sd_workers), "-", sprintf("%s ± %s", sprintf("%.0f", num_workers), sprintf("%.0f", sd_workers))),
    num_queens = if_else(is.na(num_queens) | is.na(sd_queens), "-", sprintf("%s ± %s", sprintf("%.0f", num_queens), sprintf("%.0f", sd_queens))),
    num_brood = if_else(is.na(num_brood) | is.na(sd_brood), "-", sprintf("%s ± %s", sprintf("%.0f", num_brood), sprintf("%.0f", sd_brood))),
    num_alates = if_else(is.na(num_alates) | is.na(sd_alates), "-", sprintf("%s ± %s", sprintf("%.0f", num_alates), sprintf("%.0f", sd_alates)))) |>
  relocate(ants, .after = colony) |>
  relocate(area, .after = ants) |>
  select(-contains("sd_") & -num_ants) |>
  gt() |>
  cols_label(
    colony  = "Colony ID",
    ants = "# Workers",
    area = html("Nest Area (mm<sup>2</sup>)"),
    num_workers = "Mean ± SD Workers",
    num_queens = "Mean ± SD Queens",
    num_brood = "Mean ± SD Brood",
    num_alates = "Mean ± SD Alates"
  ) |>
  tab_header(
    title = "Colony Census Data",
    subtitle = "Number of each colony member in a colony, and the size of their nests"
  ) |>
  opt_align_table_header(align = "left") |>
  tab_spanner(
    label = "Study variation",
    columns = starts_with("num")
  ) |>
  cols_align(
    align = "left",
    columns = everything()
  ) 
  
gtsave(colonyCensus, here("analysis", "supplementaryMaterials", "supplementaryTables", "TableA1.docx"), )

####################################################################################################################
# COLONY MEMBER SCATTERPLOTS
# The below script produces the colony member density plots
# Each scatter plot colors the points by K-nearest neighbors, which is defined by how many points are in proximity to one another
# Each colony's worker, brood, queen, and alate (low nest density only) are created in separate facets: facet_grid(Nest ~ ColonyMember)
####################################################################################################################

# Adding ColonyMember column for each required dataset
# Workers
WorkerDensityPlot <- WorkerDistScaledRD1_RD2 %>%
  mutate(ColonyMember = "Workers")

#Brood
BroodDensityPlot <- BroodDistScaledRD1_RD2 %>%
  mutate(ColonyMember = "Brood")

#Queens
QueenDensityPlot <- QueenDistScaledRD1_RD2 %>%
  mutate(ColonyMember = "Queens")

#Alates
AlateDensityPlot <- AlateDistScaledRD2 %>%
  mutate(ColonyMember = "Alates")

# Full dataset with min-max scaled points
AllColonyMemberDensity <- 
  full_join(WorkerDensityPlot, BroodDensityPlot) %>%
  full_join(QueenDensityPlot) %>%
  full_join(AlateDensityPlot) %>%
  group_by(Colony, Nest) %>% # Group data by Colony and Nest
  mutate(ScaledX = (ScaledX - min(ScaledX)) / (max(ScaledX) - min(ScaledX)), # Min-max scaled X
         ScaledY = (ScaledY - min(ScaledY)) / (max(ScaledY) - min(ScaledY))) %>% # Min-max scaled Y
  ungroup() # Ungroup

# KNN PLOT
# Reorder the ColonyMember column's factors
AllColonyMemberDensity$ColonyMember = factor(AllColonyMemberDensity$ColonyMember, levels=c('Workers','Brood','Queens','Alates'))

label_function <- function(x) {
  return(x / 400)
}

# Plot
DensityPlotColony <- AllColonyMemberDensity %>%
  ggplot(aes(ScaledX, ScaledY)) +
  geom_pointdensity(alpha = 0.75, method = "default") +
  scale_color_viridis(labels = label_function) +
  coord_fixed() +
  theme_pubclean() +
  xlab(NULL) +
  ylab(NULL) +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "bottom",
        strip.background = element_blank(),
        legend.title = element_text(size = 18, color = "black"),
        legend.text = element_text(size = 18, color = "black"),
        strip.text.x = element_text(size = 18, color = "black"),
        strip.text.y = element_blank(),
        panel.spacing = unit(2, "lines")) +
  labs(color = "Local\nNeighbors") +
  facet_grid(Nest ~ ColonyMember) +
  guides(color = guide_colorsteps(barheight = unit(0.5, "cm"),
                                  barwidth = unit(7.5, "cm"),
                                  even.steps = FALSE,
                                  frame.colour = "black"))

# Use ggplot_build to inspect the calculated values
DensityPlotColony

# Save plot as a PDF
ggsave(file = "analysis/figures/Fig2.jpg", plot = DensityPlotColony, width = 8.5, height = 4, units = "in")

####################################################################################################################
# NEST SECTION DENSITY CALCULATIONS
# The following scripts calculate the proportion of colony members in each nest section (Column Bin: 1-8), calculated in the file Bins_Working.R 
# The script uses a null data set that's just Colony, Nest, and Bin called BinsNullFull, which just shows bin 1-8 for each Colony and nest combination
# The following also uses a reference data set CornerFull, which assignes the presence of corners (Y/N) to nest sections
####################################################################################################################

# First we create the proportions data set, then a null one 
# This approach allows zeros to be present in the proportions data sets, since no workers in the nest section is relevant data

# WORKERS
# High density treatment
Prop_functionWorker <- function(data.table) {
  AntProp <- data.table %>% # Creating the data set of worker proportions in each nest section
    group_by(Colony, Nest, Day) %>% # Group by columns Colony, Nest, Day
    mutate(count = n()) %>% # Count total number of workers in each observation
    group_by(Colony, Nest, Day, Bin) %>% # Group by columns Colony, Nest, Day, Bin
    mutate(BinCount = n(), # Counting the number of each worker in each bin in each observation
           PropWorker = (BinCount / count)) %>% # Calculate the proportion of workers in each bin 
    select(Colony, Day, Nest, Bin, PropWorker, Density) %>% # Select only the desired columns
    distinct() # Remove duplicate rows
  # Creating the data set of null worker proportions in each nest section
  AntPropNull <- AntProp %>%  
    ungroup() %>% # Ungroup the data set
    select(c(Colony, Nest, Day, Density)) %>% # Select the desired columns
    distinct() # Remove duplicate rows
  NestArchNullBins <- full_join(AntPropNull, BinsNullFull, relationship = "many-to-many") %>% # Join the two null data sets
    drop_na()
  # Joining the working data set to the null one, which keeps the zeros in the final data set
  AntPropFullWorkers <<- full_join(NestArchNullBins, AntProp) %>%  
    group_by(Colony, Nest, Day) %>% # Group by columns Colony, Nest, and Day
    mutate(PropWorker = ifelse(is.na(PropWorker), 0, PropWorker),# NAs are produced in the join above, this makes them zeros
           Binsum = sum(PropWorker)) %>% # Create a column that sums the proportions
    filter(Binsum != 0) %>% # Removes any rows with zeros from the Binsum column. This is only a precaution 
    select(Colony, Day, Nest, Bin, PropWorker, Density) %>% # Select only the desired columns
    left_join(CornerFull)  # Joins with a data set that assigned corner presence to each nest section
}

# Run the proportions of workers in nest sections function for the FullDataCoordWorkers data set 
Prop_functionWorker(FullDataCoordWorkers)

# WORKERS
# Low density treatment
Prop_functionWorker <- function(data.table) {
  AntProp <- data.table %>% # Creating the data set of worker proportions in each nest section
    group_by(Colony, Nest, Day) %>% # Group by columns Colony, Nest, Day
    mutate(count = n()) %>% # Count total number of workers in each observation
    group_by(Colony, Nest, Day, Bin) %>% # Group by columns Colony, Nest, Day, Bin
    mutate(BinCount = n(), # Counting the number of each worker in each bin in each observation
           PropWorker = (BinCount / count)) %>% # Calculate the proportion of workers in each bin 
    select(Colony, Day, Nest, Bin, PropWorker, Density) %>% # Select only the desired columns
    distinct() # Remove duplicate rows
  # Creating the data set of null worker proportions in each nest section
  AntPropNull <- AntProp %>%  
    ungroup() %>% # Ungroup the data set
    select(c(Colony, Nest, Day, Density)) %>% # Select the desired columns
    drop_na() %>% # Remove any NAs
    distinct() # Remove duplicate rows
  NestArchNullBins <- full_join(AntPropNull, BinsNullFull, relationship = "many-to-many") # Join the two null data sets
  # Joining the working data set to the null one, which keeps the zeros in the final data set
  AntPropFullWorkersRD2 <<- full_join(NestArchNullBins, AntProp) %>%  
    group_by(Colony, Nest, Day) %>% # Group by columns Colony, Nest, and Day
    mutate(PropWorker = ifelse(is.na(PropWorker), 0, PropWorker),# NAs are produced in the join above, this makes them zeros
           Binsum = sum(PropWorker)) %>% # Create a column that sums the proportions
    filter(Binsum != 0) %>% # Removes any rows with zeros from the Binsum column. This is only a precaution 
    select(Colony, Day, Nest, Bin, PropWorker, Density) %>% # Select only the desired columns
    left_join(CornerFull) # Joins with a data set that assigned corner presence to each nest section
}

# Run the proportions of workers in nest sections function for the FullDataCoordWorkersRD2 data set 
Prop_functionWorker(FullDataCoordWorkersRD2)

#Join worker proportions in nest sections data sets
AntPropFullWorkersRD1_RD2 <- full_join(AntPropFullWorkers, AntPropFullWorkersRD2)

# BROOD
# High density treatment
Prop_functionBrood <- function(data.table) {
  BroodProp <- data.table %>% # Creating the data set of brood proportions in each nest section
    group_by(Colony, Nest, Day) %>% # Group by columns Colony, Nest, Day
    mutate(count = n()) %>% # Count total number of brood in each observation
    group_by(Colony, Nest, Day, Bin) %>% # Group by columns Colony, Nest, Day, Bin
    mutate(BinCount = n(), # Counting the number of each brood in each bin in each observation
           PropBrood = (BinCount / count)) %>% # Calculate the proportion of brood in each bin 
    select(Colony, Day, Nest, Bin, PropBrood, Density) %>% # Select only the desired columns
    distinct() # Remove duplicate rows
  # Creating the data set of null brood proportions in each nest section
  BroodPropNull <- BroodProp %>%  
    ungroup() %>% # Ungroup the data set
    select(c(Colony, Nest, Day, Density)) %>% # Select the desired columns
    drop_na() %>% # Remove any NAs
    distinct() # Remove duplicate rows
  NestArchNullBins <- full_join(BroodPropNull, BinsNullFull, relationship = "many-to-many") # Join the two null data sets
  # Joining the working data set to the null one, which keeps the zeros in the final data set
  AntPropFullBrood <<- full_join(NestArchNullBins, BroodProp) %>%  
    group_by(Colony, Nest, Day) %>% # Group by columns Colony, Nest, and Day
    mutate(PropBrood = ifelse(is.na(PropBrood), 0, PropBrood),# NAs are produced in the join above, this makes them zeros
           Binsum = sum(PropBrood)) %>% # Create a column that sums the proportions
    filter(Binsum != 0) %>% # Removes any rows with zeros from the Binsum column. This is only a precaution 
    select(Colony, Day, Nest, Bin, PropBrood, Density) %>% # Select only the desired columns
    left_join(CornerFull) %>%# Joins with a data set that assigned corner presence to each nest section
    distinct()
}

# Run the proportions of brood in nest sections function for the FullDataCoordBrood data set 
Prop_functionBrood(FullDataCoordBrood)

# BROOD
# Low density treatment
Prop_functionBrood <- function(data.table) {
  BroodProp <- data.table %>% # Creating the data set of brood proportions in each nest section
    group_by(Colony, Nest, Day) %>% # Group by columns Colony, Nest, Day
    mutate(count = n()) %>% # Count total number of brood in each observation
    group_by(Colony, Nest, Day, Bin) %>% # Group by columns Colony, Nest, Day, Bin
    mutate(BinCount = n(), # Counting the number of each brood in each bin in each observation
           PropBrood = (BinCount / count)) %>% # Calculate the proportion of brood in each bin 
    select(Colony, Day, Nest, Bin, PropBrood, Density) %>% # Select only the desired columns
    distinct() # Remove duplicate rows
  # Creating the data set of null brood proportions in each nest section
  BroodPropNull <- BroodProp %>%  
    ungroup() %>% # Ungroup the data set
    select(c(Colony, Nest, Day, Density)) %>% # Select the desired columns
    drop_na() %>% # Remove any NAs
    distinct() # Remove duplicate rows
  NestArchNullBins <- full_join(BroodPropNull, BinsNullFull, relationship = "many-to-many") # Join the two null data sets
  # Joining the working data set to the null one, which keeps the zeros in the final data set
  AntPropFullBroodRD2 <<- full_join(NestArchNullBins, BroodProp) %>%  
    group_by(Colony, Nest, Day) %>% # Group by columns Colony, Nest, and Day
    mutate(PropBrood = ifelse(is.na(PropBrood), 0, PropBrood),# NAs are produced in the join above, this makes them zeros
           Binsum = sum(PropBrood)) %>% # Create a column that sums the proportions
    filter(Binsum != 0) %>% # Removes any rows with zeros from the Binsum column. This is only a precaution 
    select(Colony, Day, Nest, Bin, PropBrood, Density) %>% # Select only the desired columns
    left_join(CornerFull) # Joins with a data set that assigned corner presence to each nest section
}

# Run the proportions of brood in nest sections function for the FullDataCoordBroodRD2 data set 
Prop_functionBrood(FullDataCoordBroodRD2)

# Join brood proportions in nest sections data sets
AntPropFullBroodRD1_RD2 <- full_join(AntPropFullBrood, AntPropFullBroodRD2) 

# QUEENS
# High density treatment
Prop_functionQueen <- function(data.table) {
  QueenProp <- data.table %>% # Creating the data set of queen proportions in each nest section
    group_by(Colony, Nest, Day) %>% # Group by columns Colony, Nest, Day
    mutate(count = n()) %>% # Count total number of queen in each observation
    group_by(Colony, Nest, Day, Bin) %>% # Group by columns Colony, Nest, Day, Bin
    mutate(BinCount = n(), # Counting the number of each queen in each bin in each observation
           PropQueen = (BinCount / count)) %>% # Calculate the proportion of queen in each bin 
    select(Colony, Day, Nest, Bin, PropQueen, Density) %>% # Select only the desired columns
    distinct() # Remove duplicate rows
  # Creating the data set of null queen proportions in each nest section
  QueenPropNull <- QueenProp %>%  
    ungroup() %>% # Ungroup the data set
    select(c(Colony, Nest, Day, Density)) %>% # Select the desired columns
    drop_na() %>% # Remove any NAs
    distinct() # Remove duplicate rows
  NestArchNullBins <- full_join(QueenPropNull, BinsNullFull, relationship = "many-to-many") # Join the two null data sets
  # Joining the working data set to the null one, which keeps the zeros in the final data set
  AntPropFullQueen <<- full_join(NestArchNullBins, QueenProp) %>%  
    group_by(Colony, Nest, Day) %>% # Group by columns Colony, Nest, and Day
    mutate(PropQueen = ifelse(is.na(PropQueen), 0, PropQueen),# NAs are produced in the join above, this makes them zeros
           Binsum = sum(PropQueen)) %>% # Create a column that sums the proportions
    filter(Binsum != 0) %>% # Removes any rows with zeros from the Binsum column. This is only a precaution 
    select(Colony, Day, Nest, Bin, PropQueen, Density) %>% # Select only the desired columns
    left_join(CornerFull) # Joins with a data set that assigned corner presence to each nest section
}

# Run the proportions of queens in nest sections function for the FullDataCoordQueen data set 
Prop_functionQueen(FullDataCoordQueen)

# QUEENS
# Low density treatment
Prop_functionQueen <- function(data.table) {
  QueenProp <- data.table %>% # Creating the data set of queen proportions in each nest section
    group_by(Colony, Nest, Day) %>% # Group by columns Colony, Nest, Day
    mutate(count = n()) %>% # Count total number of queen in each observation
    group_by(Colony, Nest, Day, Bin) %>% # Group by columns Colony, Nest, Day, Bin
    mutate(BinCount = n(), # Counting the number of each queen in each bin in each observation
           PropQueen = (BinCount / count)) %>% # Calculate the proportion of queen in each bin 
    select(Colony, Day, Nest, Bin, PropQueen, Density) %>% # Select only the desired columns
    distinct() # Remove duplicate rows
  # Creating the data set of null queen proportions in each nest section
  QueenPropNull <- QueenProp %>%  
    ungroup() %>% # Ungroup the data set
    select(c(Colony, Nest, Day, Density)) %>% # Select the desired columns
    drop_na() %>% # Remove any NAs
    distinct() # Remove duplicate rows
  NestArchNullBins <- full_join(QueenPropNull, BinsNullFull, relationship = "many-to-many") # Join the two null data sets
  # Joining the working data set to the null one, which keeps the zeros in the final data set
  AntPropFullQueenRD2 <<- full_join(NestArchNullBins, QueenProp) %>%  
    group_by(Colony, Nest, Day) %>% # Group by columns Colony, Nest, and Day
    mutate(PropQueen = ifelse(is.na(PropQueen), 0, PropQueen),# NAs are produced in the join above, this makes them zeros
           Binsum = sum(PropQueen)) %>% # Create a column that sums the proportions
    filter(Binsum != 0) %>% # Removes any rows with zeros from the Binsum column. This is only a precaution 
    select(Colony, Day, Nest, Bin, PropQueen, Density) %>% #Select only the desired columns
    left_join(CornerFull) # Joins with a data set that assigned corner presence to each nest section
}

# Run the proportions of queens in nest sections function for the FullDataCoordQueenRD2 data set 
Prop_functionQueen(FullDataCoordQueenRD2)

# Join queen proportions in nest sections data sets
AntPropFullQueenRD1_RD2 <- full_join(AntPropFullQueen, AntPropFullQueenRD2)

# ALATES
# Only the high density treatment
Prop_functionAlate <- function(data.table) {
  AlateProp <- data.table %>% # Creating the data set of alate proportions in each nest section
    group_by(Colony, Nest, Day) %>% # Group by columns Colony, Nest, Day
    mutate(count = n()) %>% # Count total number of alates in each observation
    group_by(Colony, Nest, Day, Bin) %>% # Group by columns Colony, Nest, Day, Bin
    mutate(BinCount = n(), # Counting the number of each alate in each bin in each observation
           PropAlate = (BinCount / count)) %>% # Calculate the proportion of alates in each bin 
    select(Colony, Day, Nest, Bin, PropAlate) %>% # Select only the desired columns
    distinct() # Remove duplicate rows
  # Creating the data set of null alate proportions in each nest section
  AlatePropNull <- AlateProp %>%  
    ungroup() %>% # Ungroup the data set
    select(c(Colony, Nest, Day)) %>% # Select the desired columns
    drop_na() %>% # Remove any NAs
    distinct() # Remove duplicate rows
  NestArchNullBins <- full_join(AlatePropNull, BinsNullFull, relationship = "many-to-many") # Join the two null data sets
  # Joining the working data set to the null one, which keeps the zeros in the final data set
  AntPropFullAlateRD2 <<- full_join(NestArchNullBins, AlateProp) %>%  
    group_by(Colony, Nest, Day) %>% # Group by columns Colony, Nest, and Day
    mutate(PropAlate = ifelse(is.na(PropAlate), 0, PropAlate),# NAs are produced in the join above, this makes them zeros
           Binsum = sum(PropAlate)) %>% # Create a column that sums the proportions
    filter(Binsum != 0) %>% # Removes any rows with zeros from the Binsum column. This is only a precaution 
    select(Colony, Day, Nest, Bin, PropAlate) %>% # Select only the desired columns
    left_join(CornerFull) # Joins with a data set that assigned corner presence to each nest section
}

# Run the Prop_functionAlate function for the FullDataCoordAlates data set 
Prop_functionAlate(FullDataCoordAlates)

####################################################################################################################
# PLOTS AND ANALYSES: Colony member densities through the nest
# The scripts below are to analyze and visualize: 
# Worker, brood, queen, and alate densities through the nest
####################################################################################################################

# WORKER DENSITIES IN NEST SECTIONS
# BOXPLOTS 
# TUBE NEST
# High nest density
WorkerProp1 <- ggplot(data = AntPropFullWorkers %>% arrange(Nest), 
                     aes(x = as.factor(Bin), y = PropWorker, fill = Nest)) + 
  geom_boxplot(color = "grey25", 
               alpha = 0.65,
               coef = 200) +
  xlab(NULL) + 
  ylab(NULL) +
  ggtitle("High density") +
  theme_pubclean() +  
  theme(axis.text = element_text(size = 18, color = "black"),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(size = 18, color = "black", hjust = 0.85, vjust = 0.5),
        legend.position = "none") +
  guides(fill = guide_legend(title = "Nest", color = "black")) +
  scale_fill_manual(breaks = c("Tube", "Circle"), 
                    name = "Nest",
                    values = c("red", "blue")) +
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.5, 1.0))

WorkerProp2 <- ggplot(data = AntPropFullWorkersRD2 %>% arrange(Nest), 
                      aes(x = as.factor(Bin), y = PropWorker, fill = Nest)) + 
  geom_boxplot(color = "grey25", 
               alpha = 0.65,
               coef = 200) +
  xlab(NULL) + 
  ylab(NULL) +
  ggtitle("Low density") +
  theme_pubclean() +  
  theme(axis.text.y = element_text(size = 18, color = "white"),
        axis.text.x = element_text(size = 18, color = "black"),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(size = 18, color = "black", hjust = 0.85, vjust = 0.5),
        legend.position = "none") +
  guides(fill = guide_legend(title = "Nest", color = "black")) +
  scale_fill_manual(breaks = c("Tube", "Circle"), 
                    name = "Nest",
                    values = c("red", "blue")) +
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.5, 1.0))


# Compiling the worker density in nest sections box plots 
WorkerPropPlot <- ggarrange(WorkerProp1, WorkerProp2,
                            labels = c("(a)", "(b)"),
                            font.label = list(size = 18, face = "plain"),
                            label.x = 0.88,
                            label.y = 1,
                            ncol = 2, nrow = 1,
                            common.legend = FALSE)

# Annotating the compiled tube nest plot to include a title and common y-axis

WorkerPropPlotFull <- annotate_figure(WorkerPropPlot,
                                      top = text_grob("Workers", color = "black",
                                                      size = 18, 
                                                      x = 0.065, y = -0.75),
                                      bottom = NULL,
                                      left = NULL,
                                      right = NULL
)

# LINEAR MIXED EFFECTS MODEL: Worker density through the nest
# RESPONSE VARIABLE
# PropWorker - Proportion of total workers found in each nest section
# FIXED EFFECTS 
# Bin - Nest section, transformed to raw polynomial term because of a priori assumptions 
# Nest - Nest shape (Tube / Circle)
# Density - Nest density (High / Low)
# Day - Experimental observation (From days 1-16, always days 3-14 in High density treatment)
# Corner - Presence of a corner in the nest section (Y / N)
# RANDOM EFFECT
# (1 | Colony) - Colony identification 
m_workerProp <- lmer(PropWorker ~ poly(Bin, degree = 2, raw = TRUE) * Nest + Density + Day + Corner + (1 | Colony), data = AntPropFullWorkersRD1_RD2)

# BROOD DENSITIES IN NEST SECTIONS
# BOXPLOTS
# High density treatment
BroodProp1 <- ggplot(data = AntPropFullBrood %>% arrange(Nest), 
                     aes(x = as.factor(Bin), y = PropBrood, fill = Nest)) + 
  geom_boxplot(color = "grey25", 
               alpha = 0.65,
               coef = 200) +
  xlab(NULL) + 
  ylab(NULL) +
  ggtitle("High density") +
  theme_pubclean() +  
  theme(axis.text = element_text(size = 18, color = "black"),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(size = 18, color = "white", hjust = 0.87, vjust = 0.5),
        legend.position = "none") +
  scale_fill_manual(breaks = c("Tube", "Circle"), 
                    name = "Nest",
                    values = c("red", "blue")) +
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.5, 1.0))

# Low density treatment
BroodProp2 <- ggplot(data = AntPropFullBroodRD2 %>% arrange(Nest), 
                     aes(x = as.factor(Bin), y = PropBrood, fill = Nest)) + 
  geom_boxplot(color = "grey25", 
               alpha = 0.65,
               coef = 200) + 
  xlab(NULL) + 
  ylab(NULL) +
  ggtitle("Low density") +
  theme_pubclean() +  
  theme(axis.text.y = element_text(size = 18, color = "white"),
        axis.text.x = element_text(size = 18, color = "black"),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(size = 18, color = "white", hjust = 0.875, vjust = 0.5),
        legend.position = "none") +
  scale_fill_manual(breaks = c("Tube", "Circle"), 
                    name = "Nest",
                    values = c("red", "blue")) +
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.5, 1.0))



# Compiling the two above brood densities in nest sections boxplots 
BroodPropPlot <- ggarrange(BroodProp1, BroodProp2,
                           labels = c("(c)", "(d)"),
                           font.label = list(size = 18, face = "plain"),
                           label.x = 0.88,
                           label.y = 1,
                           ncol = 2, nrow = 1)

# Annotating the compiled boxplots to include shared axes labels
BroodPropPlotFull <- annotate_figure(BroodPropPlot,
                                   top = text_grob("Brood", color = "black", 
                                                   size = 18, 
                                                   x = 0.05, y = -0.75),
                                   bottom = NULL,
                                   left = NULL,
                                   right = NULL
)

# LINEAR MIXED EFFECTS MODEL: Brood density through the nest
# RESPONSE VARIABLE
# PropBrood - Proportion of total brood found in each nest section
# FIXED EFFECTS 
# Bin - Nest section, transformed to raw polynomial term because of a priori assumptions 
# Nest - Nest shape (Tube / Circle)
# Density - Nest density (High / Low)
# Day - Experimental observation (From days 1-16, always days 3-14 in High density treatment)
# Corner - Presence of a corner in the nest section (Y / N)
# RANDOM EFFECT
# (1 | Colony) - Colony identification 
m_broodProp <- lmer(PropBrood ~ poly(Bin, degree = 2, raw = TRUE) * Nest + Density + Day + Corner + (1 | Colony), data = AntPropFullBroodRD1_RD2)

# QUEEN DENSITIES IN NEST SECTIONS
# BOXPLOTS

# High density treatment
QueenProp1 <- ggplot(data = AntPropFullQueen %>% arrange(Nest), 
                   aes(x = as.factor(Bin), y = PropQueen, fill = Nest)) + 
  stat_boxplot_custom(qs = c(0, 0.25, 0.5, 0.75, 1.00),
                      aes(fill = Nest),
                      color = "grey25", 
                      alpha = 0.65) +
  xlab(NULL) + 
  ylab(NULL) +
  ggtitle("High density") +
  theme_pubclean() +
  theme(axis.text = element_text(size = 18, color = "black"),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(size = 18, color = "white", hjust = 0.87, vjust = 0.5),
        legend.position = "none") +
  guides(fill = guide_legend(title = "Nest", color = "black")) +
  scale_fill_manual(breaks = c("Tube", "Circle"), 
                    name = "Nest",
                    values = c("red", "blue")) +
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.5, 1.0))


#Low density treatment
QueenProp2 <- ggplot(data = AntPropFullQueenRD2 %>% arrange(Nest), 
                   aes(x = as.factor(Bin), y = PropQueen, fill = Nest)) + 
  stat_boxplot_custom(qs = c(0, 0.25, 0.5, 0.75, 1.00),
                      aes(fill = Nest),
                      color = "grey25", 
                      alpha = 0.65) +
  xlab(NULL) + 
  ylab(NULL) +
  ggtitle("High density") +
  theme_pubclean() +
  theme(axis.ticks = element_blank(),
        axis.text.x = element_text(size = 18, color = "black"),
        axis.title = element_blank(),
        axis.text.y = element_text(size = 18, color = "white"),
        plot.title = element_text(size = 18, color = "white", hjust = 0.87, vjust = 0.5),
        legend.position = "none") +
  scale_fill_manual(breaks = c("Tube", "Circle"), 
                    name = "Nest",
                    values = c("red", "blue")) +
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.5, 1.0))

# Compiling the two above queen densities in nest sections boxplots 
QueenPropPlot <- ggarrange(QueenProp1, QueenProp2,
                           labels = c("(e)", "(f)"),
                           font.label = list(size = 18, face = "plain"),
                           label.x = 0.88,
                           label.y = 1,
                           ncol = 2, nrow = 1)

# Annotating the compiled boxplots to include common axes labels
QueenPropPlotFull <- annotate_figure(QueenPropPlot,
                top = text_grob("Queens", color = "black", 
                                size = 18, 
                                x = 0.06, y = -0.75),
                bottom = NULL,
                left = NULL,
                right = NULL
)

# LINEAR MIXED EFFECTS MODEL: Queen density through the nest
# RESPONSE VARIABLE
# PropQueen - Proportion of total queens found in each nest section
# FIXED EFFECTS 
# Bin - Nest section, transformed to raw polynomial term because of a priori assumptions 
# Nest - Nest shape (Tube / Circle)
# Density - Nest density (High / Low)
# Day - Experimental observation (From days 1-16, always days 3-14 in High density treatment)
# Corner - Presence of a corner in the nest section (Y / N)
# RANDOM EFFECT
# (1 | Colony) - Colony identification 
m_queenProp <- lmer(PropQueen ~ poly(Bin, degree = 2, raw = TRUE) * Nest + Density + Day + Corner + (1 | Colony), data = AntPropFullQueenRD1_RD2)

# ALATE DENSITIES IN NEST SECTIONS
# BOXPLOTS 
# Note that alates were only in the low nest density
AlatePropFig <- ggplot(data = AntPropFullAlateRD2 %>% arrange(Nest), 
       aes(x = as.factor(Bin), y = PropAlate)) + 
  stat_boxplot_custom(qs = c(0, 0.25, 0.5, 0.75, 1.00),
                      aes(fill = Nest), 
                      color = "grey25", 
                      alpha = 0.65) +
  xlab(NULL) + 
  ylab(NULL) +
  ggtitle("Low density") +
  theme_pubclean() +
  theme(axis.text = element_text(size = 18, color = "black"),
        axis.ticks = element_blank(),
        axis.title = element_text(size = 18, color = "black"),
        plot.title = element_text(size = 18, color = "white", hjust = 0.87, vjust = 0.5),
        legend.position = "none") +
  scale_fill_manual(breaks = c("Tube", "Circle"), 
                    name = "Nest",
                    values = c("red", "blue")) +
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.5, 1.0))

# Empty plot with just a legend
legend_props <- get_legend(
  ggplot(data = AntPropFullAlateRD2 %>% arrange(Nest), 
         aes(x = as.factor(Bin), y = PropAlate, fill = Nest)) +
    geom_boxplot(alpha = 0.65) +
    lims(y = c(0,0)) +
    theme_void()+
    theme(legend.position = c(0.5,0.5),
          legend.key = element_blank(),
          legend.direction = "horizontal",
          legend.text = element_text(size = 18, color = "black"),
          legend.title = element_text(size = 18, color = "black"),
          legend.key.size = unit(1, 'cm'))+
    guides(fill = guide_legend(title = "Nest", color = "black")) +
    scale_fill_manual(breaks = c("Tube", "Circle"), 
                      name = "Nest",
                      values = c("red", "blue")))

# Arranging the above alate densities in nest sections boxplot 
AlatePropPlot <- ggarrange(AlatePropFig,
                           labels = c("(g)"),
                           font.label = list(size = 18, face = "plain"),
                           label.x = 0.88,
                           label.y = 1,
                           ncol = 1, nrow = 1) + 
  theme(plot.margin = margin(0.4, 0.1, 0.1, 0.1, "cm"))

# Annotating the alate proportions boxplot to include a title
AlateProp <- annotate_figure(AlatePropPlot,
                             top = text_grob("Alates", color = "black", 
                                             size = 18, 
                                             x = 0.115, y = -1.25),
                                   bottom = NULL,
                                   left = NULL,
                                   right = NULL
)

AlateLegend <- ggarrange(legend_props, AlateProp)

# Compiling the brood and queen densities in nest sections plots
AllProp <- cowplot::plot_grid(WorkerPropPlotFull, 
          BroodPropPlotFull, 
          QueenPropPlotFull,
          AlateLegend,
          ncol = 1,
          nrow = 4)

AllPropFull <- annotate_figure(AllProp,
                top = NULL,
                bottom = text_grob("Nest section", color = "black", 
                                   size = 18),
                left = text_grob("Proportion of colony member", color = "black", 
                                 size = 18, rot = 90),
                right = NULL
)

# Save plot as a PDF
ggsave(file = "analysis/figures/Fig3.jpg", plot = AllPropFull, width = 8.5, height = 8, units = "in", dpi = 400)

# LINEAR MIXED EFFECTS MODEL: Observed alate density through the nest
# RESPONSE VARIABLE
# PropAlate - Proportion of total alates found in each nest section
# FIXED EFFECTS 
# Bin - Nest section, transformed to raw polynomial term because of a priori assumptions 
# Nest - Nest shape (Tube / Circle)
# Day - Experimental observation (From days 1-16, always days 3-14 in High density treatment)
# Corner - Presence of a corner in the nest section (Y / N)
# RANDOM EFFECT
# (1 | Colony) - Colony identification  
m_alateProp <- lmer(PropAlate ~  poly(Bin, degree = 2, raw = TRUE) * Nest + Day + Corner + (1 | Colony), data = AntPropFullAlateRD2)

# All colony member proportions within nest sections model outputs in one concise table
tab_model(m_workerProp, m_broodProp, m_queenProp, m_alateProp,
          show.df = TRUE, 
          show.stat = TRUE, 
          show.se = TRUE, 
          show.ci = FALSE, 
          show.icc = FALSE,
          auto.label = FALSE,
          dv.labels = c("Workers", "Brood", "Queens", "Alates"),
          pred.labels = c("Intercept", "NestSect", "NestSect2", "Nest",
                          "Density", "Day", 
                          "Corner", "NestSect:Nest", 
                          "NestSect2:Nest"),
          col.order = c("est", "se", "df.error", "stat", "p"),
          string.pred = "Coeffcient",
          string.est = "Est.",
          string.se = "SE",
          string.stat = "T",
          string.p = "P",
          digits = 3,
          title = "Linear Mixed Effects: colony member proportions within nest sections",
          file = "analysis/tables/Table1.html")

webshot("analysis/tables/Table1.html", 
        "analysis/tables/Table1.pdf")

Convert2Docx::Converter(pdf_file = "analysis/tables/Table1.pdf",
                        docx_filename = "analysis/tables/Table1.docx")

####################################################################################################################
# PLOTS AND ANALYSES: Colony member distances from the nest entrance
# The scripts below are to analyze and visualize: 
# Worker, brood, queen, and alate distances from the nest entrance (including comparing workers and Netlogo simulations)
####################################################################################################################

# WORKER SCALED DISTANCES FROM THE NEST ENTRANCE
# HISTOGRAMS + BOXPLOTS COMBO PLOTS
# High density treatment
# Histogram
WorkerDist1 <- ggplot(WorkerDistScaled %>% arrange(Nest), 
                      aes(ScaledDist, 
                          fill = Nest)) + 
  geom_vline(aes(xintercept = 0), color = "gray85", linewidth = 2) +
  geom_vline(aes(xintercept = 0.34), color = "gray85", linewidth = 2) +
  geom_histogram(position = "identity",
                 alpha = 0.7,
                 binwidth = 0.0416666) +
  ggtitle("High density") +
  labs(color = "Nest") +
  xlab(NULL) + 
  ylab("Worker count") +
  theme_pubclean() +  
  theme(axis.text.x = element_text(size = 16, color = "black", hjust = 1),
        axis.text.y = element_text(size = 16, color = "black"),
        axis.ticks.y = element_blank(),
        axis.title.x = element_text(size = 18,color = "black", hjust = 1),
        axis.title.y = element_text(size = 18,color = "black"),
        plot.title = element_text(size = 18, color = "white", hjust = 0.8, vjust = 0.5),
        legend.key = element_blank(),
        legend.justification = c(1.0, -0.7),
        legend.position = c(1.0, 1.0),
        legend.direction = "horizontal",
        legend.text = element_text(size = 18, color = "black"),
        legend.title = element_text(size = 18, color = "black"),
        legend.key.size = unit(1, 'cm')) +
  scale_x_continuous(breaks = c(0, 0.5, 1.0),
                     limits = c(0, 1)) +
  scale_y_continuous(breaks = c(0, 2000),
                     limits = c(0, 2000)) +
  scale_fill_manual(breaks = c("Tube", "Circle"), 
                    name = "Nest",
                    values = c("red", "blue")) +
  coord_flip()

# Boxplot
WorkerBox1 <- WorkerDistScaled %>% arrange(Nest) %>% left_join(NestAreaFull) |>
  ggplot(aes(x = fct_reorder(as.factor(Colony), Number.ants), y = ScaledDist,
             fill = Nest)) + 
  geom_hline(aes(yintercept = 0), color = "gray85", linewidth = 2) +
  geom_hline(aes(yintercept = 0.34), color = "gray85", linewidth = 2) +
  geom_boxplot(color = "grey25", 
               alpha = 0.65,
               coef = 200) +
  ggtitle("High density") +
  labs(fill = "Nest") +
  xlab("Colony") + 
  ylab(NULL) +
  theme_pubclean() +  
  theme(axis.text.x = element_text(size = 18, color = "black"),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_text(size = 16,color = "black"),
        axis.title.y = element_text(size = 16,color = "black"),
        plot.title = element_text(size = 18, color = "black", hjust = 0.8, vjust = 0.5),
        legend.key = element_blank(),
        legend.justification = c(1.0, -0.7),
        legend.position = c(1.0, 1.0),
        legend.direction = "horizontal",
        legend.text = element_text(size = 18, color = "black"),
        legend.title = element_text(size = 18, color = "black"),
        legend.key.size = unit(1, 'cm')) +
  scale_y_continuous(breaks = c(0, 0.5, 1.0),
                     limits = c(0, 1)) +
  scale_fill_manual(breaks = c("Tube", "Circle"), 
                    name = "Nest",
                    values = c("red", "blue"))

# Low density treatment
# Histogram
WorkerDist2 <- ggplot(WorkerDistScaledRD2 %>% arrange(Nest), 
                      aes(ScaledDist, 
                          fill = Nest)) + 
  geom_vline(aes(xintercept = 0), color = "gray85", linewidth = 2) +
  geom_vline(aes(xintercept = 0.34), color = "gray85", linewidth = 2) +
  geom_histogram(position = "identity",
                 alpha = 0.7,
                 binwidth = 0.0416666) +
  ggtitle("Low density") +
  labs(color = "Nest") +
  xlab(NULL) + 
  ylab("Worker count") +
  theme_pubclean() +  
  theme(axis.text.x = element_text(size = 16, color = "black", hjust = 1),
        axis.text.y = element_text(size = 16, color = "black"),
        axis.ticks.y = element_blank(),
        axis.title.x = element_text(size = 18,color = "black", hjust = 1),
        axis.title.y = element_text(size = 18,color = "white"),
        plot.title = element_text(size = 18, color = "white", hjust = 0.8, vjust = 0.5),
        legend.key = element_blank(),
        legend.justification = c(1.0, -0.7),
        legend.position = c(1.0, 1.0),
        legend.direction = "horizontal",
        legend.text = element_text(size = 18, color = "black"),
        legend.title = element_text(size = 18, color = "black"),
        legend.key.size = unit(1, 'cm')) +
  scale_x_continuous(breaks = c(0, 0.5, 1.0),
                     limits = c(0, 1)) +
  scale_y_continuous(breaks = c(0, 2000),
                     limits = c(0, 2000)) +
  scale_fill_manual(breaks = c("Tube", "Circle"), 
                    name = "Nest",
                    values = c("red", "blue")) +
  coord_flip()

# Boxplot
WorkerBox2 <- WorkerDistScaledRD2 %>% arrange(Nest) %>% left_join(NestAreaFull) |>
  ggplot(aes(x = fct_reorder(as.factor(Colony), Number.ants), y = ScaledDist,
             fill = Nest)) +   
  geom_hline(aes(yintercept = 0), color = "gray85", linewidth = 2) +
  geom_hline(aes(yintercept = 0.34), color = "gray85", linewidth = 2) +
  geom_boxplot(color = "grey25", 
               alpha = 0.65,
               coef = 200) +
  ggtitle("Low density") +
  labs(fill = "Nest") +
  xlab("Colony") + 
  ylab(NULL) +
  theme_pubclean() +  
  theme(axis.text.x = element_text(size = 16, color = "black"),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_text(size = 18,color = "black"),
        axis.title.y = element_text(size = 18,color = "black"),
        plot.title = element_text(size = 18, color = "black", hjust = 0.8, vjust = 0.5),
        legend.key = element_blank(),
        legend.justification = c(1.0, -0.7),
        legend.position = c(1.0, 1.0),
        legend.direction = "horizontal",
        legend.text = element_text(size = 18, color = "black"),
        legend.title = element_text(size = 18, color = "black"),
        legend.key.size = unit(1, 'cm')) +
  scale_y_continuous(breaks = c(0, 0.5, 1.0),
                     limits = c(0, 1)) +
  scale_fill_manual(breaks = c("Tube", "Circle"), 
                    name = "Nest",
                    values = c("red", "blue"))

# LINEAR MIXED EFFECTS MODEL: Worker distances from the nest entrance
# RESPONSE VARIABLE
# ScaledDist - Worker scaled distances from the nest entrance (0 - 1, where 1 is the longest, shortest distance from the nest entrance)
# FIXED EFFECTS 
# Nest - Nest shape (Tube / Circle)
# Density - Nest density (High / Low)
# Day - Experimental observation (From days 1-16, always days 3-14 in High density treatment)
# RANDOM EFFECT
# (1 | Colony) - Colony identification 
m_workerDist <- lmer(ScaledDist ~ Nest * Density + Day + (1 | Colony), data = WorkerDistScaledRD1_RD2)

# BROOD SCALED DISTANCES FROM THE NEST ENTRANCE
# HISTOGRAMS + BOXPLOTS COMBO PLOTS
# High density treatment
# Histogram
BroodDist1 <- ggplot(BroodDistScaled %>% arrange(Nest), 
                     aes(ScaledDist, 
                         fill = Nest)) +
  geom_vline(aes(xintercept = 0), color = "gray85", linewidth = 2) +
  geom_vline(aes(xintercept = 0.34), color = "gray85", linewidth = 2) +
  geom_histogram(position = "identity",
                 alpha = 0.7,
                 binwidth = 0.0416666) +
  ggtitle("High density") +
  labs(color = "Nest") +
  xlab(NULL) + 
  ylab("Brood count") +
  theme_pubclean() +  
  theme(axis.text.x = element_text(size = 16, color = "black", hjust = 1),
        axis.text.y = element_text(size = 16, color = "black"),
        axis.ticks.y = element_blank(),
        axis.title.x = element_text(size = 18,color = "black", hjust = 1),
        axis.title.y = element_text(size = 18,color = "black"),
        plot.title = element_text(size = 18, color = "white", hjust = 0.8, vjust = 0.5),
        legend.key = element_blank(),
        legend.justification = c(1.0, -0.7),
        legend.position = c(1.0, 1.0),
        legend.direction = "horizontal",
        legend.text = element_text(size = 18, color = "black"),
        legend.title = element_text(size = 18, color = "black"),
        legend.key.size = unit(1, 'cm')) +
  scale_x_continuous(breaks = c(0, 0.5, 1.0),
                     limits = c(0, 1)) +
  scale_y_continuous(breaks = c(0, 8000),
                     limits = c(0, 8000)) +
  scale_fill_manual(breaks = c("Tube", "Circle"), 
                    name = "Nest",
                    values = c("red", "blue")) +
  coord_flip()

# Boxplot
BroodBox1 <- BroodDistScaled %>% arrange(Nest) %>% left_join(NestAreaFull) |>
  ggplot(aes(x = fct_reorder(as.factor(Colony), Number.ants), y = ScaledDist,
             fill = Nest)) + 
  geom_hline(aes(yintercept = 0), color = "gray85", linewidth = 2) +
  geom_hline(aes(yintercept = 0.34), color = "gray85", linewidth = 2) +
  geom_boxplot(color = "grey25", 
               alpha = 0.65,
               coef = 200) +
  ggtitle("High density") +
  labs(fill = "Nest") +
  xlab("Colony") + 
  ylab(NULL) +
  theme_pubclean() +  
  theme(axis.text.x = element_text(size = 18, color = "black"),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_text(size = 16,color = "black"),
        axis.title.y = element_text(size = 16,color = "black"),
        plot.title = element_text(size = 18, color = "white", hjust = 0.8, vjust = 0.5),
        legend.key = element_blank(),
        legend.justification = c(1.0, -0.7),
        legend.position = c(1.0, 1.0),
        legend.direction = "horizontal",
        legend.text = element_text(size = 18, color = "black"),
        legend.title = element_text(size = 18, color = "black"),
        legend.key.size = unit(1, 'cm')) +
  scale_y_continuous(breaks = c(0, 0.5, 1.0),
                     limits = c(0, 1)) +
  scale_fill_manual(breaks = c("Tube", "Circle"), 
                    name = "Nest",
                    values = c("red", "blue"))

# Low density treatment
# Histogram
BroodDist2 <- ggplot(BroodDistScaledRD2 %>% arrange(Nest), 
                     aes(ScaledDist, 
                         fill = Nest)) + 
  geom_vline(aes(xintercept = 0), color = "gray85", linewidth = 2) +
  geom_vline(aes(xintercept = 0.34), color = "gray85", linewidth = 2) +
  geom_histogram(position = "identity",
                 alpha = 0.7,
                 binwidth = 0.0416666) +
  ggtitle("Low density") +
  labs(color = "Nest") +
  xlab(NULL) + 
  ylab("Brood count") +
  theme_pubclean() +  
  theme(axis.text.x = element_text(size = 16, color = "black", hjust = 1),
        axis.text.y = element_text(size = 16, color = "black"),
        axis.ticks.y = element_blank(),
        axis.title.x = element_text(size = 18,color = "black", hjust = 1),
        axis.title.y = element_text(size = 18, color = "white"),
        plot.title = element_text(size = 18, color = "white", hjust = 0.8, vjust = 0.5),
        legend.key = element_blank(),
        legend.justification = c(1.0, -0.7),
        legend.position = c(1.0, 1.0),
        legend.direction = "horizontal",
        legend.text = element_text(size = 18, color = "black"),
        legend.title = element_text(size = 18, color = "black"),
        legend.key.size = unit(1, 'cm')) +
  scale_x_continuous(breaks = c(0, 0.5, 1.0),
                     limits = c(0, 1)) +
  scale_y_continuous(breaks = c(0, 8000),
                     limits = c(0, 8000)) +
  scale_fill_manual(breaks = c("Tube", "Circle"), 
                    name = "Nest",
                    values = c("red", "blue")) +
  coord_flip()

# Boxplot
BroodBox2 <- BroodDistScaledRD2 %>% arrange(Nest) %>% left_join(NestAreaFull) |>
  ggplot(aes(x = fct_reorder(as.factor(Colony), Number.ants), y = ScaledDist,
             fill = Nest)) + 
  geom_hline(aes(yintercept = 0), color = "gray85", linewidth = 2) +
  geom_hline(aes(yintercept = 0.34), color = "gray85", linewidth = 2) +
  geom_boxplot(color = "grey25", 
               alpha = 0.65,
               coef = 200) +
  ggtitle("Low density") +
  labs(fill = "Nest") +
  xlab("Colony") + 
  ylab(NULL) +
  theme_pubclean() +  
  theme(axis.text.x = element_text(size = 16, color = "black"),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_text(size = 18,color = "black"),
        axis.title.y = element_text(size = 18, color = "white"),
        plot.title = element_text(size = 18, color = "white", hjust = 0.8, vjust = 0.5),
        legend.key = element_blank(),
        legend.justification = c(1.0, -0.7),
        legend.position = c(1.0, 1.0),
        legend.direction = "horizontal",
        legend.text = element_text(size = 18, color = "black"),
        legend.title = element_text(size = 18, color = "black"),
        legend.key.size = unit(1, 'cm')) +
  scale_y_continuous(breaks = c(0, 0.5, 1.0),
                     limits = c(0, 1)) +
  scale_fill_manual(breaks = c("Tube", "Circle"), 
                    name = "Nest",
                    values = c("red", "blue"))

# Compiling the brood and queen densities in nest sections plots
fullDistPlot <- ggarrange(WorkerDist1, WorkerBox1, WorkerDist2, WorkerBox2,
                          BroodDist1, BroodBox1, BroodDist2, BroodBox2,
                          widths = c(1/2, 1, 1/2, 1, 1/2, 1, 1/2, 1), 
                          common.legend = T,
                          ncol = 4, nrow = 2,
                          labels = c("", "(a)", "", "(b)",
                                     "", "(c)", "", "(d)"),
                          label.x = 0.85,
                          font.label = list(size = 18, face = "plain"))

AllDistFull <- annotate_figure(fullDistPlot, 
                top = NULL,
                bottom = NULL,
                left = text_grob("Scaled distance to nest entrance", color = "black", 
                                 size = 18, rot = 90),
                right = NULL
                )

# Save plot as a PDF
ggsave(file = "analysis/figures/Fig4.jpg", plot = AllDistFull, width = 11, height = 8, units = "in", dpi = 400)

# LINEAR MIXED EFFECTS MODEL: Brood scaled distances from the nest entrance
# RESPONSE VARIABLE
# ScaledDist - Brood scaled distances from the nest entrance (0 - 1, where 1 is the longest, shortest distance from the nest entrance)
# FIXED EFFECTS 
# Nest - Nest shape (Tube / Circle)
# Density - Nest density (High / Low)
# Day - Experimental observation (From days 1-16, always days 3-14 in High density treatment)
# RANDOM EFFECT
# (1 | Colony) - Colony identification 
m_broodDist <- lmer(ScaledDist ~ Nest * Density + Day + (1 | Colony), data = BroodDistScaledRD1_RD2)

# QUEEN SCALED DISTANCES FROM THE NEST ENTRANCE
# HISTOGRAMS
# High density treatment
QueenDist1 <- ggplot(QueenDistScaled %>% arrange(Nest),
                     aes(ScaledDist, 
                         fill = Nest)) + 
  geom_histogram(position = "identity", 
                 alpha = 0.7, 
                 binwidth = 0.0416666) +
  xlab(NULL) + 
  ylab("Queen count") +
  ggtitle("High density") +
  theme_pubclean() +  
  theme(axis.ticks = element_blank(),
        axis.text = element_text(size = 18, color = "black"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 18, color = "black"),
        plot.title = element_text(size = 18, color = "black", hjust = 0.8, vjust = 0.5),
        legend.key = element_blank(),
        legend.justification = c(1.0, -0.7),
        legend.position = c(1.0, 1.0),
        legend.direction = "horizontal",
        legend.text = element_text(size = 18, color = "black"),
        legend.title = element_text(size = 18, color = "black"),
        legend.key.size = unit(1, 'cm')) +
  scale_fill_manual(breaks = c("Tube", "Circle"), 
                    name = "Nest",
                    values = c("red", "blue")) +
  xlim(0, 1) +
  scale_y_continuous(breaks = c(0, 50, 100),
                     limits = c(0, 100))

# Low density treatment
QueenDist2 <- ggplot(QueenDistScaledRD2 %>% arrange(Nest),
                     aes(ScaledDist, 
                         fill= Nest)) + 
  geom_histogram(position = "identity", 
                 alpha = 0.7, 
                 binwidth = 0.0416666) +
  xlab(NULL) + 
  ylab(NULL) +
  ggtitle("Low density") +
  theme_pubclean() +  
  theme(axis.ticks = element_blank(),
        axis.text = element_text(size = 18, color = "black"),
        axis.title = element_blank(),
        plot.title = element_text(size = 18, color = "black", hjust = 0.8, vjust = 0.5),
        legend.key = element_blank(),
        legend.justification = c(1.0, -0.7),
        legend.position = c(1.0, 1.0),
        legend.direction = "horizontal",
        legend.text = element_text(size = 18, color = "black"),
        legend.title = element_text(size = 18, color = "black"),
        legend.key.size = unit(1, 'cm')) +
  scale_fill_manual(breaks = c("Tube", "Circle"), 
                    name = "Nest",
                    values = c("red", "blue")) +
  xlim(0, 1) +
  scale_y_continuous(breaks = c(0, 50, 100),
                     limits = c(0, 100))

# Compiling the queens distance to the nest entrance histograms
QueenDistPlot <- ggarrange(QueenDist1, QueenDist2,
                           labels = c("(a)", "(b)"),
                           label.x = 0.875,
                           label.y = 0.99,
                           font.label = list(size = 18, face = "plain"),
                           ncol = 2, nrow = 1,
                           common.legend = TRUE)


# LINEAR MIXED EFFECTS MODEL: Queen scaled distances from the nest entrance
# RESPONSE VARIABLE
# ScaledDist - Queen scaled distances from the nest entrance (0 - 1, where 1 is the longest, shortest distance from the nest entrance)
# FIXED EFFECTS 
# Nest - Nest shape (Tube / Circle)
# Density - Nest density (High / Low)
# Day - Experimental observation (From days 1-16, always days 3-14 in High density treatment)
# RANDOM EFFECT
# (1 | Colony) - Colony identification 
m_queenDist <- lmer(ScaledDist ~ Nest * Density + Day + (1 | Colony), data = QueenDistScaledRD1_RD2)

# ALATE SCALED DISTANCES TO THE NEST ENTRANCE
# Removing individuals with unknown alate sex
# In these plots and analyses, we care about the individuals with identifiable sex
# 10 individuals are removing the unknown sex individuals, representing < 1% of the data set  
AlateDistScaledRD2Plot <- AlateDistScaledRD2 %>%
  filter(Sex != "?")

# HISTOGRAM
AlateDist1 <- ggplot(AlateDistScaledRD2Plot %>% arrange(Nest), 
                     aes(ScaledDist, 
                         fill = Nest)) + 
  geom_histogram(position = "identity", 
                 alpha = 0.7, 
                 binwidth = 0.0416666) +
  ggtitle("(c)") +
  xlab(NULL) +
  ylab("Alate count") +
  theme_pubclean() +
  theme(plot.title = element_text(size = 18, color = "black", hjust = 1, vjust = 1),
        axis.ticks = element_blank(),
        axis.text = element_text(size = 18, color = "black"),
        axis.title = element_text(size = 18, color = "black"),
        legend.position = "none") +
  guides(fill = guide_legend(title = "Nest", color = "black")) +
  scale_fill_manual(breaks = c("Tube", "Circle"), 
                    name = "Nest",
                    values = c("red", "blue")) +
  scale_y_continuous(breaks = c(0, 50, 100),
                     limits = c(0, 100))
                     

# BOXPLOT SHOWING THE RELATIONSHIP BETWEEN ALATE SEX AND SCALED DISTANCE TO THE NEST ENTRANCE 
AlateDist2 <- ggplot(AlateDistScaledRD2Plot %>% arrange(Nest),
                     aes(x = Sex, y = ScaledDist, 
                         fill = Nest)) +
  stat_boxplot_custom(qs = c(0, 0.25, 0.5, 0.75, 1.00),
                      aes(fill = Nest), 
                      color = "grey25", 
                      alpha = 0.65,
                      width = 0.5) + 
  theme_pubclean() +
  ggtitle("(d)") +
  xlab("Alate sex") +
  ylab(NULL) +
  theme(plot.title = element_text(size = 18, color = "black", hjust = 1, vjust = 1),
        axis.ticks = element_blank(),
        axis.text = element_text(size = 18, color = "black"),
        axis.title = element_text(size = 18, color = "black"),
        legend.position = "none") +
  scale_fill_manual(breaks = c("Tube", "Circle"), 
                    name = "Nest",
                    values = c("red", "blue")) +
  coord_flip()

# Compiling the alate distance to the entrance plots 
AlateDistPlot <- ggarrange(AlateDist1, AlateDist2,
                           ncol = 2, nrow = 1)

# Annotating the compiled plots to include a common y axis and title
AlateFullDist <- annotate_figure(AlateDistPlot,
                                      top = NULL,
                                      bottom = text_grob("Scaled distance to nest entrance", 
                                                         color = "black",
                                                         size = 18),
                                      left = NULL,
                                      right = NULL
)

# Compiling the brood and queen densities in nest sections plots
QueenAlateDist <- ggarrange(QueenDistPlot,
                            AlateFullDist,
                            ncol = 1, nrow = 2)

# Save plot as a PDF
ggsave(file = "analysis/supplementaryMaterials/supplementaryFigures/Fig_A4.jpg", plot = QueenAlateDist, width = 8.5, height = 6, units = "in", dpi = 400)

# LINEAR MIXED EFFECTS MODEL: Alate scaled distances from the nest entrance
# RESPONSE VARIABLE
# ScaledDist - Alate scaled distances from the nest entrance (0 - 1, where 1 is the longest, shortest distance from the nest entrance)
# FIXED EFFECTS 
# Nest - Nest shape (Tube / Circle)
# Sex - Alate sex (M / F)
# Day - Experimental observation (From days 1-16, always days 3-14 in High density treatment)
# Ratio - Ratio of male alates over total alates in the observation (0 - 1)
# RANDOM EFFECT
# (1 | Colony) - Colony identification 
m_alateDist <- lmer(ScaledDist ~ Nest + Sex + Ratio + Day + (1 | Colony), data = AlateDistScaledRD2Plot)

# All colony member distance to the nest entrance model outputs in one concise table
tab_model(m_workerDist, m_broodDist, m_queenDist, m_alateDist,
          show.df = TRUE, 
          show.stat = TRUE, 
          show.se = TRUE, 
          show.ci = FALSE, 
          show.icc = FALSE,
          auto.label = TRUE,
          dv.labels = c("Workers", "Brood", "Queens", "Alates"),
          pred.labels = c("Intercept", "Nest", "Density", "Day", "Nest:Density",
                          "Sex", "SexRatio"),
          col.order = c("est", "se", "df.error", "stat", "p"),
          string.pred = "Coeffcient",
          string.est = "Est.",
          string.se = "SE",
          string.stat = "T",
          string.p = "P",
          digits = 3,
          title = "Linear Mixed Effects: colony member scaled distances to the nest entrance",
          file = "analysis/supplementaryMaterials/supplementaryTables/TableA2.html")

webshot("analysis/supplementaryMaterials/supplementaryTables/TableA2.html", 
        "analysis/supplementaryMaterials/supplementaryTables/TableA2.pdf")

Convert2Docx::Converter(pdf_file = "analysis/supplementaryMaterials/supplementaryTables/TableA2.pdf",
                        docx_filename = "analysis/supplementaryMaterials/supplementaryTables/TableA2.docx")

####################################################################################################################
# PLOTS AND ANALYSES: Mobile colony member distances from the brood center
# The scripts below are to analyze and visualize: 
# Worker, queen, and alate distances from the brood center
####################################################################################################################

# WORKER SCALED DISTANCE TO THE BROOD CENTER
# HISTOGRAMS
# High density treatment
WorkerBroodDist1 <- ggplot(BroodCentDistWorkersRD1 %>% arrange(Nest),
                           aes(ToBrood, 
                               fill = Nest)) + 
  geom_histogram(position = "identity", 
                 alpha = 0.7, 
                 binwidth = 0.0416666) +
  ggtitle("High density") +
  labs(color = "Nest") +
  xlab(NULL) + 
  ylab(NULL) +
  theme_pubclean() +  
  theme(axis.ticks = element_blank(),
        axis.text = element_text(size = 18, color = "black"),
        axis.title = element_blank(),
        plot.title = element_text(size = 18, color = "black", hjust = 0.83, vjust = 0.51),
        legend.key = element_blank(),
        legend.justification = c(1, -0.7),
        legend.position = c(1.0, 0.7),
        legend.direction = "horizontal",
        legend.text = element_text(size = 18, color = "black"),
        legend.title = element_text(size = 18, color = "black"),
        legend.key.size = unit(1, 'cm')) +
  guides(fill = guide_legend(title = "Nest")) +
  scale_fill_manual(breaks = c("Tube", "Circle"), 
                    name = "Nest",
                    values = c("red", "blue")) + 
  xlim(0, 0.85) + 
  ylim(0, 3000)

# Low density treatment
WorkerBroodDist2 <- ggplot(BroodCentDistWorkersRD2 %>% arrange(Nest), 
                           aes(ToBrood, 
                               fill = Nest)) + 
  geom_histogram(position = "identity", 
                 alpha = 0.7, 
                 binwidth = 0.0416666) +
  labs(color = "Nest") +
  xlab(NULL) + 
  ylab(NULL) +
  ggtitle("Low density") +
  theme_pubclean() +  
  theme(axis.text.x = element_text(size = 18, color = "black"),
        axis.text.y = element_text(size = 18, color = "white"),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(size = 18, color = "black", hjust = 0.83, vjust = 0.51),
        legend.key = element_blank(),
        legend.justification = c(1, -0.7),
        legend.position = c(1.0, 0.7),
        legend.direction = "horizontal",
        legend.text = element_text(size = 18, color = "black"),
        legend.title = element_text(size = 18, color = "black"),
        legend.key.size = unit(1, 'cm')) +
  guides(fill = guide_legend(title = "Nest")) +
  scale_fill_manual(breaks = c("Tube", "Circle"), 
                    name = "Nest",
                    values = c("red", "blue")) + 
  xlim(0, 0.85) + 
  ylim(0, 3000)

#Compiling worker scaled distances from brood center plots
WorkerBroodDistPlot <- ggarrange(WorkerBroodDist1, WorkerBroodDist2,
                                 labels = c("(a)", "(b)"),
                                 label.x = 0.875,
                                 font.label = list(size = 18, face = "plain"),
                                 ncol = 2, nrow = 1,
                                 common.legend = TRUE)

#Annotating the compiled plot to include a common y axis and title
WorkerBroodFullDist <- annotate_figure(WorkerBroodDistPlot,
                                       top = NULL,
                                       bottom = text_grob("Scaled distance to brood center", color = "black",
                                                          size = 18),
                                       left = text_grob("Worker count", color = "black",
                                                  size = 18, rot = 90),
                                       right = NULL
)

# Save plot as a PDF
ggsave(file = "analysis/figures/Fig5.jpg", plot = WorkerBroodFullDist, width = 8.5, height = 3.5, units = "in", dpi = 400)

# LINEAR MIXED EFFECTS MODEL: Worker scaled distances to the brood center
# RESPONSE VARIABLE
# ToBrood - Worker scaled distances from the brood center (0 - 1, where 1 is the longest, shortest distance from the nest entrance)
# FIXED EFFECTS 
# Nest - Nest shape (Tube / Circle)
# Density - Nest density (High / Low)
# Day - Experimental observation (From days 1-16, always days 3-14 in High density treatment)
# RANDOM EFFECT
# (1 | Colony) - Colony identification 
m_workerBroodDist <- lmer(ToBrood ~ Nest * Density + Day + (1 | Colony), data = BroodCentDistWorkersRD1_RD2)

# QUEEN SCALED DISTANCE TO THE BROOD CENTER
# HISTOGRAMS
# High density treatment
QueenBroodDist1 <- ggplot(BroodCentDistQueensRD1 %>% arrange(Nest),
                        aes(ToBrood, 
                            fill = Nest)) + 
  geom_histogram(position = "identity", 
                 alpha = 0.7, 
                 binwidth = 0.0416666) +
  labs(color = "Nest") +
  ggtitle("High density") +
  ylab("Queen count") +
  theme_pubclean() +  
  theme(axis.ticks = element_blank(),
        axis.text = element_text(size = 18, color = "black"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 18, color = "black"),
        plot.title = element_text(size = 18, color = "black", hjust = 0.75, vjust = 0.5),
        legend.key = element_blank(),
        legend.justification = c(1.0, -0.7),
        legend.position = c(1.0, 1.0),
        legend.direction = "horizontal",
        legend.text = element_text(size = 18, color = "black"),
        legend.title = element_text(size = 18, color = "black"),
        legend.key.size = unit(1, 'cm')) +
  guides(fill = guide_legend(title = "Nest")) +
  scale_fill_manual(breaks = c("Tube", "Circle"), 
                    name = "Nest",
                    values = c("red", "blue")) +
  xlim(0, 0.85) +
  scale_y_continuous(breaks = c(0, 100, 200),
                     limits = c(0, 200))

# Low density treatment
QueenBroodDist2 <- ggplot(BroodCentDistQueensRD2 %>% arrange(Nest),
                          aes(ToBrood, fill = Nest)) + 
  geom_histogram(position = "identity", 
                 alpha = 0.7, 
                 binwidth = 0.0416666) +
  ggtitle("Low density") +
  theme_pubclean() +  
  theme(axis.ticks = element_blank(),
        axis.text = element_text(size = 18, color = "black"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 18, color = "white"),
        plot.title = element_text(size = 18, color = "black", hjust = 0.75, vjust = 0.5),
        legend.key = element_blank(),
        legend.justification = c(1.0, -0.7),
        legend.position = c(1.0, 1.0),
        legend.direction = "horizontal",
        legend.text = element_text(size = 18, color = "black"),
        legend.title = element_text(size = 18, color = "black"),
        legend.key.size = unit(1, 'cm')) +
  scale_fill_manual(breaks = c("Tube", "Circle"), 
                    name = "Nest",
                    values = c("red", "blue")) +
  xlim(0, 0.85) +
  scale_y_continuous(breaks = c(0, 100, 200),
                     limits = c(0, 200))

# Compiling queens scaled distances to the brood center plots
QueenBroodDistPlot <- ggarrange(QueenBroodDist1, QueenBroodDist2,
                                labels = c("(a)", "(b)"),
                                label.x = 0.85,
                                font.label = list(size = 18, face = "plain"),
                                ncol = 2, nrow = 1,
                                common.legend = TRUE)

# LINEAR MIXED EFFECTS MODEL: Queen scaled distances to the brood center
# RESPONSE VARIABLE
# ToBrood - Queen scaled distances from the brood center (0 - 1, where 1 is the longest, shortest distance from the nest entrance)
# FIXED EFFECTS 
# Nest - Nest shape (Tube / Circle)
# Density - Nest density (High / Low)
# Day - Experimental observation (From days 1-16, always days 3-14 in High density treatment)
# RANDOM EFFECT
# (1 | Colony) - Colony identification 
m_queenBroodDist <- lmer(ToBrood ~ Nest * Density + Day + (1 | Colony), data = BroodCentDistQueensRD1_RD2)

# ALATE SCALED DISTANCE TO THE BROOD CENTER
# Removing individuals with unknown alate sex
# In these plots and analyses, we care about the individuals with identifiable sex
# 10 individuals are removing the unknown sex individuals, representing < 1% of the data set  
BroodCentDistAlatesRD2Plot <- BroodCentDistAlatesRD2 %>% filter(Sex != "?")

# HISTOGRAM
AlateBroodPlot <- ggplot(BroodCentDistAlatesRD2Plot %>% arrange(Nest),
                         aes(ToBrood, 
                             fill = Nest)) + 
  geom_histogram(position = "identity", 
                 alpha = 0.7, 
                 binwidth = 0.0416666) +
  ggtitle("(c)") +
  xlab(NULL) +
  ylab("Alate count") +
  theme_pubclean() +
  theme(plot.title = element_text(size = 18, color = "black", hjust = 1, vjust = 1),
        axis.ticks = element_blank(),
        axis.text = element_text(size = 18, color = "black"),
        axis.title = element_text(size = 18),
        legend.position = "none") +
  guides(fill = guide_legend(title = "Nest")) +
  scale_fill_manual(breaks = c("Tube", "Circle"), 
                    name = "Nest",
                    values = c("red", "blue")) +
  scale_y_continuous(breaks = c(0, 50, 100),
                     limits = c(0, 100))

# BOXPLOT SHOWING THE RELATIONSHIP BETWEEN ALATE SEX AND SCALED DISTANCE TO THE BROOD CENTER
AlateBroodPlot2 <- ggplot(BroodCentDistAlatesRD2Plot %>% arrange(Nest),
                     aes(x = Sex, y = ToBrood, 
                         fill = Nest)) +
  stat_boxplot_custom(qs = c(0, 0.25, 0.5, 0.75, 1.00),
                      aes(fill = Nest), 
                      color = "grey25", 
                      alpha = 0.65,
                      width = 0.5) + 
  theme_pubclean() +
  ggtitle("(d)") +
  xlab("Alate sex") +
  ylab(NULL) +
  theme(plot.title = element_text(size = 18, color = "black", hjust = 1, vjust = 1),
        axis.ticks = element_blank(),
        axis.text = element_text(size = 18, color = "black"),
        axis.title = element_text(size = 18, color = "black"),
        legend.key = element_blank(),
        legend.justification = c(0.5, 1),
        legend.text = element_text(size = 18, color = "black"),
        legend.title = element_text(size = 18, color = "black"),
        legend.position = "none") +
  scale_fill_manual(breaks = c("Tube", "Circle"), 
                    name = "Nest",
                    values = c("red", "blue")) +
  coord_flip() 
  

# Compiling the alate distance to the the brood center plots 
AlateBroodDistPlot <- ggarrange(AlateBroodPlot, AlateBroodPlot2,
                           ncol = 2, nrow = 1)

# Annotating the compiled plots to include a common y axis and title
AlateBroodFullDist <- annotate_figure(AlateBroodDistPlot,
                                      top = NULL,
                                      bottom = text_grob("Scaled distance to brood center", color = "black",
                                                         size = 18),
                                      left = NULL,
                                      right = NULL
)

# Compiling the brood and queen densities in nest sections plots
QueenAlateBroodDist <- ggarrange(QueenBroodDistPlot,
                                 AlateBroodFullDist, 
                                 ncol = 1, nrow = 2)

# Save plot as a PDF
ggsave(file = "analysis/supplementaryMaterials/supplementaryFigures/Fig_A5.jpg", plot = QueenAlateBroodDist, width = 8.5, height = 6, units = "in", dpi = 400)

# LINEAR MIXED EFFECTS MODEL: Alate scaled distances from the nest entrance
# RESPONSE VARIABLE
# ToBrood - Alate scaled distances from the brood center (0 - 1, where 1 is the longest, shortest distance from the nest entrance)
# FIXED EFFECTS 
# Nest - Nest shape (Tube / Circle)
# Sex - Alate sex (M / F)
# Day - Experimental observation (From days 1-16, always days 3-14 in High density treatment)
# Ratio - Ratio of male alates over total alates in the observation (0 - 1)
# RANDOM EFFECT
# (1 | Colony) - Colony identification 
m_alateBroodDist <- lmer(ToBrood ~ Nest + Sex + Day + Ratio + (1 | Colony), data = BroodCentDistAlatesRD2Plot)

# All colony member distance to the brood center model outputs in one concise table
tab_model(m_workerBroodDist, m_queenBroodDist, m_alateBroodDist,
          show.df = TRUE, 
          show.stat = TRUE, 
          show.se = TRUE, 
          show.ci = FALSE, 
          show.icc = FALSE,
          auto.label = TRUE,
          dv.labels = c("Workers", "Queens", "Alates"),
          pred.labels = c("Intercept", "Nest", "Density", "Day",
                          "Nest:Density",
                          "Sex", "SexRatio"),
          col.order = c("est", "se", "df.error", "stat", "p"),
          string.pred = "Coeffcient",
          string.est = "Est.",
          string.se = "SE",
          string.stat = "T",
          string.p = "P",
          digits = 3,
          title = "Linear Mixed Effects: colony member scaled distances to the brood center",
          file = "analysis/supplementaryMaterials/supplementaryTables/TableA3.html")

webshot("analysis/supplementaryMaterials/supplementaryTables/TableA3.html", 
        "analysis/supplementaryMaterials/supplementaryTables/TableA3.pdf")

Convert2Docx::Converter(pdf_file = "analysis/supplementaryMaterials/supplementaryTables/TableA3.pdf",
                        docx_filename = "analysis/supplementaryMaterials/supplementaryTables/TableA3.docx")

####################################################################################################################
# ANALYSES: Scaled spatial fidelity and occurrence zones
# The scripts below are to analyze: 
# Scaled worker spatial fidelity and occurrence zone sizes, and consider how they relate to nest shape
####################################################################################################################

# LINEAR MIXED EFFECTS MODEL: Worker spatial fidelity zone size (scaled) and nest shape
# RESPONSE VARIABLE
# SFZ - Worker spatial fidelity zone size (0 - 1, where 1 is the entire area of the nest), zones have at least 3 observations and at least 15% of total observations
# FIXED EFFECTS 
# Nest - Nest shape (Tube / Circle)
# Density - Nest density (High / Low)
# Number.ants - Colony size
# MeanScaledDist - each worker's average distance to the nest entrance (0 - 1, where 1 is the longest, shortest distance from the nest entrance)
# RANDOM EFFECTS
# (1 | Colony) - Colony identification 
# (1 | AntID) - Worker color identification 
m_SFZScaled <- lmer(SFZ ~ Nest * Number.ants + Nest * MeanScaledDist + Density * Number.ants + (1 | Colony), data = WorkerDistScaledRD1_RD2SFZWorking)


# LINEAR MIXED EFFECTS MODEL: Worker occurrence zone size (scaled) and nest shape
# RESPONSE VARIABLE
# Occur - Worker occurrence zone size (0 - 1, where 1 is the entire area of the nest), zones have at least 3 observations
# FIXED EFFECTS 
# Nest - Nest shape (Tube / Circle)
# Density - Nest density (High / Low)
# Number.ants - Colony size
# MeanScaledDist - each worker's average distance to the nest entrance (0 - 1, where 1 is the longest, shortest distance from the nest entrance)
# RANDOM EFFECTS
# (1 | Colony) - Colony identification 
# (1 | AntID) - Worker color identification 
m_OccurScaled <- lmer(Occur ~ Nest * Number.ants + Nest * MeanScaledDist + Density * Number.ants + (1 | Colony), data = WorkerDistScaledRD1_RD2SFZWorking)

####################################################################################################################
# ANALYSES: True spatial fidelity and occurrence zones (cm^2)
# The scripts below are to analyze and visualize: 
# True worker spatial fidelity and occurrence zone sizes (cm^2), and consider how they relate to nest shape
####################################################################################################################

# LINEAR MIXED EFFECTS MODEL: Worker spatial fidelity zone size (cm^2) and nest shape
# RESPONSE VARIABLE
# SFZ_Area - True worker fidelity zone size (cm^2), zones have at least 7 observations and be at least 15% of the total observations
# FIXED EFFECTS 
# Nest - Nest shape (Tube / Circle)
# Density - Nest density (High / Low)
# Number.ants - Colony size
# MeanScaledDist - each worker's average distance to the nest entrance (0 - 1, where 1 is the longest, shortest distance from the nest entrance)
# RANDOM EFFECTS
# (1 | Colony) - Colony identification 
# (1 | AntID) - Worker color identification 
m_SFZArea <- lmer(SFZ_Area ~ Nest * Number.ants + Nest * MeanScaledDist + Density * Number.ants + (1 | Colony), data = WorkerDistScaledRD1_RD2SFZWorking)

# LINEAR MIXED EFFECTS MODEL: Worker occurrence zone size (cm^2) and nest shape
# RESPONSE VARIABLE
# Occur_Area - True worker occurrence zone size (cm^2), zones have at least 7 observations
# FIXED EFFECTS 
# Nest - Nest shape (Tube / Circle)
# Density - Nest density (High / Low)
# Number.ants - Colony size
# MeanScaledDist - each worker's average distance to the nest entrance (0 - 1, where 1 is the longest, shortest distance from the nest entrance)
# RANDOM EFFECTS
# (1 | Colony) - Colony identification 
# (1 | AntID) - Worker color identification 
m_OccurArea <- lmer(Occur_Area ~ Nest * Number.ants + Nest * MeanScaledDist + Density * Number.ants + (1 | Colony), data = WorkerDistScaledRD1_RD2SFZWorking)

# All site fidelity v. nest shape model outputs in one concise table
tab_model(m_SFZScaled, m_SFZArea, m_OccurScaled, m_OccurArea,
          show.df = TRUE, 
          show.stat = TRUE, 
          show.se = TRUE, 
          show.ci = FALSE, 
          show.icc = FALSE,
          auto.label = TRUE,
          dv.labels = c("Scaled Fidelity Zone", "Fidelity Zone (cm2)", "Scaled Occurrence Zone", "Occurrence Zone (cm2)"),
          pred.labels = c("Intercept", "Nest", "ColonySize", "MeanScaledDist", "Density","Nest:ColonySize", 
                          "Nest:MeanScaledDist", "ColonySize:Density"),
          col.order = c("est", "se", "df.error", "stat", "p"),
          string.pred = "Coeffcient",
          string.est = "Est.",
          string.se = "SE",
          string.stat = "T",
          string.p = "P",
          digits = 3,
          title = "Linear Mixed Effects: individual worker site fidelity in each nest shape",
          file = "analysis/tables/Table2.html")

webshot("analysis/tables/Table2.html", 
        "analysis/tables/Table2.pdf")

Convert2Docx::Converter(pdf_file = "analysis/tables/Table2.pdf",
                        docx_filename = "analysis/tables/Table2.docx")

####################################################################################################################
# PLOTS AND ANALYSES: Spatial fidelity and occurrence zone sizes & distances from the nest entrance
# The scripts below are to analyze and visualize: 
# Worker spatial fidelity and occurrence zone sizes, and consider how they relate to both nest shape and mean distance from the nest entrance
####################################################################################################################

# Full dataset for below
SFZ_All_Data <- left_join(WorkerDistScaledRD1_RD2SFZWorking, BroodCentDistWorkersSFZ)

# FIDELITY ZONE SIZE AND DISTANCE TO THE NEST ENTRANCE
# SCATTER PLOTS + BOXPLOT COMBO PLOTS
# High density treatment
scaleFUNDist <- function(x) sprintf("%.1f", x)
scaleFUN <- function(x) sprintf("%.0f", x)
SFZDist1 <- SFZ_All_Data %>% 
  filter(Density == "High") %>% arrange(Nest) %>%
  ggplot(aes(x = MeanScaledDist, y = SFZ, fill = Nest)) +
  xlab(NULL) +
  ylab("Scaled fidelity zone size") +
  ggtitle("High density") +
  geom_jitter(aes(x = MeanScaledDist, y = SFZ, color = MeanToBrood, shape = Nest), size = 6, alpha = 0.5) +
  geom_ysideboxplot(aes(x = Nest), orientation = "x", coef = 200, alpha = 0.65) +
  theme_pubclean() +  
  theme(axis.text.x = element_text(size = 18, color = "black"),
        axis.text.y = element_text(size = 18, color = "black"),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 18, color = "black"),
        plot.title = element_text(size = 18, color = "black", hjust = 0.85, vjust = 0.65),
        legend.key = element_blank(),
        legend.direction = "horizontal",
        legend.text = element_text(size = 18, color = "black"),
        legend.title = element_text(size = 18, color = "black"),
        legend.key.size = unit(1, 'cm')) +
  labs(shape = "Nest", color = "Mean dist to brood") +
  guides(alpha = "none",
         color = guide_colourbar(barwidth = 14, barheight = 0.5, 
                                 frame.colour = "black", ticks.colour = "black", ticks.linewidth = 0.75,
                                 order = 2)) +
  scale_fill_manual(breaks = c("Circle", "Tube"), 
                    name = "Nest",
                    values = c("blue", "red"),
                    labels = c("Circle", "Tube"),
                    guide = "none") +
  scale_color_viridis() +
  guides(shape = guide_legend(override.aes = list(alpha = 0.75))) +
  scale_ysidex_discrete(labels = c("C", "T")) +
  scale_x_continuous(labels = scaleFUNDist, limits = c(0, 1),
                     breaks = c(0, 0.4, 0.8)) + 
  scale_y_continuous(labels = scaleFUN, limits = c(1, 4))

# Low density treatment
SFZDist2 <- SFZ_All_Data %>% 
  filter(Density == "Low") %>% arrange(Nest) %>%
  ggplot(aes(x = MeanScaledDist, y = SFZ, fill = Nest)) +
  xlab(NULL) +
  ylab(NULL) +
  ggtitle("Low density") +
  geom_jitter(aes(x = MeanScaledDist, y = SFZ, color = MeanToBrood, shape = Nest), size = 6, alpha = 0.5) +
  geom_ysideboxplot(aes(x = Nest), orientation = "x", coef = 200, alpha = 0.65) +
  theme_pubclean() +  
  theme(axis.text.x = element_text(size = 18, color = "black"),
        axis.text.y = element_text(size = 18, color = "black"),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(size = 18, color = "black", hjust = 0.85, vjust = 0.65),
        legend.key = element_blank(),
        legend.direction = "horizontal",
        legend.text = element_text(size = 18, color = "black"),
        legend.title = element_text(size = 18, color = "black"),
        legend.key.size = unit(1, 'cm')) +
  labs(shape = "Nest", color = "Mean dist to brood") +
  guides(alpha = "none",
         color = guide_colourbar(barwidth = 14, barheight = 0.5, 
                                 frame.colour = "black", ticks.colour = "black", ticks.linewidth = 0.75,
                                 order = 2)) +
  scale_fill_manual(breaks = c("Circle", "Tube"), 
                    name = "Nest",
                    values = c("blue", "red"),
                    labels = c("Circle", "Tube"),
                    guide = "none") +
  scale_color_viridis() +
  scale_ysidex_discrete(labels = c("C", "T")) +
  scale_x_continuous(labels = scaleFUNDist, limits = c(0, 1),
                     breaks = c(0, 0.4, 0.8)) +  
  scale_y_continuous(labels = scaleFUN, limits = c(1, 4))

# OCCURRENCE ZONE SIZE AND DISTANCE TO THE ENTRANCE
# SCATTER PLOTS + BOXPLOT COMBO PLOTS
# High density treatment
OccurDist1 <- SFZ_All_Data %>% 
  filter(Density == "High") %>% arrange(Nest) %>%
  ggplot(aes(x = MeanScaledDist, y = Occur, fill = Nest)) +
  xlab(NULL) +
  ylab("Scaled occurrence zone size") +
  ggtitle("High density") +
  geom_jitter(aes(x = MeanScaledDist, y = Occur, color = MeanToBrood, shape = Nest), size = 6, alpha = 0.5) +
  geom_ysideboxplot(aes(x = Nest), orientation = "x", coef = 200, alpha = 0.65) +
  theme_pubclean() +  
  theme(axis.text.x = element_text(size = 18, color = "black"),
        axis.text.y = element_text(size = 18, color = "black"),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 18, color = "black"),
        plot.title = element_text(size = 18, color = "white", hjust = 0.825, vjust = 0.5),
        legend.key = element_blank(),
        legend.direction = "horizontal",
        legend.text = element_text(size = 18, color = "black"),
        legend.title = element_text(size = 18, color = "black"),
        legend.key.size = unit(1, 'cm')) +
  labs(shape = "Nest", color = "Mean dist to brood") +
  guides(alpha = "none",
         color = guide_colourbar(barwidth = 14, barheight = 0.5, 
                                 frame.colour = "black", ticks.colour = "black", ticks.linewidth = 0.75,
                                 order = 2)) +
  scale_fill_manual(breaks = c("Circle", "Tube"), 
                    name = "Nest",
                    values = c("blue", "red"),
                    labels = c("Circle", "Tube"),
                    guide = "none") +
  scale_color_viridis() +
  scale_ysidex_discrete(labels = c("C", "T")) +
  scale_x_continuous(labels = scaleFUNDist, limits = c(0, 1),
                     breaks = c(0, 0.4, 0.8)) +  
  scale_y_continuous(labels = scaleFUN, limits = c(1, 10))

# Low density treatment
OccurDist2 <- SFZ_All_Data %>% 
  filter(Density == "Low") %>% arrange(Nest) %>%
  ggplot(aes(x = MeanScaledDist, y = Occur, fill = Nest)) +
  xlab(NULL) +
  ylab(NULL) +
  ggtitle("Low density") +
  geom_jitter(aes(x = MeanScaledDist, y = Occur, color = MeanToBrood, shape = Nest), size = 6, alpha = 0.5) +
  geom_ysideboxplot(aes(x = Nest), orientation = "x", coef = 200, alpha = 0.65) +
  theme_pubclean() +  
  theme(axis.text.x = element_text(size = 18, color = "black"),
        axis.text.y = element_text(size = 18, color = "black"),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(size = 18, color = "white", hjust = 0.825, vjust = 0.5),
        legend.key = element_blank(),
        legend.direction = "horizontal",
        legend.text = element_text(size = 18, color = "black"),
        legend.title = element_text(size = 18, color = "black"),
        legend.key.size = unit(1, 'cm')) +
  labs(shape = "Nest", color = "Mean dist to brood") +
  guides(alpha = "none",
         color = guide_colourbar(barwidth = 14, barheight = 0.5, 
                                 frame.colour = "black", ticks.colour = "black", ticks.linewidth = 0.75,
                                 order = 2)) +
  scale_fill_manual(breaks = c("Circle", "Tube"), 
                    name = "Nest",
                    values = c("blue", "red"),
                    labels = c("Circle", "Tube"),
                    guide = "none") +
  scale_color_viridis() +
  scale_ysidex_discrete(labels = NULL) +
  scale_x_continuous(labels = scaleFUNDist, limits = c(0, 0.6)) +  
  scale_y_continuous(labels = scaleFUN, limits = c(1, 10))

SFZOccurDistPlot <- ggarrange(SFZDist1, SFZDist2,
                              OccurDist1, OccurDist2,
                              labels = c("(a)", "(b)", "(c)", "(d)"),
                              label.x = 0.875,
                              font.label = list(size = 18, face = "plain"),
                              ncol = 2, nrow = 2,
                              common.legend = TRUE)

# Annotate the compiled plots to include a common x-axis
SFZOccurFullBroodDistPlot <- annotate_figure(SFZOccurDistPlot,
                                             top = NULL,
                                             bottom = text_grob("Average scaled distance to nest entrance", color = "black",
                                                                size = 18, x = 0.525),
                                             left = NULL,
                                             right = NULL
)

# Save plot as a PDF
ggsave(file = "analysis/figures/Fig6.jpg", plot = SFZOccurFullBroodDistPlot, width = 8.5, height = 7, units = "in", dpi = 400)

####################################################################################################################
# PLOTS AND ANALYSES: Spatial fidelity and occurrence zone sizes (cm^2) & colony size
# The scripts below are to analyze and visualize: 
# Worker spatial fidelity and occurrence zone sizes(cm^2), and consider how they relate colony size
####################################################################################################################
scaleFUNDist <- function(x) sprintf("%.1f", x)
# Spatial fidelity zone size and number of observations for an individual
SFZCol1 <- ggplot(data = WorkerDistScaledRD1_RD2SFZWorking %>% filter(Density == "High") %>% arrange(Nest), 
                  aes(x = Number.ants, y = SFZ_Area, 
                      color = Nest,
                      linetype = Nest,
                      shape = Nest)) +
  xlab(NULL) +
  ylab(expression(paste('Fidelity zone size ('*cm^2*')'))) +
  ggtitle("High density") +
  geom_jitter(size = 6, alpha = 0.5, aes(color = Nest, shape = Nest), width = 5, height = 0.05) +
  geom_line(stat = "smooth", method = lm, se = FALSE, linewidth = 2, color = "black") +
  theme_pubclean() +  
  theme(axis.text.x = element_text(size = 20, color = "black"),
        axis.text.y = element_text(size = 20, color = "black"),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 20, color = "black"),
        plot.title = element_text(size = 20, color = "black", hjust = 0.8, vjust = 0.5),
        legend.key = element_blank(),
        legend.justification = c(1, -0.7),
        legend.position = c(1, 0.71),
        legend.direction = "horizontal",
        legend.text = element_text(size = 20, color = "black"),
        legend.title = element_text(size = 20, color = "black"),
        legend.key.size = unit(1, 'cm')) +
  labs(color = "Nest", linetype = "Nest", shape = "Nest") +
  scale_color_manual(breaks = c("Circle", "Tube"), 
                     name = "Nest",
                     values = c("blue", "red"),
                     labels = c("Circle", "Tube")) +
  guides(shape = guide_legend(override.aes = list(alpha = 0.75))) +
  scale_y_continuous(labels = scaleFUN, limits = c(0, 4), breaks = c(0, 2, 4))  

# Low density treatment
SFZCol2 <- ggplot(data = WorkerDistScaledRD1_RD2SFZWorking %>% filter(Density == "Low") %>% arrange(Nest),
                  aes(x = Number.ants, y = SFZ_Area, 
                      color = Nest,
                      linetype = Nest,
                      shape = Nest)) +
  xlab(NULL) +
  ylab(NULL) +
  ggtitle("Low density") +
  geom_jitter(size = 6, alpha = 0.5, aes(color = Nest, shape = Nest), width = 5, height = 0.05) +
  geom_line(stat = "smooth", method = lm, se = FALSE, linewidth = 2, color = "black") +
  theme_pubclean() +  
  theme(axis.text.x = element_text(size = 20, color = "black"),
        axis.text.y = element_text(size = 20, color = "black"),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(size = 20, color = "black", hjust = 0.825, vjust = 0.5),
        legend.key = element_blank(),
        legend.justification = c(1, -0.7),
        legend.position = c(1, 0.71),
        legend.direction = "horizontal",
        legend.text = element_text(size = 20, color = "black"),
        legend.title = element_text(size = 20, color = "black"),
        legend.key.size = unit(1, 'cm')) +
  labs(color = "Nest", linetype = "Nest", shape = "Nest") +
  scale_color_manual(breaks = c("Circle", "Tube"), 
                     name = "Nest",
                     values = c("blue", "red"),
                     labels = c("Circle", "Tube")) +
  guides(shape = guide_legend(override.aes = list(alpha = 0.75))) +
  scale_y_continuous(labels = scaleFUN, limits = c(0, 4), breaks = c(0, 2, 4))  

# Occurrence zone size and number of observations for an individual
OccurCol1 <- ggplot(data = WorkerDistScaledRD1_RD2SFZWorking %>% filter(Density == "High") %>% arrange(Nest), 
                    aes(x = Number.ants, y = Occur_Area, 
                        color = Nest,
                        linetype = Nest,
                        shape = Nest)) +
  xlab(NULL) +
  ylab(expression(paste('Occurrence zone size ('*cm^2*')'))) +
  ggtitle("High density") +
  geom_jitter(size = 6, alpha = 0.5, aes(color = Nest, shape = Nest), width = 5, height = 0.05) +
  geom_line(stat = "smooth", method = lm, se = FALSE, linewidth = 2, color = "black") +
  theme_pubclean() + 
  theme(axis.text.x = element_text(size = 18, color = "black"),
        axis.text.y = element_text(size = 18, color = "black"),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 18, color = "black"),
        plot.title = element_text(size = 18, color = "white", hjust = 0.85, vjust = 0.5),
        legend.key = element_blank(),
        legend.justification = c(1, -0.7),
        legend.position = c(1, 0.71),
        legend.direction = "horizontal",
        legend.text = element_text(size = 18, color = "black"),
        legend.title = element_text(size = 18, color = "black"),
        legend.key.size = unit(1, 'cm')) +
  labs(color = "Nest", linetype = "Nest", shape = "Nest") +
  scale_color_manual(breaks = c("Circle", "Tube"), 
                     name = "Nest",
                     values = c("blue", "red"),
                     labels = c("Circle", "Tube")) +
  guides(shape = guide_legend(override.aes = list(alpha = 0.75))) +
  scale_y_continuous(labels = scaleFUN, limits = c(0, 4), breaks = c(0, 2, 4))  

# Low density treatment
OccurCol2 <- ggplot(data = WorkerDistScaledRD1_RD2SFZWorking %>% filter(Density == "Low") %>% arrange(Nest),
                    aes(x = Number.ants, y = Occur_Area, 
                        color = Nest,
                        linetype = Nest,
                        shape = Nest)) +
  xlab(NULL) +
  ylab(NULL) +
  ggtitle("Low density") +
  geom_jitter(size = 6, alpha = 0.5, aes(color = Nest, shape = Nest), width = 5, height = 0.05) +
  geom_line(stat = "smooth", method = lm, se = FALSE, linewidth = 2, color = "black") +
  theme_pubclean() +  
  theme(axis.text.x = element_text(size = 18, color = "black"),
        axis.text.y = element_text(size = 18, color = "black"),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(size = 18, color = "white", hjust = 0.875, vjust = 0.5),
        legend.key = element_blank(),
        legend.justification = c(1, -0.7),
        legend.position = c(1, 0.71),
        legend.direction = "horizontal",
        legend.text = element_text(size = 18, color = "black"),
        legend.title = element_text(size = 18, color = "black"),
        legend.key.size = unit(1, 'cm')) +
  labs(color = "Nest", linetype = "Nest", shape = "Nest") +
  scale_color_manual(breaks = c("Circle", "Tube"), 
                     name = "Nest",
                     values = c("blue", "red"),
                     labels = c("Circle", "Tube")) +
  guides(shape = guide_legend(override.aes = list(alpha = 0.75))) +
  scale_y_continuous(labels = scaleFUN, limits = c(0, 4), breaks = c(0, 2, 4))  

# Compiling worker true occurrence zone size vs. colony size plots
SFZOccurZoneColPlot <- ggarrange(SFZCol1, SFZCol2, OccurCol1, OccurCol2,
                                 labels = c("(a)", "(b)", "(c)", "(d)"),
                                 label.x = 0.875,
                                 label.y = 0.995,
                                 font.label = list(size = 18, face = "plain"),
                                 ncol = 2, nrow = 2,
                                 common.legend = TRUE)

# Annotate the compiled plots to include a common x-axis
FidOccurColSizePlot <- annotate_figure(SFZOccurZoneColPlot,
                                       top = NULL,
                                       bottom = text_grob("Number of workers", color = "black",
                                                          size = 18, x = 0.525, y = 0.98),
                                       left = NULL,
                                       right = NULL
)

# Save plot as a PDF
ggsave(file = "analysis/figures/Fig7.jpg", plot = FidOccurColSizePlot, width = 8.5, height = 7, units = "in", dpi = 400)

####################################################################################################################
# ANALYSES: Spatial fidelity and occurrence zone sizes & distances from the brood center
# The scripts below are to analyze and visualize: 
# Worker spatial fidelity and occurrence zone sizes, and consider how they relate to both nest shape and mean distance from the brood center
####################################################################################################################

# LINEAR MIXED EFFECTS MODEL: Worker spatial fidelity zone size (scaled) and nest shape
# RESPONSE VARIABLE
# SFZ - Worker spatial fidelity zone size (0 - 1, where 1 is the entire area of the nest), zones have at least 3 observations and at least 15% of total observations
# FIXED EFFECTS 
# BroodDist - each worker's average distance to the brood center (0 - 1, where 1 is the longest, shortest distance from the nest entrance)
# Nest - Nest shape (Tube / Circle)
# Density - Nest density (High / Low)
# Number.ants - Colony size
# MeanScaledDist - each worker's average distance to the nest entrance (0 - 1, where 1 is the longest, shortest distance from the nest entrance)
# RANDOM EFFECTS
# (1 | Colony) - Colony identification 
# (1 | AntID) - Worker color identification 
m_SFZBroodDist <- lmer(SFZ ~ Nest * Number.ants + Nest * MeanToBrood + Density * Number.ants + (1 | Colony), data = BroodCentDistWorkersSFZ)

# LINEAR MIXED EFFECTS MODEL: Worker occurrence zone size (scaled) and nest shape
# RESPONSE VARIABLE
# Occur - Worker occurrence zone size (0 - 1, where 1 is the entire area of the nest), zones have at least 3 observations 
# FIXED EFFECTS 
# BroodDist - each worker's average distance to the brood center (0 - 1, where 1 is the longest, shortest distance from the nest entrance)
# Nest - Nest shape (Tube / Circle)
# Density - Nest density (High / Low)
# Number.ants - Colony size
# MeanScaledDist - each worker's average distance to the nest entrance (0 - 1, where 1 is the longest, shortest distance from the nest entrance)
# RANDOM EFFECTS
# (1 | Colony) - Colony identification 
m_OccurBroodDist <- lmer(Occur ~ Nest * Number.ants + Nest * MeanToBrood + Density * Number.ants + (1 | Colony), data = BroodCentDistWorkersSFZ)

# LINEAR MIXED EFFECTS MODEL: Worker spatial fidelity zone size (cm^2) and nest shape
# RESPONSE VARIABLE
# SFZ_Area - Worker spatial fidelity zone size (cm^2), zones have at least 3 observations and at least 15% of total observations
# FIXED EFFECTS 
# BroodDist - each worker's average distance to the brood center (0 - 1, where 1 is the longest, shortest distance from the nest entrance)
# Nest - Nest shape (Tube / Circle)
# Density - Nest density (High / Low)
# Number.ants - Colony size
# MeanScaledDist - each worker's average distance to the nest entrance (0 - 1, where 1 is the longest, shortest distance from the nest entrance)
# RANDOM EFFECTS
# (1 | Colony) - Colony identification 
# (1 | AntID) - Worker color identification 
m_SFZAreaBroodDist <- lmer(SFZ_Area ~ Nest * Number.ants + Nest * MeanToBrood + Density * Number.ants + (1 | Colony), data = BroodCentDistWorkersSFZ)

# LINEAR MIXED EFFECTS MODEL: Worker occurrence zone size (cm^2) and nest shape
# RESPONSE VARIABLE
# Occur - Worker occurrence zone size (cm^2), zones have at least 3 observations 
# FIXED EFFECTS 
# BroodDist - each worker's average distance to the brood center (0 - 1, where 1 is the longest, shortest distance from the nest entrance)
# Nest - Nest shape (Tube / Circle)
# Density - Nest density (High / Low)
# Number.ants - Colony size
# MeanScaledDist - each worker's average distance to the nest entrance (0 - 1, where 1 is the longest, shortest distance from the nest entrance)
# RANDOM EFFECTS
# (1 | Colony) - Colony identification 
m_OccurAreaBroodDist <- lmer(Occur_Area ~ Nest * Number.ants + Nest * MeanToBrood + Density * Number.ants + (1 | Colony), data = BroodCentDistWorkersSFZ)

# All site fidelity v. colony size model outputs in one concise table
tab_model(m_SFZBroodDist, m_SFZAreaBroodDist, m_OccurBroodDist, m_OccurAreaBroodDist, 
          show.df = TRUE, 
          show.stat = TRUE, 
          show.se = TRUE, 
          show.ci = FALSE, 
          show.icc = FALSE,
          auto.label = FALSE,
          dv.labels = c("Scaled Fidelity Zone", "Fidelity Zone (cm2)", "Scaled Occurrence Zone", "Occurrence Zone (cm2)"),
          pred.labels = c("Intercept", "Nest", "ColonySize", "MeanToBrood", "Density","Nest:ColonySize", 
                          "Nest:MeanToBrood", "ColonySize:Density"),
          col.order = c("est", "se", "df.error", "stat", "p"),
          string.pred = "Coeffcient",
          string.est = "Est.",
          string.se = "SE",
          string.stat = "T",
          string.p = "P",
          digits = 3,
          title = "Linear Mixed Effects: individual worker site fidelity v. scaled distance to the brood center",
          file = "analysis/supplementaryMaterials/supplementaryTables/TableA4.html")

webshot("analysis/supplementaryMaterials/supplementaryTables/TableA4.html", 
        "analysis/supplementaryMaterials/supplementaryTables/TableA4.pdf")

Convert2Docx::Converter(pdf_file = "analysis/supplementaryMaterials/supplementaryTables/TableA4.pdf",
                        docx_filename = "analysis/supplementaryMaterials/supplementaryTables/TableA4.docx")

####################################################################################################################
# PLOTS AND ANALYSES: Scaled distance to nest entrance vs. scaled distance to brood center (SFZ data only)
# The scripts below are to analyze and visualize: 
# Scaled distance to nest entrance vs. scaled distance to brood center while only utilizing SFZ data to reduce noise
####################################################################################################################

# FULL DATAFRAME
SFZ_All_Data <- left_join(WorkerDistScaledRD1_RD2SFZWorking, 
          BroodCentDistWorkersSFZ |> select(Colony, Nest, AntID, MeanToBrood)) 

# SCALED DIST TO ENTRANCE VS SCALED DIST TO BROOD
# TUBE NEST
TubeBroodvDist <- SFZ_All_Data %>% 
  filter(Nest == "Tube") %>% 
  ggplot(aes(x = MeanScaledDist, y = MeanToBrood, shape = Density, color = Density)) +
  xlab(NULL) +
  ylab("Average Scaled distance to brood center") +
  ggtitle("Tube Nest") +
  geom_point(size = 3, alpha = 0.33) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1) +
  theme_pubclean() +   
  theme(axis.text.x = element_text(size = 16, color = "black"),
        axis.text.y = element_text(size = 16, color = "black"),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 16, color = "black"),
        plot.title = element_text(size = 16, color = "black", hjust = 0.825, vjust = 0.5),
        legend.key = element_blank(),
        legend.direction = "horizontal",
        legend.text = element_text(size = 16, color = "black"),
        legend.title = element_text(size = 16, color = "black"),
        legend.key.size = unit(1, 'cm')) +
  labs(shape = "Density", color = "Density", alpha = "none") +
  guides(shape = guide_legend(override.aes = list(alpha = 1))) +
  scale_color_manual(breaks = c("High", "Low"), 
                     name = "Density",
                     values = c(lighten("red", 0.4), darken("red", 0.6)),
                     labels = c("High", "Low")) +
  scale_x_continuous(labels = scaleFUNDist, limits = c(0, 1)) +
  scale_y_continuous(labels = scaleFUNDist, limits = c(0, 0.8),
                     breaks = c(0, 0.4, 0.8))

# CIRCLE NEST
CircleBroodvDist <- SFZ_All_Data %>% 
  filter(Nest == "Circle") %>% 
  ggplot(aes(x = MeanScaledDist, y = MeanToBrood, shape = Density, color = Density)) +
  xlab(NULL) +
  ylab("Average Scaled occurrence zone size") +
  ggtitle("Circle Nest") +
  geom_point(size = 3, alpha = 0.33) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1) +
  theme_pubclean() +   
  theme(axis.text.x = element_text(size = 16, color = "black"),
        axis.text.y = element_text(size = 16, color = "black"),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 16, color = "white"),
        plot.title = element_text(size = 16, color = "black", hjust = 0.825, vjust = 0.5),
        legend.key = element_blank(),
        legend.direction = "horizontal",
        legend.text = element_text(size = 16, color = "black"),
        legend.title = element_text(size = 16, color = "black"),
        legend.key.size = unit(1, 'cm')) +
  labs(shape = "Density", color = "Density", alpha = "none") +
  guides(shape = guide_legend(override.aes = list(alpha = 1))) +
  scale_color_manual(breaks = c("High", "Low"), 
                     name = "Density",
                     values = c(lighten("blue", 0.4), darken("blue", 0.6)),
                     labels = c("High", "Low")) +
  scale_x_continuous(labels = scaleFUNDist, limits = c(0, 0.34)) +
  scale_y_continuous(labels = scaleFUNDist, limits = c(0, 0.2),
                     breaks = c(0, 0.1, 0.2))

# Compiling worker scaled dist to entrance vs. scaled dist to brood
BroodScalesDistPlot <- ggarrange(TubeBroodvDist, CircleBroodvDist,
                                 labels = c("(a)", "(b)"),
                                 label.x = 0.875,
                                 font.label = list(size = 16, face = "plain"),
                                 ncol = 2, nrow = 1,
                                 common.legend = FALSE) 

# Annotate the compiled plots to include a common x-axis
BroodScalesDistPlotFinal <- annotate_figure(BroodScalesDistPlot,
                                             top = NULL,
                                             bottom = text_grob("Average scaled distance to nest entrance", color = "black",
                                                                size = 16, x = 0.525),
                                             left = NULL,
                                             right = NULL
)

# Save plot as a PDF
ggsave(file = "analysis/figures/Fig8.jpg", plot = BroodScalesDistPlotFinal, width = 8.5, height = 7, units = "in", dpi = 400)

# LINEAR MIXED EFFECTS MODEL: Queen scaled distances to the brood center
# RESPONSE VARIABLE
# MeanToBrood - Worker scaled distances from the brood center (0 - 1, where 1 is the longest, shortest distance from the nest entrance)
# FIXED EFFECTS 
# MeanScaledDist - Worker scaled distances from the entrance (0 - 1, where 1 is the longest, shortest distance from the nest entrance)
# Nest - Nest shape (Tube / Circle)
# Density - Nest density (High / Low)
# RANDOM EFFECT
# (1 | Colony) - Colony identification 
m_scaledBroodDistScaledDist <- lmer(MeanToBrood ~ MeanScaledDist * Nest + Density + (1 | Colony), data = SFZ_All_Data)

tab_model(m_scaledBroodDistScaledDist,
          show.df = TRUE, 
          show.stat = TRUE, 
          show.se = TRUE, 
          show.ci = FALSE, 
          show.icc = FALSE,
          auto.label = TRUE,
          dv.labels = NULL,
          pred.labels = c("Intercept", "MeanScaledDist", "Nest", "Density",
                          "MeanScaledDist:Nest"),
          col.order = c("est", "se", "df.error", "stat", "p"),
          string.pred = "Coeffcient",
          string.est = "Est.",
          string.se = "SE",
          string.stat = "T",
          string.p = "P",
          digits = 3,
          title = "Linear Mixed Effects: worker scaled distance to brood center vs.<br>scaled distance to nest entrance",
          file = "analysis/supplementaryMaterials/supplementaryTables/TableA4.html")

webshot("analysis/tables/Table3.html", 
        "analysis/tables/Table3.pdf")
