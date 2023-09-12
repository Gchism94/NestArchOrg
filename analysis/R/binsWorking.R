####################################################################################################################
## Author: GREG CHISM
## Date: September 2023
## email: gchism94@gmail.com
## Project: Nest shape influences colony organization in ants: spatial distribution and connectedness of colony members differs from that predicted by random movement and is affected by available space
## Title: Nest section bin functions & nest density calculations
####################################################################################################################

# BIN ASSIGNMENT FUNCTION & NEST SECTION PROPORTIONS
# This code does the following:  
# (1) Processes data for nest section binning (eight even area bins) 
# (2) Bins the data into the eight even area nest sections
# (3) Calculates the proportions of individuals in each nest sections

if (!require(pacman)) install.packages('pacman') # Download package with function to load multiple packaged at once
####################################################################################################################
# Loading required packages for code below  
# p_load() will download packages that aren't in system library
####################################################################################################################

pacman::p_load(here,
               tidyverse)

####################################################################################################################
# IMPORT ALL NECESSARY DATASETS 
# This code imports all necessary data sets for the script below
####################################################################################################################

# WORKERS
# High nest density
FullDataCoordWorkers <- read.csv(here("analysis", "data", "RawData", "FullDataCoordWorkers.csv"), row.names = FALSE)

# Low nest density
FullDataCoordWorkersRD2 <- read.csv(here("analysis", "data", "RawData", "FullDataCoordWorkersRD2.csv"), row.names = FALSE)

# BROOD
# High nest density
FullDataCoordBrood <- read.csv(here("analysis", "data", "RawData", "FullDataCoordBrood.csv"), row.names = FALSE)

# Low nest density
FullDataCoordBroodRD2 <- read.csv(here("analysis", "data", "RawData", "FullDataCoordBroodRD2.csv"), row.names = FALSE)

# QUEENS
# High nest density
FullDataCoordQueen <- read.csv(here("analysis", "data", "RawData", "FullDataCoordQueen.csv"), row.names = FALSE)

# Low nest density
FullDataCoordQueenRD2 <- read.csv(here("analysis", "data", "RawData", "FullDataCoordQueenRD2.csv"), row.names = FALSE)

# ALATES
# Low nest density
FullDataCoordAlate <- read.csv(here("analysis", "data", "RawData", "FullDataCoordAlate.csv"), row.names = FALSE)

# NULL BINS REFERENC
BinsNullFull <- read.csv(here("analysis", "data", "RefData", "BinsNullFull.csv"), row.names = FALSE)

# BIN REFERENCE COORDINATES
BinCoordFull <- read.csv(here("analysis", "data", "RefData", "BinCoordFull.csv"), row.names = FALSE)

####################################################################################################################
# EMPIRICAL DATA PROCESSING 
# This code processes data for binning 
####################################################################################################################

# Joining all databases, to be subset later into individual colony members
# WORKERS
FullDataCoordWorkersRD1_RD2 <- full_join(FullDataCoordWorkers, FullDataCoordWorkersRD2) %>%
  mutate(ColonyMember = "Workers")

# BROOD
FullDataCoordBroodRD1_RD2 <- full_join(FullDataCoordBrood, FullDataCoordBroodRD2) %>%
  mutate(ColonyMember = "Brood")

# QUEENS
FullDataCoordQueenRD1_RD2 <- full_join(FullDataCoordQueen, FullDataCoordQueenRD2) %>%
  mutate(ColonyMember = "Queens")

# ALATES
FullDataCoordAlates <- FullDataCoordAlate %>%
  mutate(ColonyMember = "Alates") %>%
  separate(SexID, c("Alate", "Sex", "SexNumber", "TotalNumber"), sep = ",", remove = TRUE) %>%
  select(-c(Alate))

# Alate sex ratios
# Note, these ratios include "?", where the sex of the alate was uncertain.
# We chose males because they were always present when alates were in the nest.
FullDataCoordAlatesMales <- FullDataCoordAlates %>%
  filter(TotalNumber != "?" & SexNumber != "?") %>%
  group_by(Colony, Nest, Day, Sex) %>% #Group by the Colony, Nest, and Day columns, allowing us to find the below values for each photo.
  mutate(MaxTotal = max(as.numeric(TotalNumber)), #The number of alates in that observation
         MaxSex = max(as.numeric(SexNumber)), #The number of males in that observation
         Ratio = MaxSex / MaxTotal) %>% #The ratio of males / total alates
  filter(Sex == "M") %>%
  ungroup() %>%
  select(Colony, Nest, Day, Ratio) %>% #Select the desired columns
  distinct() #Remove duplicate rows

# Join the alate and male data sets, this is the data set used below
FullDataCoordAlatesRatio <- left_join(FullDataCoordAlates, FullDataCoordAlatesMales)

# ALL COLONY MEMBERS
FullDataCoordAll <- full_join(FullDataCoordWorkersRD1_RD2, FullDataCoordBroodRD1_RD2) %>%
  full_join(FullDataCoordQueenRD1_RD2) %>%
  full_join(FullDataCoordAlatesRatio)

####################################################################################################################
# DATA BINNING FUNCTION
# The code below bins x and y coordinate colony data into eight even area nest sections
# To do this, a reference data set of bin coordinates is used and coordinates are run through a series of conditional statements
# Where each conditional statement checks whether the coordinate is in one of eight bins sequentially

# The code is set up such that you can run each colony sequentially, which was done to avoid errors in loops that can result in losing an entire set of data 
####################################################################################################################

# COLONY 1
CoordBinned <- function(data_table){
  # Selecting the colony bin references
  BinCoordAssign <- BinCoordFull %>%
    filter(Colony == "1")
  
  # Binning the tube colony member coordinates
  # Filtering out the tube data for the selected colony
  Colony1BinnedTube <- data_table %>%
    filter(Colony == "1" & Nest == "Tube") %>%
    # Ifelse conditional statements for each bin
    mutate(Bin =
             # The bins determine whether the x and y coordinates are within a set of relevant bounds for each bin
             # The order is always x coordinates first, then y
             # Bin 1
             if_else(
               ScaledX >= BinCoordAssign$ScaledX[2] & 
                 ScaledX <= BinCoordAssign$ScaledX[1] &
                 ScaledY <= BinCoordAssign$ScaledY[2], 1,
               # Bin 2
               if_else(ScaledX >= BinCoordAssign$ScaledX[3] & 
                         ScaledX <= BinCoordAssign$ScaledX[2] &
                         ScaledY >= BinCoordAssign$ScaledY[3] &
                         ScaledY <= BinCoordAssign$ScaledY[2], 2,
                       # Bin 3
                       if_else(ScaledX >= BinCoordAssign$ScaledX[4] & 
                                 ScaledX <= BinCoordAssign$ScaledX[3] &
                                 ScaledY >= BinCoordAssign$ScaledY[3] &
                                 ScaledY <= BinCoordAssign$ScaledY[4], 3,
                               # Bin 4
                               if_else(ScaledX >= BinCoordAssign$ScaledX[4] & 
                                         ScaledX <= BinCoordAssign$ScaledX[5] &
                                         ScaledY >= BinCoordAssign$ScaledY[4] &
                                         ScaledY <= BinCoordAssign$ScaledY[5], 4,
                                       # Bin 5
                                       if_else(ScaledX >= BinCoordAssign$ScaledX[5] & 
                                                 ScaledX <= BinCoordAssign$ScaledX[6] &
                                                 ScaledY >= BinCoordAssign$ScaledY[4] &
                                                 ScaledY <= BinCoordAssign$ScaledY[5], 5,
                                               # Bin 6
                                               if_else(ScaledX >= BinCoordAssign$ScaledX[6] & 
                                                         ScaledX <= BinCoordAssign$ScaledX[7] &
                                                         ScaledY >= BinCoordAssign$ScaledY[7] &
                                                         ScaledY <= BinCoordAssign$ScaledY[6], 6,
                                                       # Bin 7
                                                       if_else(ScaledX >= BinCoordAssign$ScaledX[8] & 
                                                                 ScaledX <= BinCoordAssign$ScaledX[7] &
                                                                 ScaledY <= BinCoordAssign$ScaledY[7], 7,
                                                               # Bin 8
                                                               if_else(ScaledX >= BinCoordAssign$ScaledX[9] & 
                                                                         ScaledX <= BinCoordAssign$ScaledX[8] &
                                                                         ScaledY <= BinCoordAssign$ScaledY[9], 8, 0
                                                                       )))))))))
  # Binning circle nest coordinates 
  # Filtering out the circle data for the selected colony
  # The code is the same as above, except its only y coordinates since xlims don't matter for the binning to work 
  Colony1BinnedCircle <- data_table %>%
    filter(Colony == "1" & Nest == "Circle") %>%
    mutate(Bin =
             # Bin 1
             if_else(ScaledY <= BinCoordAssign$ScaledY[10], 1,
                     # Bin 2
                     if_else(ScaledY >= BinCoordAssign$ScaledY[10] & 
                               ScaledY <= BinCoordAssign$ScaledY[11], 2,
                             # Bin 3
                             if_else(ScaledY >= BinCoordAssign$ScaledY[11] & 
                                       ScaledY <= BinCoordAssign$ScaledY[12], 3,
                                     # Bin 4
                                     if_else(ScaledY >= BinCoordAssign$ScaledY[12] & 
                                               ScaledY <= BinCoordAssign$ScaledY[13], 4,
                                             # Bin 5
                                             if_else(ScaledY >= BinCoordAssign$ScaledY[13] & 
                                                       ScaledY <= BinCoordAssign$ScaledY[14], 5,
                                                     # Bin 6
                                                     if_else(ScaledY >= BinCoordAssign$ScaledY[14] & 
                                                               ScaledY <= BinCoordAssign$ScaledY[15], 6,
                                                             # Bin 7
                                                             if_else(ScaledY >= BinCoordAssign$ScaledY[15] & 
                                                                       ScaledY <= BinCoordAssign$ScaledY[16], 7,
                                                                     # Bin 8
                                                                     if_else(ScaledY >= BinCoordAssign$ScaledY[16], 8, 0
                                                                             )))))))))
  Colony1Binned <<- full_join(Colony1BinnedTube, Colony1BinnedCircle)
}

# Run the eight nest section binning function for the combined colony member data set
CoordBinned(FullDataCoordAll) 

# COLONY 2
CoordBinned <- function(data_table){
  # Selecting the colony bin references
  BinCoordAssign <- BinCoordFull %>%
    filter(Colony == "2")
  
  # Binning the tube colony member coordinates
  # Filtering out the tube data for the selected colony
  Colony2BinnedTube <- data_table %>%
    filter(Colony == "2" & Nest == "Tube") %>%
    # Ifelse conditional statements for each bin
    mutate(Bin =
             # The bins determine whether the x and y coordinates are within a set of relevant bounds for each bin
             # The order is always x coordinates first, then y
             # Bin 1
             if_else(
               ScaledX >= BinCoordAssign$ScaledX[2] & 
                 ScaledX <= BinCoordAssign$ScaledX[1] &
                 ScaledY <= BinCoordAssign$ScaledY[2], 1,
               # Bin 2
               if_else(ScaledX >= BinCoordAssign$ScaledX[3] & 
                         ScaledX <= BinCoordAssign$ScaledX[2] &
                         ScaledY >= BinCoordAssign$ScaledY[3] &
                         ScaledY <= BinCoordAssign$ScaledY[2], 2,
                       # Bin 3
                       if_else(ScaledX >= BinCoordAssign$ScaledX[4] & 
                                 ScaledX <= BinCoordAssign$ScaledX[3] &
                                 ScaledY >= BinCoordAssign$ScaledY[3] &
                                 ScaledY <= BinCoordAssign$ScaledY[4], 3,
                               # Bin 4
                               if_else(ScaledX >= BinCoordAssign$ScaledX[4] & 
                                         ScaledX <= BinCoordAssign$ScaledX[5] &
                                         ScaledY >= BinCoordAssign$ScaledY[4] &
                                         ScaledY <= BinCoordAssign$ScaledY[5], 4,
                                       # Bin 5
                                       if_else(ScaledX >= BinCoordAssign$ScaledX[5] & 
                                                 ScaledX <= BinCoordAssign$ScaledX[6] &
                                                 ScaledY >= BinCoordAssign$ScaledY[4] &
                                                 ScaledY <= BinCoordAssign$ScaledY[5], 5,
                                               # Bin 6
                                               if_else(ScaledX >= BinCoordAssign$ScaledX[6] & 
                                                         ScaledX <= BinCoordAssign$ScaledX[7] &
                                                         ScaledY >= BinCoordAssign$ScaledY[7] &
                                                         ScaledY <= BinCoordAssign$ScaledY[6], 6,
                                                       # Bin 7
                                                       if_else(ScaledX >= BinCoordAssign$ScaledX[8] & 
                                                                 ScaledX <= BinCoordAssign$ScaledX[7] &
                                                                 ScaledY <= BinCoordAssign$ScaledY[7], 7,
                                                               # Bin 8
                                                               if_else(ScaledX >= BinCoordAssign$ScaledX[9] & 
                                                                         ScaledX <= BinCoordAssign$ScaledX[8] &
                                                                         ScaledY <= BinCoordAssign$ScaledY[9], 8, 0
                                                               )))))))))
  # Binning circle nest coordinates 
  # Filtering out the circle data for the selected colony
  # The code is the same as above, except its only y coordinates since xlims don't matter for the binning to work 
  Colony2BinnedCircle <- data_table %>%
    filter(Colony == "2" & Nest == "Circle") %>%
    mutate(Bin =
             # Bin 1
             if_else(ScaledY <= BinCoordAssign$ScaledY[10], 1,
                     # Bin 2
                     if_else(ScaledY >= BinCoordAssign$ScaledY[10] & 
                               ScaledY <= BinCoordAssign$ScaledY[11], 2,
                             # Bin 3
                             if_else(ScaledY >= BinCoordAssign$ScaledY[11] & 
                                       ScaledY <= BinCoordAssign$ScaledY[12], 3,
                                     # Bin 4
                                     if_else(ScaledY >= BinCoordAssign$ScaledY[12] & 
                                               ScaledY <= BinCoordAssign$ScaledY[13], 4,
                                             # Bin 5
                                             if_else(ScaledY >= BinCoordAssign$ScaledY[13] & 
                                                       ScaledY <= BinCoordAssign$ScaledY[14], 5,
                                                     # Bin 6
                                                     if_else(ScaledY >= BinCoordAssign$ScaledY[14] & 
                                                               ScaledY <= BinCoordAssign$ScaledY[15], 6,
                                                             # Bin 7
                                                             if_else(ScaledY >= BinCoordAssign$ScaledY[15] & 
                                                                       ScaledY <= BinCoordAssign$ScaledY[16], 7,
                                                                     # Bin 8
                                                                     if_else(ScaledY >= BinCoordAssign$ScaledY[16], 8, 0
                                                                     )))))))))
  Colony2Binned <<- full_join(Colony2BinnedTube, Colony2BinnedCircle)
}

# Run the eight nest section binning function for the combined colony member data set
CoordBinned(FullDataCoordAll) 

# COLONY 3
CoordBinned <- function(data_table){
  # Selecting the colony bin references
  BinCoordAssign <- BinCoordFull %>%
    filter(Colony == "3")
  
  # Binning the tube colony member coordinates
  # Filtering out the tube data for the selected colony
  Colony3BinnedTube <- data_table %>%
    filter(Colony == "3" & Nest == "Tube") %>%
    # Ifelse conditional statements for each bin
    mutate(Bin =
             # The bins determine whether the x and y coordinates are within a set of relevant bounds for each bin
             # The order is always x coordinates first, then y
             # Bin 1
             if_else(
               ScaledX >= BinCoordAssign$ScaledX[2] & 
                 ScaledX <= BinCoordAssign$ScaledX[1] &
                 ScaledY <= BinCoordAssign$ScaledY[2], 1,
               # Bin 2
               if_else(ScaledX >= BinCoordAssign$ScaledX[3] & 
                         ScaledX <= BinCoordAssign$ScaledX[2] &
                         ScaledY >= BinCoordAssign$ScaledY[3] &
                         ScaledY <= BinCoordAssign$ScaledY[2], 2,
                       # Bin 3
                       if_else(ScaledX >= BinCoordAssign$ScaledX[4] & 
                                 ScaledX <= BinCoordAssign$ScaledX[3] &
                                 ScaledY >= BinCoordAssign$ScaledY[3] &
                                 ScaledY <= BinCoordAssign$ScaledY[4], 3,
                               # Bin 4
                               if_else(ScaledX >= BinCoordAssign$ScaledX[4] & 
                                         ScaledX <= BinCoordAssign$ScaledX[5] &
                                         ScaledY >= BinCoordAssign$ScaledY[4] &
                                         ScaledY <= BinCoordAssign$ScaledY[5], 4,
                                       # Bin 5
                                       if_else(ScaledX >= BinCoordAssign$ScaledX[5] & 
                                                 ScaledX <= BinCoordAssign$ScaledX[6] &
                                                 ScaledY >= BinCoordAssign$ScaledY[4] &
                                                 ScaledY <= BinCoordAssign$ScaledY[5], 5,
                                               # Bin 6
                                               if_else(ScaledX >= BinCoordAssign$ScaledX[6] & 
                                                         ScaledX <= BinCoordAssign$ScaledX[7] &
                                                         ScaledY >= BinCoordAssign$ScaledY[7] &
                                                         ScaledY <= BinCoordAssign$ScaledY[6], 6,
                                                       # Bin 7
                                                       if_else(ScaledX >= BinCoordAssign$ScaledX[8] & 
                                                                 ScaledX <= BinCoordAssign$ScaledX[7] &
                                                                 ScaledY <= BinCoordAssign$ScaledY[7], 7,
                                                               # Bin 8
                                                               if_else(ScaledX >= BinCoordAssign$ScaledX[9] & 
                                                                         ScaledX <= BinCoordAssign$ScaledX[8] &
                                                                         ScaledY <= BinCoordAssign$ScaledY[9], 8, 0
                                                               )))))))))
  # Binning circle nest coordinates 
  # Filtering out the circle data for the selected colony
  # The code is the same as above, except its only y coordinates since xlims don't matter for the binning to work 
  Colony3BinnedCircle <- data_table %>%
    filter(Colony == "3" & Nest == "Circle") %>%
    mutate(Bin =
             # Bin 1
             if_else(ScaledY <= BinCoordAssign$ScaledY[10], 1,
                     # Bin 2
                     if_else(ScaledY >= BinCoordAssign$ScaledY[10] & 
                               ScaledY <= BinCoordAssign$ScaledY[11], 2,
                             # Bin 3
                             if_else(ScaledY >= BinCoordAssign$ScaledY[11] & 
                                       ScaledY <= BinCoordAssign$ScaledY[12], 3,
                                     # Bin 4
                                     if_else(ScaledY >= BinCoordAssign$ScaledY[12] & 
                                               ScaledY <= BinCoordAssign$ScaledY[13], 4,
                                             # Bin 5
                                             if_else(ScaledY >= BinCoordAssign$ScaledY[13] & 
                                                       ScaledY <= BinCoordAssign$ScaledY[14], 5,
                                                     # Bin 6
                                                     if_else(ScaledY >= BinCoordAssign$ScaledY[14] & 
                                                               ScaledY <= BinCoordAssign$ScaledY[15], 6,
                                                             # Bin 7
                                                             if_else(ScaledY >= BinCoordAssign$ScaledY[15] & 
                                                                       ScaledY <= BinCoordAssign$ScaledY[16], 7,
                                                                     # Bin 8
                                                                     if_else(ScaledY >= BinCoordAssign$ScaledY[16], 8, 0
                                                                     )))))))))
  Colony3Binned <<- full_join(Colony3BinnedTube, Colony3BinnedCircle)
}

# Run the eight nest section binning function for the combined colony member data set
CoordBinned(FullDataCoordAll) 

# COLONY 4
CoordBinned <- function(data_table){
  # Selecting the colony bin references
  BinCoordAssign <- BinCoordFull %>%
    filter(Colony == "4")
  
  # Binning the tube colony member coordinates
  # Filtering out the tube data for the selected colony
  Colony4BinnedTube <- data_table %>%
    filter(Colony == "4" & Nest == "Tube") %>%
    # Ifelse conditional statements for each bin
    mutate(Bin =
             # The bins determine whether the x and y coordinates are within a set of relevant bounds for each bin
             # The order is always x coordinates first, then y
             # Bin 1
             if_else(
               ScaledX >= BinCoordAssign$ScaledX[2] & 
                 ScaledX <= BinCoordAssign$ScaledX[1] &
                 ScaledY <= BinCoordAssign$ScaledY[2], 1,
               # Bin 2
               if_else(ScaledX >= BinCoordAssign$ScaledX[3] & 
                         ScaledX <= BinCoordAssign$ScaledX[2] &
                         ScaledY >= BinCoordAssign$ScaledY[3] &
                         ScaledY <= BinCoordAssign$ScaledY[2], 2,
                       # Bin 3
                       if_else(ScaledX >= BinCoordAssign$ScaledX[4] & 
                                 ScaledX <= BinCoordAssign$ScaledX[3] &
                                 ScaledY >= BinCoordAssign$ScaledY[3] &
                                 ScaledY <= BinCoordAssign$ScaledY[4], 3,
                               # Bin 4
                               if_else(ScaledX >= BinCoordAssign$ScaledX[4] & 
                                         ScaledX <= BinCoordAssign$ScaledX[5] &
                                         ScaledY >= BinCoordAssign$ScaledY[4] &
                                         ScaledY <= BinCoordAssign$ScaledY[5], 4,
                                       # Bin 5
                                       if_else(ScaledX >= BinCoordAssign$ScaledX[5] & 
                                                 ScaledX <= BinCoordAssign$ScaledX[6] &
                                                 ScaledY >= BinCoordAssign$ScaledY[4] &
                                                 ScaledY <= BinCoordAssign$ScaledY[5], 5,
                                               # Bin 6
                                               if_else(ScaledX >= BinCoordAssign$ScaledX[6] & 
                                                         ScaledX <= BinCoordAssign$ScaledX[7] &
                                                         ScaledY >= BinCoordAssign$ScaledY[7] &
                                                         ScaledY <= BinCoordAssign$ScaledY[6], 6,
                                                       # Bin 7
                                                       if_else(ScaledX >= BinCoordAssign$ScaledX[8] & 
                                                                 ScaledX <= BinCoordAssign$ScaledX[7] &
                                                                 ScaledY <= BinCoordAssign$ScaledY[7], 7,
                                                               # Bin 8
                                                               if_else(ScaledX >= BinCoordAssign$ScaledX[9] & 
                                                                         ScaledX <= BinCoordAssign$ScaledX[8] &
                                                                         ScaledY <= BinCoordAssign$ScaledY[9], 8, 0
                                                               )))))))))
  # Binning circle nest coordinates 
  # Filtering out the circle data for the selected colony
  # The code is the same as above, except its only y coordinates since xlims don't matter for the binning to work 
  Colony4BinnedCircle <- data_table %>%
    filter(Colony == "4" & Nest == "Circle") %>%
    mutate(Bin =
             # Bin 1
             if_else(ScaledY <= BinCoordAssign$ScaledY[10], 1,
                     # Bin 2
                     if_else(ScaledY >= BinCoordAssign$ScaledY[10] & 
                               ScaledY <= BinCoordAssign$ScaledY[11], 2,
                             # Bin 3
                             if_else(ScaledY >= BinCoordAssign$ScaledY[11] & 
                                       ScaledY <= BinCoordAssign$ScaledY[12], 3,
                                     # Bin 4
                                     if_else(ScaledY >= BinCoordAssign$ScaledY[12] & 
                                               ScaledY <= BinCoordAssign$ScaledY[13], 4,
                                             # Bin 5
                                             if_else(ScaledY >= BinCoordAssign$ScaledY[13] & 
                                                       ScaledY <= BinCoordAssign$ScaledY[14], 5,
                                                     # Bin 6
                                                     if_else(ScaledY >= BinCoordAssign$ScaledY[14] & 
                                                               ScaledY <= BinCoordAssign$ScaledY[15], 6,
                                                             # Bin 7
                                                             if_else(ScaledY >= BinCoordAssign$ScaledY[15] & 
                                                                       ScaledY <= BinCoordAssign$ScaledY[16], 7,
                                                                     # Bin 8
                                                                     if_else(ScaledY >= BinCoordAssign$ScaledY[16], 8, 0
                                                                     )))))))))
  Colony4Binned <<- full_join(Colony4BinnedTube, Colony4BinnedCircle)
}

# Run the eight nest section binning function for the combined colony member data set
CoordBinned(FullDataCoordAll) 

# COLONY 5
CoordBinned <- function(data_table){
  # Selecting the colony bin references
  BinCoordAssign <- BinCoordFull %>%
    filter(Colony == "5")
  
  # Binning the tube colony member coordinates
  # Filtering out the tube data for the selected colony
  Colony5BinnedTube <- data_table %>%
    filter(Colony == "5" & Nest == "Tube") %>%
    # Ifelse conditional statements for each bin
    mutate(Bin =
             # The bins determine whether the x and y coordinates are within a set of relevant bounds for each bin
             # The order is always x coordinates first, then y
             # Bin 1
             if_else(
               ScaledX >= BinCoordAssign$ScaledX[2] & 
                 ScaledX <= BinCoordAssign$ScaledX[1] &
                 ScaledY <= BinCoordAssign$ScaledY[2], 1,
               # Bin 2
               if_else(ScaledX >= BinCoordAssign$ScaledX[3] & 
                         ScaledX <= BinCoordAssign$ScaledX[2] &
                         ScaledY >= BinCoordAssign$ScaledY[3] &
                         ScaledY <= BinCoordAssign$ScaledY[2], 2,
                       # Bin 3
                       if_else(ScaledX >= BinCoordAssign$ScaledX[4] & 
                                 ScaledX <= BinCoordAssign$ScaledX[3] &
                                 ScaledY >= BinCoordAssign$ScaledY[3] &
                                 ScaledY <= BinCoordAssign$ScaledY[4], 3,
                               # Bin 4
                               if_else(ScaledX >= BinCoordAssign$ScaledX[4] & 
                                         ScaledX <= BinCoordAssign$ScaledX[5] &
                                         ScaledY >= BinCoordAssign$ScaledY[4] &
                                         ScaledY <= BinCoordAssign$ScaledY[5], 4,
                                       # Bin 5
                                       if_else(ScaledX >= BinCoordAssign$ScaledX[5] & 
                                                 ScaledX <= BinCoordAssign$ScaledX[6] &
                                                 ScaledY >= BinCoordAssign$ScaledY[4] &
                                                 ScaledY <= BinCoordAssign$ScaledY[5], 5,
                                               # Bin 6
                                               if_else(ScaledX >= BinCoordAssign$ScaledX[6] & 
                                                         ScaledX <= BinCoordAssign$ScaledX[7] &
                                                         ScaledY >= BinCoordAssign$ScaledY[7] &
                                                         ScaledY <= BinCoordAssign$ScaledY[6], 6,
                                                       # Bin 7
                                                       if_else(ScaledX >= BinCoordAssign$ScaledX[8] & 
                                                                 ScaledX <= BinCoordAssign$ScaledX[7] &
                                                                 ScaledY <= BinCoordAssign$ScaledY[7], 7,
                                                               # Bin 8
                                                               if_else(ScaledX >= BinCoordAssign$ScaledX[9] & 
                                                                         ScaledX <= BinCoordAssign$ScaledX[8] &
                                                                         ScaledY <= BinCoordAssign$ScaledY[9], 8, 0
                                                               )))))))))
  # Binning circle nest coordinates 
  # Filtering out the circle data for the selected colony
  # The code is the same as above, except its only y coordinates since xlims don't matter for the binning to work 
  Colony5BinnedCircle <- data_table %>%
    filter(Colony == "5" & Nest == "Circle") %>%
    mutate(Bin =
             # Bin 1
             if_else(ScaledY <= BinCoordAssign$ScaledY[10], 1,
                     # Bin 2
                     if_else(ScaledY >= BinCoordAssign$ScaledY[10] & 
                               ScaledY <= BinCoordAssign$ScaledY[11], 2,
                             # Bin 3
                             if_else(ScaledY >= BinCoordAssign$ScaledY[11] & 
                                       ScaledY <= BinCoordAssign$ScaledY[12], 3,
                                     # Bin 4
                                     if_else(ScaledY >= BinCoordAssign$ScaledY[12] & 
                                               ScaledY <= BinCoordAssign$ScaledY[13], 4,
                                             # Bin 5
                                             if_else(ScaledY >= BinCoordAssign$ScaledY[13] & 
                                                       ScaledY <= BinCoordAssign$ScaledY[14], 5,
                                                     # Bin 6
                                                     if_else(ScaledY >= BinCoordAssign$ScaledY[14] & 
                                                               ScaledY <= BinCoordAssign$ScaledY[15], 6,
                                                             # Bin 7
                                                             if_else(ScaledY >= BinCoordAssign$ScaledY[15] & 
                                                                       ScaledY <= BinCoordAssign$ScaledY[16], 7,
                                                                     # Bin 8
                                                                     if_else(ScaledY >= BinCoordAssign$ScaledY[16], 8, 0
                                                                     )))))))))
  Colony5Binned <<- full_join(Colony5BinnedTube, Colony5BinnedCircle)
}

# Run the eight nest section binning function for the combined colony member data set
CoordBinned(FullDataCoordAll) 

# COLONY 6
CoordBinned <- function(data_table){
  # Selecting the colony bin references
  BinCoordAssign <- BinCoordFull %>%
    filter(Colony == "6")
  
  # Binning the tube colony member coordinates
  # Filtering out the tube data for the selected colony
  Colony6BinnedTube <- data_table %>%
    filter(Colony == "6" & Nest == "Tube") %>%
    # Ifelse conditional statements for each bin
    mutate(Bin =
             # The bins determine whether the x and y coordinates are within a set of relevant bounds for each bin
             # The order is always x coordinates first, then y
             # Bin 1
             if_else(
               ScaledX >= BinCoordAssign$ScaledX[2] & 
                 ScaledX <= BinCoordAssign$ScaledX[1] &
                 ScaledY <= BinCoordAssign$ScaledY[2], 1,
               # Bin 2
               if_else(ScaledX >= BinCoordAssign$ScaledX[3] & 
                         ScaledX <= BinCoordAssign$ScaledX[2] &
                         ScaledY >= BinCoordAssign$ScaledY[3] &
                         ScaledY <= BinCoordAssign$ScaledY[2], 2,
                       # Bin 3
                       if_else(ScaledX >= BinCoordAssign$ScaledX[4] & 
                                 ScaledX <= BinCoordAssign$ScaledX[3] &
                                 ScaledY >= BinCoordAssign$ScaledY[3] &
                                 ScaledY <= BinCoordAssign$ScaledY[4], 3,
                               # Bin 4
                               if_else(ScaledX >= BinCoordAssign$ScaledX[4] & 
                                         ScaledX <= BinCoordAssign$ScaledX[5] &
                                         ScaledY >= BinCoordAssign$ScaledY[4] &
                                         ScaledY <= BinCoordAssign$ScaledY[5], 4,
                                       # Bin 5
                                       if_else(ScaledX >= BinCoordAssign$ScaledX[5] & 
                                                 ScaledX <= BinCoordAssign$ScaledX[6] &
                                                 ScaledY >= BinCoordAssign$ScaledY[4] &
                                                 ScaledY <= BinCoordAssign$ScaledY[5], 5,
                                               # Bin 6
                                               if_else(ScaledX >= BinCoordAssign$ScaledX[6] & 
                                                         ScaledX <= BinCoordAssign$ScaledX[7] &
                                                         ScaledY >= BinCoordAssign$ScaledY[7] &
                                                         ScaledY <= BinCoordAssign$ScaledY[6], 6,
                                                       # Bin 7
                                                       if_else(ScaledX >= BinCoordAssign$ScaledX[8] & 
                                                                 ScaledX <= BinCoordAssign$ScaledX[7] &
                                                                 ScaledY <= BinCoordAssign$ScaledY[7], 7,
                                                               # Bin 8
                                                               if_else(ScaledX >= BinCoordAssign$ScaledX[9] & 
                                                                         ScaledX <= BinCoordAssign$ScaledX[8] &
                                                                         ScaledY <= BinCoordAssign$ScaledY[9], 8, 0
                                                               )))))))))
  # Binning circle nest coordinates 
  # Filtering out the circle data for the selected colony
  # The code is the same as above, except its only y coordinates since xlims don't matter for the binning to work 
  Colony6BinnedCircle <- data_table %>%
    filter(Colony == "6" & Nest == "Circle") %>%
    mutate(Bin =
             # Bin 1
             if_else(ScaledY <= BinCoordAssign$ScaledY[10], 1,
                     # Bin 2
                     if_else(ScaledY >= BinCoordAssign$ScaledY[10] & 
                               ScaledY <= BinCoordAssign$ScaledY[11], 2,
                             # Bin 3
                             if_else(ScaledY >= BinCoordAssign$ScaledY[11] & 
                                       ScaledY <= BinCoordAssign$ScaledY[12], 3,
                                     # Bin 4
                                     if_else(ScaledY >= BinCoordAssign$ScaledY[12] & 
                                               ScaledY <= BinCoordAssign$ScaledY[13], 4,
                                             # Bin 5
                                             if_else(ScaledY >= BinCoordAssign$ScaledY[13] & 
                                                       ScaledY <= BinCoordAssign$ScaledY[14], 5,
                                                     # Bin 6
                                                     if_else(ScaledY >= BinCoordAssign$ScaledY[14] & 
                                                               ScaledY <= BinCoordAssign$ScaledY[15], 6,
                                                             # Bin 7
                                                             if_else(ScaledY >= BinCoordAssign$ScaledY[15] & 
                                                                       ScaledY <= BinCoordAssign$ScaledY[16], 7,
                                                                     # Bin 8
                                                                     if_else(ScaledY >= BinCoordAssign$ScaledY[16], 8, 0
                                                                     )))))))))
  Colony6Binned <<- full_join(Colony6BinnedTube, Colony6BinnedCircle)
}

# Run the eight nest section binning function for the combined colony member data set
CoordBinned(FullDataCoordAll) 

# COLONY 7
CoordBinned <- function(data_table){
  # Selecting the colony bin references
  BinCoordAssign <- BinCoordFull %>%
    filter(Colony == "7")
  
  # Binning the tube colony member coordinates
  # Filtering out the tube data for the selected colony
  Colony7BinnedTube <- data_table %>%
    filter(Colony == "7" & Nest == "Tube") %>%
    # Ifelse conditional statements for each bin
    mutate(Bin =
             # The bins determine whether the x and y coordinates are within a set of relevant bounds for each bin
             # The order is always x coordinates first, then y
             # Bin 1
             if_else(
               ScaledX >= BinCoordAssign$ScaledX[2] & 
                 ScaledX <= BinCoordAssign$ScaledX[1] &
                 ScaledY <= BinCoordAssign$ScaledY[2], 1,
               # Bin 2
               if_else(ScaledX >= BinCoordAssign$ScaledX[3] & 
                         ScaledX <= BinCoordAssign$ScaledX[2] &
                         ScaledY >= BinCoordAssign$ScaledY[3] &
                         ScaledY <= BinCoordAssign$ScaledY[2], 2,
                       # Bin 3
                       if_else(ScaledX >= BinCoordAssign$ScaledX[4] & 
                                 ScaledX <= BinCoordAssign$ScaledX[3] &
                                 ScaledY >= BinCoordAssign$ScaledY[3] &
                                 ScaledY <= BinCoordAssign$ScaledY[4], 3,
                               # Bin 4
                               if_else(ScaledX >= BinCoordAssign$ScaledX[4] & 
                                         ScaledX <= BinCoordAssign$ScaledX[5] &
                                         ScaledY >= BinCoordAssign$ScaledY[4] &
                                         ScaledY <= BinCoordAssign$ScaledY[5], 4,
                                       # Bin 5
                                       if_else(ScaledX >= BinCoordAssign$ScaledX[5] & 
                                                 ScaledX <= BinCoordAssign$ScaledX[6] &
                                                 ScaledY >= BinCoordAssign$ScaledY[4] &
                                                 ScaledY <= BinCoordAssign$ScaledY[5], 5,
                                               # Bin 6
                                               if_else(ScaledX >= BinCoordAssign$ScaledX[6] & 
                                                         ScaledX <= BinCoordAssign$ScaledX[7] &
                                                         ScaledY >= BinCoordAssign$ScaledY[7] &
                                                         ScaledY <= BinCoordAssign$ScaledY[6], 6,
                                                       # Bin 7
                                                       if_else(ScaledX >= BinCoordAssign$ScaledX[8] & 
                                                                 ScaledX <= BinCoordAssign$ScaledX[7] &
                                                                 ScaledY <= BinCoordAssign$ScaledY[7], 7,
                                                               # Bin 8
                                                               if_else(ScaledX >= BinCoordAssign$ScaledX[9] & 
                                                                         ScaledX <= BinCoordAssign$ScaledX[8] &
                                                                         ScaledY <= BinCoordAssign$ScaledY[9], 8, 0
                                                               )))))))))
  # Binning circle nest coordinates 
  # Filtering out the circle data for the selected colony
  # The code is the same as above, except its only y coordinates since xlims don't matter for the binning to work 
  Colony7BinnedCircle <- data_table %>%
    filter(Colony == "7" & Nest == "Circle") %>%
    mutate(Bin =
             # Bin 1
             if_else(ScaledY <= BinCoordAssign$ScaledY[10], 1,
                     # Bin 2
                     if_else(ScaledY >= BinCoordAssign$ScaledY[10] & 
                               ScaledY <= BinCoordAssign$ScaledY[11], 2,
                             # Bin 3
                             if_else(ScaledY >= BinCoordAssign$ScaledY[11] & 
                                       ScaledY <= BinCoordAssign$ScaledY[12], 3,
                                     # Bin 4
                                     if_else(ScaledY >= BinCoordAssign$ScaledY[12] & 
                                               ScaledY <= BinCoordAssign$ScaledY[13], 4,
                                             # Bin 5
                                             if_else(ScaledY >= BinCoordAssign$ScaledY[13] & 
                                                       ScaledY <= BinCoordAssign$ScaledY[14], 5,
                                                     # Bin 6
                                                     if_else(ScaledY >= BinCoordAssign$ScaledY[14] & 
                                                               ScaledY <= BinCoordAssign$ScaledY[15], 6,
                                                             # Bin 7
                                                             if_else(ScaledY >= BinCoordAssign$ScaledY[15] & 
                                                                       ScaledY <= BinCoordAssign$ScaledY[16], 7,
                                                                     # Bin 8
                                                                     if_else(ScaledY >= BinCoordAssign$ScaledY[16], 8, 0
                                                                     )))))))))
  Colony7Binned <<- full_join(Colony7BinnedTube, Colony7BinnedCircle)
}

# Run the eight nest section binning function for the combined colony member data set
CoordBinned(FullDataCoordAll) 

# COLONY 8
CoordBinned <- function(data_table){
  # Selecting the colony bin references
  BinCoordAssign <- BinCoordFull %>%
    filter(Colony == "8")
  
  # Binning the tube colony member coordinates
  # Filtering out the tube data for the selected colony
  Colony8BinnedTube <- data_table %>%
    filter(Colony == "8" & Nest == "Tube") %>%
    # Ifelse conditional statements for each bin
    mutate(Bin =
             # The bins determine whether the x and y coordinates are within a set of relevant bounds for each bin
             # The order is always x coordinates first, then y
             # Bin 1
             if_else(
               ScaledX >= BinCoordAssign$ScaledX[2] & 
                 ScaledX <= BinCoordAssign$ScaledX[1] &
                 ScaledY <= BinCoordAssign$ScaledY[2], 1,
               # Bin 2
               if_else(ScaledX >= BinCoordAssign$ScaledX[3] & 
                         ScaledX <= BinCoordAssign$ScaledX[2] &
                         ScaledY >= BinCoordAssign$ScaledY[3] &
                         ScaledY <= BinCoordAssign$ScaledY[2], 2,
                       # Bin 3
                       if_else(ScaledX >= BinCoordAssign$ScaledX[4] & 
                                 ScaledX <= BinCoordAssign$ScaledX[3] &
                                 ScaledY >= BinCoordAssign$ScaledY[3] &
                                 ScaledY <= BinCoordAssign$ScaledY[4], 3,
                               # Bin 4
                               if_else(ScaledX >= BinCoordAssign$ScaledX[4] & 
                                         ScaledX <= BinCoordAssign$ScaledX[5] &
                                         ScaledY >= BinCoordAssign$ScaledY[4] &
                                         ScaledY <= BinCoordAssign$ScaledY[5], 4,
                                       # Bin 5
                                       if_else(ScaledX >= BinCoordAssign$ScaledX[5] & 
                                                 ScaledX <= BinCoordAssign$ScaledX[6] &
                                                 ScaledY >= BinCoordAssign$ScaledY[4] &
                                                 ScaledY <= BinCoordAssign$ScaledY[5], 5,
                                               # Bin 6
                                               if_else(ScaledX >= BinCoordAssign$ScaledX[6] & 
                                                         ScaledX <= BinCoordAssign$ScaledX[7] &
                                                         ScaledY >= BinCoordAssign$ScaledY[7] &
                                                         ScaledY <= BinCoordAssign$ScaledY[6], 6,
                                                       # Bin 7
                                                       if_else(ScaledX >= BinCoordAssign$ScaledX[8] & 
                                                                 ScaledX <= BinCoordAssign$ScaledX[7] &
                                                                 ScaledY <= BinCoordAssign$ScaledY[7], 7,
                                                               # Bin 8
                                                               if_else(ScaledX >= BinCoordAssign$ScaledX[9] & 
                                                                         ScaledX <= BinCoordAssign$ScaledX[8] &
                                                                         ScaledY <= BinCoordAssign$ScaledY[9], 8, 0
                                                               )))))))))
  # Binning circle nest coordinates 
  # Filtering out the circle data for the selected colony
  # The code is the same as above, except its only y coordinates since xlims don't matter for the binning to work 
  Colony8BinnedCircle <- data_table %>%
    filter(Colony == "8" & Nest == "Circle") %>%
    mutate(Bin =
             # Bin 1
             if_else(ScaledY <= BinCoordAssign$ScaledY[10], 1,
                     # Bin 2
                     if_else(ScaledY >= BinCoordAssign$ScaledY[10] & 
                               ScaledY <= BinCoordAssign$ScaledY[11], 2,
                             # Bin 3
                             if_else(ScaledY >= BinCoordAssign$ScaledY[11] & 
                                       ScaledY <= BinCoordAssign$ScaledY[12], 3,
                                     # Bin 4
                                     if_else(ScaledY >= BinCoordAssign$ScaledY[12] & 
                                               ScaledY <= BinCoordAssign$ScaledY[13], 4,
                                             # Bin 5
                                             if_else(ScaledY >= BinCoordAssign$ScaledY[13] & 
                                                       ScaledY <= BinCoordAssign$ScaledY[14], 5,
                                                     # Bin 6
                                                     if_else(ScaledY >= BinCoordAssign$ScaledY[14] & 
                                                               ScaledY <= BinCoordAssign$ScaledY[15], 6,
                                                             # Bin 7
                                                             if_else(ScaledY >= BinCoordAssign$ScaledY[15] & 
                                                                       ScaledY <= BinCoordAssign$ScaledY[16], 7,
                                                                     # Bin 8
                                                                     if_else(ScaledY >= BinCoordAssign$ScaledY[16], 8, 0
                                                                     )))))))))
  Colony8Binned <<- full_join(Colony8BinnedTube, Colony8BinnedCircle)
}

# Run the eight nest section binning function for the combined colony member data set
CoordBinned(FullDataCoordAll) 

# COLONY 9
CoordBinned <- function(data_table){
  # Selecting the colony bin references
  BinCoordAssign <- BinCoordFull %>%
    filter(Colony == "9")
  
  # Binning the tube colony member coordinates
  # Filtering out the tube data for the selected colony
  Colony9BinnedTube <- data_table %>%
    filter(Colony == "9" & Nest == "Tube") %>%
    # Ifelse conditional statements for each bin
    mutate(Bin =
             # The bins determine whether the x and y coordinates are within a set of relevant bounds for each bin
             # The order is always x coordinates first, then y
             # Bin 1
             if_else(
               ScaledX >= BinCoordAssign$ScaledX[2] & 
                 ScaledX <= BinCoordAssign$ScaledX[1] &
                 ScaledY <= BinCoordAssign$ScaledY[2], 1,
               # Bin 2
               if_else(ScaledX >= BinCoordAssign$ScaledX[3] & 
                         ScaledX <= BinCoordAssign$ScaledX[2] &
                         ScaledY >= BinCoordAssign$ScaledY[3] &
                         ScaledY <= BinCoordAssign$ScaledY[2], 2,
                       # Bin 3
                       if_else(ScaledX >= BinCoordAssign$ScaledX[4] & 
                                 ScaledX <= BinCoordAssign$ScaledX[3] &
                                 ScaledY >= BinCoordAssign$ScaledY[3] &
                                 ScaledY <= BinCoordAssign$ScaledY[4], 3,
                               # Bin 4
                               if_else(ScaledX >= BinCoordAssign$ScaledX[4] & 
                                         ScaledX <= BinCoordAssign$ScaledX[5] &
                                         ScaledY >= BinCoordAssign$ScaledY[4] &
                                         ScaledY <= BinCoordAssign$ScaledY[5], 4,
                                       # Bin 5
                                       if_else(ScaledX >= BinCoordAssign$ScaledX[5] & 
                                                 ScaledX <= BinCoordAssign$ScaledX[6] &
                                                 ScaledY >= BinCoordAssign$ScaledY[4] &
                                                 ScaledY <= BinCoordAssign$ScaledY[5], 5,
                                               # Bin 6
                                               if_else(ScaledX >= BinCoordAssign$ScaledX[6] & 
                                                         ScaledX <= BinCoordAssign$ScaledX[7] &
                                                         ScaledY >= BinCoordAssign$ScaledY[7] &
                                                         ScaledY <= BinCoordAssign$ScaledY[6], 6,
                                                       # Bin 7
                                                       if_else(ScaledX >= BinCoordAssign$ScaledX[8] & 
                                                                 ScaledX <= BinCoordAssign$ScaledX[7] &
                                                                 ScaledY <= BinCoordAssign$ScaledY[7], 7,
                                                               # Bin 8
                                                               if_else(ScaledX >= BinCoordAssign$ScaledX[9] & 
                                                                         ScaledX <= BinCoordAssign$ScaledX[8] &
                                                                         ScaledY <= BinCoordAssign$ScaledY[9], 8, 0
                                                               )))))))))
  # Binning circle nest coordinates 
  # Filtering out the circle data for the selected colony
  # The code is the same as above, except its only y coordinates since xlims don't matter for the binning to work 
  Colony9BinnedCircle <- data_table %>%
    filter(Colony == "9" & Nest == "Circle") %>%
    mutate(Bin =
             # Bin 1
             if_else(ScaledY <= BinCoordAssign$ScaledY[10], 1,
                     # Bin 2
                     if_else(ScaledY >= BinCoordAssign$ScaledY[10] & 
                               ScaledY <= BinCoordAssign$ScaledY[11], 2,
                             # Bin 3
                             if_else(ScaledY >= BinCoordAssign$ScaledY[11] & 
                                       ScaledY <= BinCoordAssign$ScaledY[12], 3,
                                     # Bin 4
                                     if_else(ScaledY >= BinCoordAssign$ScaledY[12] & 
                                               ScaledY <= BinCoordAssign$ScaledY[13], 4,
                                             # Bin 5
                                             if_else(ScaledY >= BinCoordAssign$ScaledY[13] & 
                                                       ScaledY <= BinCoordAssign$ScaledY[14], 5,
                                                     # Bin 6
                                                     if_else(ScaledY >= BinCoordAssign$ScaledY[14] & 
                                                               ScaledY <= BinCoordAssign$ScaledY[15], 6,
                                                             # Bin 7
                                                             if_else(ScaledY >= BinCoordAssign$ScaledY[15] & 
                                                                       ScaledY <= BinCoordAssign$ScaledY[16], 7,
                                                                     # Bin 8
                                                                     if_else(ScaledY >= BinCoordAssign$ScaledY[16], 8, 0
                                                                     )))))))))
  Colony9Binned <<- full_join(Colony9BinnedTube, Colony9BinnedCircle)
}

# Run the eight nest section binning function for the combined colony member data set
CoordBinned(FullDataCoordAll) 

# COLONY 10
CoordBinned <- function(data_table){
  # Selecting the colony bin references
  BinCoordAssign <- BinCoordFull %>%
    filter(Colony == "10")
  
  # Binning the tube colony member coordinates
  # Filtering out the tube data for the selected colony
  Colony10BinnedTube <- data_table %>%
    filter(Colony == "10" & Nest == "Tube") %>%
    # Ifelse conditional statements for each bin
    mutate(Bin =
             # The bins determine whether the x and y coordinates are within a set of relevant bounds for each bin
             # The order is always x coordinates first, then y
             # Bin 1
             if_else(
               ScaledX >= BinCoordAssign$ScaledX[2] & 
                 ScaledX <= BinCoordAssign$ScaledX[1] &
                 ScaledY <= BinCoordAssign$ScaledY[2], 1,
               # Bin 2
               if_else(ScaledX >= BinCoordAssign$ScaledX[3] & 
                         ScaledX <= BinCoordAssign$ScaledX[2] &
                         ScaledY >= BinCoordAssign$ScaledY[3] &
                         ScaledY <= BinCoordAssign$ScaledY[2], 2,
                       # Bin 3
                       if_else(ScaledX >= BinCoordAssign$ScaledX[4] & 
                                 ScaledX <= BinCoordAssign$ScaledX[3] &
                                 ScaledY >= BinCoordAssign$ScaledY[3] &
                                 ScaledY <= BinCoordAssign$ScaledY[4], 3,
                               # Bin 4
                               if_else(ScaledX >= BinCoordAssign$ScaledX[4] & 
                                         ScaledX <= BinCoordAssign$ScaledX[5] &
                                         ScaledY >= BinCoordAssign$ScaledY[4] &
                                         ScaledY <= BinCoordAssign$ScaledY[5], 4,
                                       # Bin 5
                                       if_else(ScaledX >= BinCoordAssign$ScaledX[5] & 
                                                 ScaledX <= BinCoordAssign$ScaledX[6] &
                                                 ScaledY >= BinCoordAssign$ScaledY[4] &
                                                 ScaledY <= BinCoordAssign$ScaledY[5], 5,
                                               # Bin 6
                                               if_else(ScaledX >= BinCoordAssign$ScaledX[6] & 
                                                         ScaledX <= BinCoordAssign$ScaledX[7] &
                                                         ScaledY >= BinCoordAssign$ScaledY[7] &
                                                         ScaledY <= BinCoordAssign$ScaledY[6], 6,
                                                       # Bin 7
                                                       if_else(ScaledX >= BinCoordAssign$ScaledX[8] & 
                                                                 ScaledX <= BinCoordAssign$ScaledX[7] &
                                                                 ScaledY <= BinCoordAssign$ScaledY[7], 7,
                                                               # Bin 8
                                                               if_else(ScaledX >= BinCoordAssign$ScaledX[9] & 
                                                                         ScaledX <= BinCoordAssign$ScaledX[8] &
                                                                         ScaledY <= BinCoordAssign$ScaledY[9], 8, 0
                                                               )))))))))
  # Binning circle nest coordinates 
  # Filtering out the circle data for the selected colony
  # The code is the same as above, except its only y coordinates since xlims don't matter for the binning to work 
  Colony10BinnedCircle <- data_table %>%
    filter(Colony == "10" & Nest == "Circle") %>%
    mutate(Bin =
             # Bin 1
             if_else(ScaledY <= BinCoordAssign$ScaledY[10], 1,
                     # Bin 2
                     if_else(ScaledY >= BinCoordAssign$ScaledY[10] & 
                               ScaledY <= BinCoordAssign$ScaledY[11], 2,
                             # Bin 3
                             if_else(ScaledY >= BinCoordAssign$ScaledY[11] & 
                                       ScaledY <= BinCoordAssign$ScaledY[12], 3,
                                     # Bin 4
                                     if_else(ScaledY >= BinCoordAssign$ScaledY[12] & 
                                               ScaledY <= BinCoordAssign$ScaledY[13], 4,
                                             # Bin 5
                                             if_else(ScaledY >= BinCoordAssign$ScaledY[13] & 
                                                       ScaledY <= BinCoordAssign$ScaledY[14], 5,
                                                     # Bin 6
                                                     if_else(ScaledY >= BinCoordAssign$ScaledY[14] & 
                                                               ScaledY <= BinCoordAssign$ScaledY[15], 6,
                                                             # Bin 7
                                                             if_else(ScaledY >= BinCoordAssign$ScaledY[15] & 
                                                                       ScaledY <= BinCoordAssign$ScaledY[16], 7,
                                                                     # Bin 8
                                                                     if_else(ScaledY >= BinCoordAssign$ScaledY[16], 8, 0
                                                                     )))))))))
  Colony10Binned <<- full_join(Colony10BinnedTube, Colony10BinnedCircle)
}

# Run the eight nest section binning function for the combined colony member data set
CoordBinned(FullDataCoordAll) 

# COLONY 11
CoordBinned <- function(data_table){
  # Selecting the colony bin references
  BinCoordAssign <- BinCoordFull %>%
    filter(Colony == "11")
  
  # Binning the tube colony member coordinates
  # Filtering out the tube data for the selected colony
  Colony11BinnedTube <- data_table %>%
    filter(Colony == "11" & Nest == "Tube") %>%
    # Ifelse conditional statements for each bin
    mutate(Bin =
             # The bins determine whether the x and y coordinates are within a set of relevant bounds for each bin
             # The order is always x coordinates first, then y
             # Bin 1
             if_else(
               ScaledX >= BinCoordAssign$ScaledX[2] & 
                 ScaledX <= BinCoordAssign$ScaledX[1] &
                 ScaledY <= BinCoordAssign$ScaledY[2], 1,
               # Bin 2
               if_else(ScaledX >= BinCoordAssign$ScaledX[3] & 
                         ScaledX <= BinCoordAssign$ScaledX[2] &
                         ScaledY >= BinCoordAssign$ScaledY[3] &
                         ScaledY <= BinCoordAssign$ScaledY[2], 2,
                       # Bin 3
                       if_else(ScaledX >= BinCoordAssign$ScaledX[4] & 
                                 ScaledX <= BinCoordAssign$ScaledX[3] &
                                 ScaledY >= BinCoordAssign$ScaledY[3] &
                                 ScaledY <= BinCoordAssign$ScaledY[4], 3,
                               # Bin 4
                               if_else(ScaledX >= BinCoordAssign$ScaledX[4] & 
                                         ScaledX <= BinCoordAssign$ScaledX[5] &
                                         ScaledY >= BinCoordAssign$ScaledY[4] &
                                         ScaledY <= BinCoordAssign$ScaledY[5], 4,
                                       # Bin 5
                                       if_else(ScaledX >= BinCoordAssign$ScaledX[5] & 
                                                 ScaledX <= BinCoordAssign$ScaledX[6] &
                                                 ScaledY >= BinCoordAssign$ScaledY[4] &
                                                 ScaledY <= BinCoordAssign$ScaledY[5], 5,
                                               # Bin 6
                                               if_else(ScaledX >= BinCoordAssign$ScaledX[6] & 
                                                         ScaledX <= BinCoordAssign$ScaledX[7] &
                                                         ScaledY >= BinCoordAssign$ScaledY[7] &
                                                         ScaledY <= BinCoordAssign$ScaledY[6], 6,
                                                       # Bin 7
                                                       if_else(ScaledX >= BinCoordAssign$ScaledX[8] & 
                                                                 ScaledX <= BinCoordAssign$ScaledX[7] &
                                                                 ScaledY <= BinCoordAssign$ScaledY[7], 7,
                                                               # Bin 8
                                                               if_else(ScaledX >= BinCoordAssign$ScaledX[9] & 
                                                                         ScaledX <= BinCoordAssign$ScaledX[8] &
                                                                         ScaledY <= BinCoordAssign$ScaledY[9], 8, 0
                                                               )))))))))
  # Binning circle nest coordinates 
  # Filtering out the circle data for the selected colony
  # The code is the same as above, except its only y coordinates since xlims don't matter for the binning to work 
  Colony11BinnedCircle <- data_table %>%
    filter(Colony == "11" & Nest == "Circle") %>%
    mutate(Bin =
             # Bin 1
             if_else(ScaledY <= BinCoordAssign$ScaledY[10], 1,
                     # Bin 2
                     if_else(ScaledY >= BinCoordAssign$ScaledY[10] & 
                               ScaledY <= BinCoordAssign$ScaledY[11], 2,
                             # Bin 3
                             if_else(ScaledY >= BinCoordAssign$ScaledY[11] & 
                                       ScaledY <= BinCoordAssign$ScaledY[12], 3,
                                     # Bin 4
                                     if_else(ScaledY >= BinCoordAssign$ScaledY[12] & 
                                               ScaledY <= BinCoordAssign$ScaledY[13], 4,
                                             # Bin 5
                                             if_else(ScaledY >= BinCoordAssign$ScaledY[13] & 
                                                       ScaledY <= BinCoordAssign$ScaledY[14], 5,
                                                     # Bin 6
                                                     if_else(ScaledY >= BinCoordAssign$ScaledY[14] & 
                                                               ScaledY <= BinCoordAssign$ScaledY[15], 6,
                                                             # Bin 7
                                                             if_else(ScaledY >= BinCoordAssign$ScaledY[15] & 
                                                                       ScaledY <= BinCoordAssign$ScaledY[16], 7,
                                                                     # Bin 8
                                                                     if_else(ScaledY >= BinCoordAssign$ScaledY[16], 8, 0
                                                                     )))))))))
  Colony11Binned <<- full_join(Colony11BinnedTube, Colony11BinnedCircle)
}

# Run the eight nest section binning function for the combined colony member data set
CoordBinned(FullDataCoordAll) 

# COLONY 12
CoordBinned <- function(data_table){
  # Selecting the colony bin references
  BinCoordAssign <- BinCoordFull %>%
    filter(Colony == "12")
  
  # Binning the tube colony member coordinates
  # Filtering out the tube data for the selected colony
  Colony12BinnedTube <- data_table %>%
    filter(Colony == "12" & Nest == "Tube") %>%
    # Ifelse conditional statements for each bin
    mutate(Bin =
             # The bins determine whether the x and y coordinates are within a set of relevant bounds for each bin
             # The order is always x coordinates first, then y
             # Bin 1
             if_else(
               ScaledX >= BinCoordAssign$ScaledX[2] & 
                 ScaledX <= BinCoordAssign$ScaledX[1] &
                 ScaledY <= BinCoordAssign$ScaledY[2], 1,
               # Bin 2
               if_else(ScaledX >= BinCoordAssign$ScaledX[3] & 
                         ScaledX <= BinCoordAssign$ScaledX[2] &
                         ScaledY >= BinCoordAssign$ScaledY[3] &
                         ScaledY <= BinCoordAssign$ScaledY[2], 2,
                       # Bin 3
                       if_else(ScaledX >= BinCoordAssign$ScaledX[4] & 
                                 ScaledX <= BinCoordAssign$ScaledX[3] &
                                 ScaledY >= BinCoordAssign$ScaledY[3] &
                                 ScaledY <= BinCoordAssign$ScaledY[4], 3,
                               # Bin 4
                               if_else(ScaledX >= BinCoordAssign$ScaledX[4] & 
                                         ScaledX <= BinCoordAssign$ScaledX[5] &
                                         ScaledY >= BinCoordAssign$ScaledY[4] &
                                         ScaledY <= BinCoordAssign$ScaledY[5], 4,
                                       # Bin 5
                                       if_else(ScaledX >= BinCoordAssign$ScaledX[5] & 
                                                 ScaledX <= BinCoordAssign$ScaledX[6] &
                                                 ScaledY >= BinCoordAssign$ScaledY[4] &
                                                 ScaledY <= BinCoordAssign$ScaledY[5], 5,
                                               # Bin 6
                                               if_else(ScaledX >= BinCoordAssign$ScaledX[6] & 
                                                         ScaledX <= BinCoordAssign$ScaledX[7] &
                                                         ScaledY >= BinCoordAssign$ScaledY[7] &
                                                         ScaledY <= BinCoordAssign$ScaledY[6], 6,
                                                       # Bin 7
                                                       if_else(ScaledX >= BinCoordAssign$ScaledX[8] & 
                                                                 ScaledX <= BinCoordAssign$ScaledX[7] &
                                                                 ScaledY <= BinCoordAssign$ScaledY[7], 7,
                                                               # Bin 8
                                                               if_else(ScaledX >= BinCoordAssign$ScaledX[9] & 
                                                                         ScaledX <= BinCoordAssign$ScaledX[8] &
                                                                         ScaledY <= BinCoordAssign$ScaledY[9], 8, 0
                                                               )))))))))
  # Binning circle nest coordinates 
  # Filtering out the circle data for the selected colony
  # The code is the same as above, except its only y coordinates since xlims don't matter for the binning to work 
  Colony12BinnedCircle <- data_table %>%
    filter(Colony == "12" & Nest == "Circle") %>%
    mutate(Bin =
             # Bin 1
             if_else(ScaledY <= BinCoordAssign$ScaledY[10], 1,
                     # Bin 2
                     if_else(ScaledY >= BinCoordAssign$ScaledY[10] & 
                               ScaledY <= BinCoordAssign$ScaledY[11], 2,
                             # Bin 3
                             if_else(ScaledY >= BinCoordAssign$ScaledY[11] & 
                                       ScaledY <= BinCoordAssign$ScaledY[12], 3,
                                     # Bin 4
                                     if_else(ScaledY >= BinCoordAssign$ScaledY[12] & 
                                               ScaledY <= BinCoordAssign$ScaledY[13], 4,
                                             # Bin 5
                                             if_else(ScaledY >= BinCoordAssign$ScaledY[13] & 
                                                       ScaledY <= BinCoordAssign$ScaledY[14], 5,
                                                     # Bin 6
                                                     if_else(ScaledY >= BinCoordAssign$ScaledY[14] & 
                                                               ScaledY <= BinCoordAssign$ScaledY[15], 6,
                                                             # Bin 7
                                                             if_else(ScaledY >= BinCoordAssign$ScaledY[15] & 
                                                                       ScaledY <= BinCoordAssign$ScaledY[16], 7,
                                                                     # Bin 8
                                                                     if_else(ScaledY >= BinCoordAssign$ScaledY[16], 8, 0
                                                                     )))))))))
  Colony12Binned <<- full_join(Colony12BinnedTube, Colony12BinnedCircle)
}

# Run the eight nest section binning function for the combined colony member data set
CoordBinned(FullDataCoordAll) 

# COLONY 13
CoordBinned <- function(data_table){
  # Selecting the colony bin references
  BinCoordAssign <- BinCoordFull %>%
    filter(Colony == "13")
  
  # Binning the tube colony member coordinates
  # Filtering out the tube data for the selected colony
  Colony13BinnedTube <- data_table %>%
    filter(Colony == "13" & Nest == "Tube") %>%
    # Ifelse conditional statements for each bin
    mutate(Bin =
             # The bins determine whether the x and y coordinates are within a set of relevant bounds for each bin
             # The order is always x coordinates first, then y
             # Bin 1
             if_else(
               ScaledX >= BinCoordAssign$ScaledX[2] & 
                 ScaledX <= BinCoordAssign$ScaledX[1] &
                 ScaledY <= BinCoordAssign$ScaledY[2], 1,
               # Bin 2
               if_else(ScaledX >= BinCoordAssign$ScaledX[3] & 
                         ScaledX <= BinCoordAssign$ScaledX[2] &
                         ScaledY >= BinCoordAssign$ScaledY[3] &
                         ScaledY <= BinCoordAssign$ScaledY[2], 2,
                       # Bin 3
                       if_else(ScaledX >= BinCoordAssign$ScaledX[4] & 
                                 ScaledX <= BinCoordAssign$ScaledX[3] &
                                 ScaledY >= BinCoordAssign$ScaledY[3] &
                                 ScaledY <= BinCoordAssign$ScaledY[4], 3,
                               # Bin 4
                               if_else(ScaledX >= BinCoordAssign$ScaledX[4] & 
                                         ScaledX <= BinCoordAssign$ScaledX[5] &
                                         ScaledY >= BinCoordAssign$ScaledY[4] &
                                         ScaledY <= BinCoordAssign$ScaledY[5], 4,
                                       # Bin 5
                                       if_else(ScaledX >= BinCoordAssign$ScaledX[5] & 
                                                 ScaledX <= BinCoordAssign$ScaledX[6] &
                                                 ScaledY >= BinCoordAssign$ScaledY[4] &
                                                 ScaledY <= BinCoordAssign$ScaledY[5], 5,
                                               # Bin 6
                                               if_else(ScaledX >= BinCoordAssign$ScaledX[6] & 
                                                         ScaledX <= BinCoordAssign$ScaledX[7] &
                                                         ScaledY >= BinCoordAssign$ScaledY[7] &
                                                         ScaledY <= BinCoordAssign$ScaledY[6], 6,
                                                       # Bin 7
                                                       if_else(ScaledX >= BinCoordAssign$ScaledX[8] & 
                                                                 ScaledX <= BinCoordAssign$ScaledX[7] &
                                                                 ScaledY <= BinCoordAssign$ScaledY[7], 7,
                                                               # Bin 8
                                                               if_else(ScaledX >= BinCoordAssign$ScaledX[9] & 
                                                                         ScaledX <= BinCoordAssign$ScaledX[8] &
                                                                         ScaledY <= BinCoordAssign$ScaledY[9], 8, 0
                                                               )))))))))
  # Binning circle nest coordinates 
  # Filtering out the circle data for the selected colony
  # The code is the same as above, except its only y coordinates since xlims don't matter for the binning to work 
  Colony13BinnedCircle <- data_table %>%
    filter(Colony == "13" & Nest == "Circle") %>%
    mutate(Bin =
             # Bin 1
             if_else(ScaledY <= BinCoordAssign$ScaledY[10], 1,
                     # Bin 2
                     if_else(ScaledY >= BinCoordAssign$ScaledY[10] & 
                               ScaledY <= BinCoordAssign$ScaledY[11], 2,
                             # Bin 3
                             if_else(ScaledY >= BinCoordAssign$ScaledY[11] & 
                                       ScaledY <= BinCoordAssign$ScaledY[12], 3,
                                     # Bin 4
                                     if_else(ScaledY >= BinCoordAssign$ScaledY[12] & 
                                               ScaledY <= BinCoordAssign$ScaledY[13], 4,
                                             # Bin 5
                                             if_else(ScaledY >= BinCoordAssign$ScaledY[13] & 
                                                       ScaledY <= BinCoordAssign$ScaledY[14], 5,
                                                     # Bin 6
                                                     if_else(ScaledY >= BinCoordAssign$ScaledY[14] & 
                                                               ScaledY <= BinCoordAssign$ScaledY[15], 6,
                                                             # Bin 7
                                                             if_else(ScaledY >= BinCoordAssign$ScaledY[15] & 
                                                                       ScaledY <= BinCoordAssign$ScaledY[16], 7,
                                                                     # Bin 8
                                                                     if_else(ScaledY >= BinCoordAssign$ScaledY[16], 8, 0
                                                                     )))))))))
  Colony13Binned <<- full_join(Colony13BinnedTube, Colony13BinnedCircle)
}

# Run the eight nest section binning function for the combined colony member data set
CoordBinned(FullDataCoordAll) 

# COLONY 14
CoordBinned <- function(data_table){
  # Selecting the colony bin references
  BinCoordAssign <- BinCoordFull %>%
    filter(Colony == "14")
  
  # Binning the tube colony member coordinates
  # Filtering out the tube data for the selected colony
  Colony14BinnedTube <- data_table %>%
    filter(Colony == "14" & Nest == "Tube") %>%
    # Ifelse conditional statements for each bin
    mutate(Bin =
             # The bins determine whether the x and y coordinates are within a set of relevant bounds for each bin
             # The order is always x coordinates first, then y
             # Bin 1
             if_else(
               ScaledX >= BinCoordAssign$ScaledX[2] & 
                 ScaledX <= BinCoordAssign$ScaledX[1] &
                 ScaledY <= BinCoordAssign$ScaledY[2], 1,
               # Bin 2
               if_else(ScaledX >= BinCoordAssign$ScaledX[3] & 
                         ScaledX <= BinCoordAssign$ScaledX[2] &
                         ScaledY >= BinCoordAssign$ScaledY[3] &
                         ScaledY <= BinCoordAssign$ScaledY[2], 2,
                       # Bin 3
                       if_else(ScaledX >= BinCoordAssign$ScaledX[4] & 
                                 ScaledX <= BinCoordAssign$ScaledX[3] &
                                 ScaledY >= BinCoordAssign$ScaledY[3] &
                                 ScaledY <= BinCoordAssign$ScaledY[4], 3,
                               # Bin 4
                               if_else(ScaledX >= BinCoordAssign$ScaledX[4] & 
                                         ScaledX <= BinCoordAssign$ScaledX[5] &
                                         ScaledY >= BinCoordAssign$ScaledY[4] &
                                         ScaledY <= BinCoordAssign$ScaledY[5], 4,
                                       # Bin 5
                                       if_else(ScaledX >= BinCoordAssign$ScaledX[5] & 
                                                 ScaledX <= BinCoordAssign$ScaledX[6] &
                                                 ScaledY >= BinCoordAssign$ScaledY[4] &
                                                 ScaledY <= BinCoordAssign$ScaledY[5], 5,
                                               # Bin 6
                                               if_else(ScaledX >= BinCoordAssign$ScaledX[6] & 
                                                         ScaledX <= BinCoordAssign$ScaledX[7] &
                                                         ScaledY >= BinCoordAssign$ScaledY[7] &
                                                         ScaledY <= BinCoordAssign$ScaledY[6], 6,
                                                       # Bin 7
                                                       if_else(ScaledX >= BinCoordAssign$ScaledX[8] & 
                                                                 ScaledX <= BinCoordAssign$ScaledX[7] &
                                                                 ScaledY <= BinCoordAssign$ScaledY[7], 7,
                                                               # Bin 8
                                                               if_else(ScaledX >= BinCoordAssign$ScaledX[9] & 
                                                                         ScaledX <= BinCoordAssign$ScaledX[8] &
                                                                         ScaledY <= BinCoordAssign$ScaledY[9], 8, 0
                                                               )))))))))
  # Binning circle nest coordinates 
  # Filtering out the circle data for the selected colony
  # The code is the same as above, except its only y coordinates since xlims don't matter for the binning to work 
  Colony14BinnedCircle <- data_table %>%
    filter(Colony == "14" & Nest == "Circle") %>%
    mutate(Bin =
             # Bin 1
             if_else(ScaledY <= BinCoordAssign$ScaledY[10], 1,
                     # Bin 2
                     if_else(ScaledY >= BinCoordAssign$ScaledY[10] & 
                               ScaledY <= BinCoordAssign$ScaledY[11], 2,
                             # Bin 3
                             if_else(ScaledY >= BinCoordAssign$ScaledY[11] & 
                                       ScaledY <= BinCoordAssign$ScaledY[12], 3,
                                     # Bin 4
                                     if_else(ScaledY >= BinCoordAssign$ScaledY[12] & 
                                               ScaledY <= BinCoordAssign$ScaledY[13], 4,
                                             # Bin 5
                                             if_else(ScaledY >= BinCoordAssign$ScaledY[13] & 
                                                       ScaledY <= BinCoordAssign$ScaledY[14], 5,
                                                     # Bin 6
                                                     if_else(ScaledY >= BinCoordAssign$ScaledY[14] & 
                                                               ScaledY <= BinCoordAssign$ScaledY[15], 6,
                                                             # Bin 7
                                                             if_else(ScaledY >= BinCoordAssign$ScaledY[15] & 
                                                                       ScaledY <= BinCoordAssign$ScaledY[16], 7,
                                                                     # Bin 8
                                                                     if_else(ScaledY >= BinCoordAssign$ScaledY[16], 8, 0
                                                                     )))))))))
  Colony14Binned <<- full_join(Colony14BinnedTube, Colony14BinnedCircle)
}

# Run the eight nest section binning function for the combined colony member data set
CoordBinned(FullDataCoordAll) 

# COLONY 15
CoordBinned <- function(data_table){
  # Selecting the colony bin references
  BinCoordAssign <- BinCoordFull %>%
    filter(Colony == "15")
  
  # Binning the tube colony member coordinates
  # Filtering out the tube data for the selected colony
  Colony15BinnedTube <- data_table %>%
    filter(Colony == "15" & Nest == "Tube") %>%
    # Ifelse conditional statements for each bin
    mutate(Bin =
             # The bins determine whether the x and y coordinates are within a set of relevant bounds for each bin
             # The order is always x coordinates first, then y
             # Bin 1
             if_else(
               ScaledX >= BinCoordAssign$ScaledX[2] & 
                 ScaledX <= BinCoordAssign$ScaledX[1] &
                 ScaledY <= BinCoordAssign$ScaledY[2], 1,
               # Bin 2
               if_else(ScaledX >= BinCoordAssign$ScaledX[3] & 
                         ScaledX <= BinCoordAssign$ScaledX[2] &
                         ScaledY >= BinCoordAssign$ScaledY[3] &
                         ScaledY <= BinCoordAssign$ScaledY[2], 2,
                       # Bin 3
                       if_else(ScaledX >= BinCoordAssign$ScaledX[4] & 
                                 ScaledX <= BinCoordAssign$ScaledX[3] &
                                 ScaledY >= BinCoordAssign$ScaledY[3] &
                                 ScaledY <= BinCoordAssign$ScaledY[4], 3,
                               # Bin 4
                               if_else(ScaledX >= BinCoordAssign$ScaledX[4] & 
                                         ScaledX <= BinCoordAssign$ScaledX[5] &
                                         ScaledY >= BinCoordAssign$ScaledY[4] &
                                         ScaledY <= BinCoordAssign$ScaledY[5], 4,
                                       # Bin 5
                                       if_else(ScaledX >= BinCoordAssign$ScaledX[5] & 
                                                 ScaledX <= BinCoordAssign$ScaledX[6] &
                                                 ScaledY >= BinCoordAssign$ScaledY[4] &
                                                 ScaledY <= BinCoordAssign$ScaledY[5], 5,
                                               # Bin 6
                                               if_else(ScaledX >= BinCoordAssign$ScaledX[6] & 
                                                         ScaledX <= BinCoordAssign$ScaledX[7] &
                                                         ScaledY >= BinCoordAssign$ScaledY[7] &
                                                         ScaledY <= BinCoordAssign$ScaledY[6], 6,
                                                       # Bin 7
                                                       if_else(ScaledX >= BinCoordAssign$ScaledX[8] & 
                                                                 ScaledX <= BinCoordAssign$ScaledX[7] &
                                                                 ScaledY <= BinCoordAssign$ScaledY[7], 7,
                                                               # Bin 8
                                                               if_else(ScaledX >= BinCoordAssign$ScaledX[9] & 
                                                                         ScaledX <= BinCoordAssign$ScaledX[8] &
                                                                         ScaledY <= BinCoordAssign$ScaledY[9], 8, 0
                                                               )))))))))
  # Binning circle nest coordinates 
  # Filtering out the circle data for the selected colony
  # The code is the same as above, except its only y coordinates since xlims don't matter for the binning to work 
  Colony15BinnedCircle <- data_table %>%
    filter(Colony == "15" & Nest == "Circle") %>%
    mutate(Bin =
             # Bin 1
             if_else(ScaledY <= BinCoordAssign$ScaledY[10], 1,
                     # Bin 2
                     if_else(ScaledY >= BinCoordAssign$ScaledY[10] & 
                               ScaledY <= BinCoordAssign$ScaledY[11], 2,
                             # Bin 3
                             if_else(ScaledY >= BinCoordAssign$ScaledY[11] & 
                                       ScaledY <= BinCoordAssign$ScaledY[12], 3,
                                     # Bin 4
                                     if_else(ScaledY >= BinCoordAssign$ScaledY[12] & 
                                               ScaledY <= BinCoordAssign$ScaledY[13], 4,
                                             # Bin 5
                                             if_else(ScaledY >= BinCoordAssign$ScaledY[13] & 
                                                       ScaledY <= BinCoordAssign$ScaledY[14], 5,
                                                     # Bin 6
                                                     if_else(ScaledY >= BinCoordAssign$ScaledY[14] & 
                                                               ScaledY <= BinCoordAssign$ScaledY[15], 6,
                                                             # Bin 7
                                                             if_else(ScaledY >= BinCoordAssign$ScaledY[15] & 
                                                                       ScaledY <= BinCoordAssign$ScaledY[16], 7,
                                                                     # Bin 8
                                                                     if_else(ScaledY >= BinCoordAssign$ScaledY[16], 8, 0
                                                                     )))))))))
  Colony15Binned <<- full_join(Colony15BinnedTube, Colony15BinnedCircle)
}

# Run the eight nest section binning function for the combined colony member data set
CoordBinned(FullDataCoordAll) 

# COLONY 16
CoordBinned <- function(data_table){
  # Selecting the colony bin references
  BinCoordAssign <- BinCoordFull %>%
    filter(Colony == "16")
  
  # Binning the tube colony member coordinates
  # Filtering out the tube data for the selected colony
  Colony16BinnedTube <- data_table %>%
    filter(Colony == "16" & Nest == "Tube") %>%
    # Ifelse conditional statements for each bin
    mutate(Bin =
             # The bins determine whether the x and y coordinates are within a set of relevant bounds for each bin
             # The order is always x coordinates first, then y
             # Bin 1
             if_else(
               ScaledX >= BinCoordAssign$ScaledX[2] & 
                 ScaledX <= BinCoordAssign$ScaledX[1] &
                 ScaledY <= BinCoordAssign$ScaledY[2], 1,
               # Bin 2
               if_else(ScaledX >= BinCoordAssign$ScaledX[3] & 
                         ScaledX <= BinCoordAssign$ScaledX[2] &
                         ScaledY >= BinCoordAssign$ScaledY[3] &
                         ScaledY <= BinCoordAssign$ScaledY[2], 2,
                       # Bin 3
                       if_else(ScaledX >= BinCoordAssign$ScaledX[4] & 
                                 ScaledX <= BinCoordAssign$ScaledX[3] &
                                 ScaledY >= BinCoordAssign$ScaledY[3] &
                                 ScaledY <= BinCoordAssign$ScaledY[4], 3,
                               # Bin 4
                               if_else(ScaledX >= BinCoordAssign$ScaledX[4] & 
                                         ScaledX <= BinCoordAssign$ScaledX[5] &
                                         ScaledY >= BinCoordAssign$ScaledY[4] &
                                         ScaledY <= BinCoordAssign$ScaledY[5], 4,
                                       # Bin 5
                                       if_else(ScaledX >= BinCoordAssign$ScaledX[5] & 
                                                 ScaledX <= BinCoordAssign$ScaledX[6] &
                                                 ScaledY >= BinCoordAssign$ScaledY[4] &
                                                 ScaledY <= BinCoordAssign$ScaledY[5], 5,
                                               # Bin 6
                                               if_else(ScaledX >= BinCoordAssign$ScaledX[6] & 
                                                         ScaledX <= BinCoordAssign$ScaledX[7] &
                                                         ScaledY >= BinCoordAssign$ScaledY[7] &
                                                         ScaledY <= BinCoordAssign$ScaledY[6], 6,
                                                       # Bin 7
                                                       if_else(ScaledX >= BinCoordAssign$ScaledX[8] & 
                                                                 ScaledX <= BinCoordAssign$ScaledX[7] &
                                                                 ScaledY <= BinCoordAssign$ScaledY[7], 7,
                                                               # Bin 8
                                                               if_else(ScaledX >= BinCoordAssign$ScaledX[9] & 
                                                                         ScaledX <= BinCoordAssign$ScaledX[8] &
                                                                         ScaledY <= BinCoordAssign$ScaledY[9], 8, 0
                                                               )))))))))
  # Binning circle nest coordinates 
  # Filtering out the circle data for the selected colony
  # The code is the same as above, except its only y coordinates since xlims don't matter for the binning to work 
  Colony16BinnedCircle <- data_table %>%
    filter(Colony == "16" & Nest == "Circle") %>%
    mutate(Bin =
             # Bin 1
             if_else(ScaledY <= BinCoordAssign$ScaledY[10], 1,
                     # Bin 2
                     if_else(ScaledY >= BinCoordAssign$ScaledY[10] & 
                               ScaledY <= BinCoordAssign$ScaledY[11], 2,
                             # Bin 3
                             if_else(ScaledY >= BinCoordAssign$ScaledY[11] & 
                                       ScaledY <= BinCoordAssign$ScaledY[12], 3,
                                     # Bin 4
                                     if_else(ScaledY >= BinCoordAssign$ScaledY[12] & 
                                               ScaledY <= BinCoordAssign$ScaledY[13], 4,
                                             # Bin 5
                                             if_else(ScaledY >= BinCoordAssign$ScaledY[13] & 
                                                       ScaledY <= BinCoordAssign$ScaledY[14], 5,
                                                     # Bin 6
                                                     if_else(ScaledY >= BinCoordAssign$ScaledY[14] & 
                                                               ScaledY <= BinCoordAssign$ScaledY[15], 6,
                                                             # Bin 7
                                                             if_else(ScaledY >= BinCoordAssign$ScaledY[15] & 
                                                                       ScaledY <= BinCoordAssign$ScaledY[16], 7,
                                                                     # Bin 8
                                                                     if_else(ScaledY >= BinCoordAssign$ScaledY[16], 8, 0
                                                                     )))))))))
  Colony16Binned <<- full_join(Colony16BinnedTube, Colony16BinnedCircle)
}

# Run the eight nest section binning function for the combined colony member data set
CoordBinned(FullDataCoordAll) 

# COLONY 17
CoordBinned <- function(data_table){
  # Selecting the colony bin references
  BinCoordAssign <- BinCoordFull %>%
    filter(Colony == "17")
  
  # Binning the tube colony member coordinates
  # Filtering out the tube data for the selected colony
  Colony17BinnedTube <- data_table %>%
    filter(Colony == "17" & Nest == "Tube") %>%
    # Ifelse conditional statements for each bin
    mutate(Bin =
             # The bins determine whether the x and y coordinates are within a set of relevant bounds for each bin
             # The order is always x coordinates first, then y
             # Bin 1
             if_else(
               ScaledX >= BinCoordAssign$ScaledX[2] & 
                 ScaledX <= BinCoordAssign$ScaledX[1] &
                 ScaledY <= BinCoordAssign$ScaledY[2], 1,
               # Bin 2
               if_else(ScaledX >= BinCoordAssign$ScaledX[3] & 
                         ScaledX <= BinCoordAssign$ScaledX[2] &
                         ScaledY >= BinCoordAssign$ScaledY[3] &
                         ScaledY <= BinCoordAssign$ScaledY[2], 2,
                       # Bin 3
                       if_else(ScaledX >= BinCoordAssign$ScaledX[4] & 
                                 ScaledX <= BinCoordAssign$ScaledX[3] &
                                 ScaledY >= BinCoordAssign$ScaledY[3] &
                                 ScaledY <= BinCoordAssign$ScaledY[4], 3,
                               # Bin 4
                               if_else(ScaledX >= BinCoordAssign$ScaledX[4] & 
                                         ScaledX <= BinCoordAssign$ScaledX[5] &
                                         ScaledY >= BinCoordAssign$ScaledY[4] &
                                         ScaledY <= BinCoordAssign$ScaledY[5], 4,
                                       # Bin 5
                                       if_else(ScaledX >= BinCoordAssign$ScaledX[5] & 
                                                 ScaledX <= BinCoordAssign$ScaledX[6] &
                                                 ScaledY >= BinCoordAssign$ScaledY[4] &
                                                 ScaledY <= BinCoordAssign$ScaledY[5], 5,
                                               # Bin 6
                                               if_else(ScaledX >= BinCoordAssign$ScaledX[6] & 
                                                         ScaledX <= BinCoordAssign$ScaledX[7] &
                                                         ScaledY >= BinCoordAssign$ScaledY[7] &
                                                         ScaledY <= BinCoordAssign$ScaledY[6], 6,
                                                       # Bin 7
                                                       if_else(ScaledX >= BinCoordAssign$ScaledX[8] & 
                                                                 ScaledX <= BinCoordAssign$ScaledX[7] &
                                                                 ScaledY <= BinCoordAssign$ScaledY[7], 7,
                                                               # Bin 8
                                                               if_else(ScaledX >= BinCoordAssign$ScaledX[9] & 
                                                                         ScaledX <= BinCoordAssign$ScaledX[8] &
                                                                         ScaledY <= BinCoordAssign$ScaledY[9], 8, 0
                                                               )))))))))
  # Binning circle nest coordinates 
  # Filtering out the circle data for the selected colony
  # The code is the same as above, except its only y coordinates since xlims don't matter for the binning to work 
  Colony17BinnedCircle <- data_table %>%
    filter(Colony == "17" & Nest == "Circle") %>%
    mutate(Bin =
             # Bin 1
             if_else(ScaledY <= BinCoordAssign$ScaledY[10], 1,
                     # Bin 2
                     if_else(ScaledY >= BinCoordAssign$ScaledY[10] & 
                               ScaledY <= BinCoordAssign$ScaledY[11], 2,
                             # Bin 3
                             if_else(ScaledY >= BinCoordAssign$ScaledY[11] & 
                                       ScaledY <= BinCoordAssign$ScaledY[12], 3,
                                     # Bin 4
                                     if_else(ScaledY >= BinCoordAssign$ScaledY[12] & 
                                               ScaledY <= BinCoordAssign$ScaledY[13], 4,
                                             # Bin 5
                                             if_else(ScaledY >= BinCoordAssign$ScaledY[13] & 
                                                       ScaledY <= BinCoordAssign$ScaledY[14], 5,
                                                     # Bin 6
                                                     if_else(ScaledY >= BinCoordAssign$ScaledY[14] & 
                                                               ScaledY <= BinCoordAssign$ScaledY[15], 6,
                                                             # Bin 7
                                                             if_else(ScaledY >= BinCoordAssign$ScaledY[15] & 
                                                                       ScaledY <= BinCoordAssign$ScaledY[16], 7,
                                                                     # Bin 8
                                                                     if_else(ScaledY >= BinCoordAssign$ScaledY[16], 8, 0
                                                                     )))))))))
  Colony17Binned <<- full_join(Colony17BinnedTube, Colony17BinnedCircle)
}

# Run the eight nest section binning function for the combined colony member data set
CoordBinned(FullDataCoordAll) 

# COLONY 18
CoordBinned <- function(data_table){
  # Selecting the colony bin references
  BinCoordAssign <- BinCoordFull %>%
    filter(Colony == "18")
  
  # Binning the tube colony member coordinates
  # Filtering out the tube data for the selected colony
  Colony18BinnedTube <- data_table %>%
    filter(Colony == "18" & Nest == "Tube") %>%
    # Ifelse conditional statements for each bin
    mutate(Bin =
             # The bins determine whether the x and y coordinates are within a set of relevant bounds for each bin
             # The order is always x coordinates first, then y
             # Bin 1
             if_else(
               ScaledX >= BinCoordAssign$ScaledX[2] & 
                 ScaledX <= BinCoordAssign$ScaledX[1] &
                 ScaledY <= BinCoordAssign$ScaledY[2], 1,
               # Bin 2
               if_else(ScaledX >= BinCoordAssign$ScaledX[3] & 
                         ScaledX <= BinCoordAssign$ScaledX[2] &
                         ScaledY >= BinCoordAssign$ScaledY[3] &
                         ScaledY <= BinCoordAssign$ScaledY[2], 2,
                       # Bin 3
                       if_else(ScaledX >= BinCoordAssign$ScaledX[4] & 
                                 ScaledX <= BinCoordAssign$ScaledX[3] &
                                 ScaledY >= BinCoordAssign$ScaledY[3] &
                                 ScaledY <= BinCoordAssign$ScaledY[4], 3,
                               # Bin 4
                               if_else(ScaledX >= BinCoordAssign$ScaledX[4] & 
                                         ScaledX <= BinCoordAssign$ScaledX[5] &
                                         ScaledY >= BinCoordAssign$ScaledY[4] &
                                         ScaledY <= BinCoordAssign$ScaledY[5], 4,
                                       # Bin 5
                                       if_else(ScaledX >= BinCoordAssign$ScaledX[5] & 
                                                 ScaledX <= BinCoordAssign$ScaledX[6] &
                                                 ScaledY >= BinCoordAssign$ScaledY[4] &
                                                 ScaledY <= BinCoordAssign$ScaledY[5], 5,
                                               # Bin 6
                                               if_else(ScaledX >= BinCoordAssign$ScaledX[6] & 
                                                         ScaledX <= BinCoordAssign$ScaledX[7] &
                                                         ScaledY >= BinCoordAssign$ScaledY[7] &
                                                         ScaledY <= BinCoordAssign$ScaledY[6], 6,
                                                       # Bin 7
                                                       if_else(ScaledX >= BinCoordAssign$ScaledX[8] & 
                                                                 ScaledX <= BinCoordAssign$ScaledX[7] &
                                                                 ScaledY <= BinCoordAssign$ScaledY[7], 7,
                                                               # Bin 8
                                                               if_else(ScaledX >= BinCoordAssign$ScaledX[9] & 
                                                                         ScaledX <= BinCoordAssign$ScaledX[8] &
                                                                         ScaledY <= BinCoordAssign$ScaledY[9], 8, 0
                                                               )))))))))
  # Binning circle nest coordinates 
  # Filtering out the circle data for the selected colony
  # The code is the same as above, except its only y coordinates since xlims don't matter for the binning to work 
  Colony18BinnedCircle <- data_table %>%
    filter(Colony == "18" & Nest == "Circle") %>%
    mutate(Bin =
             # Bin 1
             if_else(ScaledY <= BinCoordAssign$ScaledY[10], 1,
                     # Bin 2
                     if_else(ScaledY >= BinCoordAssign$ScaledY[10] & 
                               ScaledY <= BinCoordAssign$ScaledY[11], 2,
                             # Bin 3
                             if_else(ScaledY >= BinCoordAssign$ScaledY[11] & 
                                       ScaledY <= BinCoordAssign$ScaledY[12], 3,
                                     # Bin 4
                                     if_else(ScaledY >= BinCoordAssign$ScaledY[12] & 
                                               ScaledY <= BinCoordAssign$ScaledY[13], 4,
                                             # Bin 5
                                             if_else(ScaledY >= BinCoordAssign$ScaledY[13] & 
                                                       ScaledY <= BinCoordAssign$ScaledY[14], 5,
                                                     # Bin 6
                                                     if_else(ScaledY >= BinCoordAssign$ScaledY[14] & 
                                                               ScaledY <= BinCoordAssign$ScaledY[15], 6,
                                                             # Bin 7
                                                             if_else(ScaledY >= BinCoordAssign$ScaledY[15] & 
                                                                       ScaledY <= BinCoordAssign$ScaledY[16], 7,
                                                                     # Bin 8
                                                                     if_else(ScaledY >= BinCoordAssign$ScaledY[16], 8, 0
                                                                     )))))))))
  Colony18Binned <<- full_join(Colony18BinnedTube, Colony18BinnedCircle)
}

# Run the eight nest section binning function for the combined colony member data set
CoordBinned(FullDataCoordAll)

# COLONY 19
CoordBinned <- function(data_table){
  # Selecting the colony bin references
  BinCoordAssign <- BinCoordFull %>%
    filter(Colony == "19")
  
  # Binning the tube colony member coordinates
  # Filtering out the tube data for the selected colony
  Colony19BinnedTube <- data_table %>%
    filter(Colony == "19" & Nest == "Tube") %>%
    # Ifelse conditional statements for each bin
    mutate(Bin =
             # The bins determine whether the x and y coordinates are within a set of relevant bounds for each bin
             # The order is always x coordinates first, then y
             # Bin 1
             if_else(
               ScaledX >= BinCoordAssign$ScaledX[2] & 
                 ScaledX <= BinCoordAssign$ScaledX[1] &
                 ScaledY <= BinCoordAssign$ScaledY[2], 1,
               # Bin 2
               if_else(ScaledX >= BinCoordAssign$ScaledX[3] & 
                         ScaledX <= BinCoordAssign$ScaledX[2] &
                         ScaledY >= BinCoordAssign$ScaledY[3] &
                         ScaledY <= BinCoordAssign$ScaledY[2], 2,
                       # Bin 3
                       if_else(ScaledX >= BinCoordAssign$ScaledX[4] & 
                                 ScaledX <= BinCoordAssign$ScaledX[3] &
                                 ScaledY >= BinCoordAssign$ScaledY[3] &
                                 ScaledY <= BinCoordAssign$ScaledY[4], 3,
                               # Bin 4
                               if_else(ScaledX >= BinCoordAssign$ScaledX[4] & 
                                         ScaledX <= BinCoordAssign$ScaledX[5] &
                                         ScaledY >= BinCoordAssign$ScaledY[4] &
                                         ScaledY <= BinCoordAssign$ScaledY[5], 4,
                                       # Bin 5
                                       if_else(ScaledX >= BinCoordAssign$ScaledX[5] & 
                                                 ScaledX <= BinCoordAssign$ScaledX[6] &
                                                 ScaledY >= BinCoordAssign$ScaledY[4] &
                                                 ScaledY <= BinCoordAssign$ScaledY[5], 5,
                                               # Bin 6
                                               if_else(ScaledX >= BinCoordAssign$ScaledX[6] & 
                                                         ScaledX <= BinCoordAssign$ScaledX[7] &
                                                         ScaledY >= BinCoordAssign$ScaledY[7] &
                                                         ScaledY <= BinCoordAssign$ScaledY[6], 6,
                                                       # Bin 7
                                                       if_else(ScaledX >= BinCoordAssign$ScaledX[8] & 
                                                                 ScaledX <= BinCoordAssign$ScaledX[7] &
                                                                 ScaledY <= BinCoordAssign$ScaledY[7], 7,
                                                               # Bin 8
                                                               if_else(ScaledX >= BinCoordAssign$ScaledX[9] & 
                                                                         ScaledX <= BinCoordAssign$ScaledX[8] &
                                                                         ScaledY <= BinCoordAssign$ScaledY[9], 8, 0
                                                               )))))))))
  # Binning circle nest coordinates 
  # Filtering out the circle data for the selected colony
  # The code is the same as above, except its only y coordinates since xlims don't matter for the binning to work 
  Colony19BinnedCircle <- data_table %>%
    filter(Colony == "19" & Nest == "Circle") %>%
    mutate(Bin =
             # Bin 1
             if_else(ScaledY <= BinCoordAssign$ScaledY[10], 1,
                     # Bin 2
                     if_else(ScaledY >= BinCoordAssign$ScaledY[10] & 
                               ScaledY <= BinCoordAssign$ScaledY[11], 2,
                             # Bin 3
                             if_else(ScaledY >= BinCoordAssign$ScaledY[11] & 
                                       ScaledY <= BinCoordAssign$ScaledY[12], 3,
                                     # Bin 4
                                     if_else(ScaledY >= BinCoordAssign$ScaledY[12] & 
                                               ScaledY <= BinCoordAssign$ScaledY[13], 4,
                                             # Bin 5
                                             if_else(ScaledY >= BinCoordAssign$ScaledY[13] & 
                                                       ScaledY <= BinCoordAssign$ScaledY[14], 5,
                                                     # Bin 6
                                                     if_else(ScaledY >= BinCoordAssign$ScaledY[14] & 
                                                               ScaledY <= BinCoordAssign$ScaledY[15], 6,
                                                             # Bin 7
                                                             if_else(ScaledY >= BinCoordAssign$ScaledY[15] & 
                                                                       ScaledY <= BinCoordAssign$ScaledY[16], 7,
                                                                     # Bin 8
                                                                     if_else(ScaledY >= BinCoordAssign$ScaledY[16], 8, 0
                                                                     )))))))))
  Colony19Binned <<- full_join(Colony19BinnedTube, Colony19BinnedCircle)
}

# Run the eight nest section binning function for the combined colony member data set
CoordBinned(FullDataCoordAll)

# COLONY 20
CoordBinned <- function(data_table){
  # Selecting the colony bin references
  BinCoordAssign <- BinCoordFull %>%
    filter(Colony == "20")
  
  # Binning the tube colony member coordinates
  # Filtering out the tube data for the selected colony
  Colony20BinnedTube <- data_table %>%
    filter(Colony == "20" & Nest == "Tube") %>%
    # Ifelse conditional statements for each bin
    mutate(Bin =
             # The bins determine whether the x and y coordinates are within a set of relevant bounds for each bin
             # The order is always x coordinates first, then y
             # Bin 1
             if_else(
               ScaledX >= BinCoordAssign$ScaledX[2] & 
                 ScaledX <= BinCoordAssign$ScaledX[1] &
                 ScaledY <= BinCoordAssign$ScaledY[2], 1,
               # Bin 2
               if_else(ScaledX >= BinCoordAssign$ScaledX[3] & 
                         ScaledX <= BinCoordAssign$ScaledX[2] &
                         ScaledY >= BinCoordAssign$ScaledY[3] &
                         ScaledY <= BinCoordAssign$ScaledY[2], 2,
                       # Bin 3
                       if_else(ScaledX >= BinCoordAssign$ScaledX[4] & 
                                 ScaledX <= BinCoordAssign$ScaledX[3] &
                                 ScaledY >= BinCoordAssign$ScaledY[3] &
                                 ScaledY <= BinCoordAssign$ScaledY[4], 3,
                               # Bin 4
                               if_else(ScaledX >= BinCoordAssign$ScaledX[4] & 
                                         ScaledX <= BinCoordAssign$ScaledX[5] &
                                         ScaledY >= BinCoordAssign$ScaledY[4] &
                                         ScaledY <= BinCoordAssign$ScaledY[5], 4,
                                       # Bin 5
                                       if_else(ScaledX >= BinCoordAssign$ScaledX[5] & 
                                                 ScaledX <= BinCoordAssign$ScaledX[6] &
                                                 ScaledY >= BinCoordAssign$ScaledY[4] &
                                                 ScaledY <= BinCoordAssign$ScaledY[5], 5,
                                               # Bin 6
                                               if_else(ScaledX >= BinCoordAssign$ScaledX[6] & 
                                                         ScaledX <= BinCoordAssign$ScaledX[7] &
                                                         ScaledY >= BinCoordAssign$ScaledY[7] &
                                                         ScaledY <= BinCoordAssign$ScaledY[6], 6,
                                                       # Bin 7
                                                       if_else(ScaledX >= BinCoordAssign$ScaledX[8] & 
                                                                 ScaledX <= BinCoordAssign$ScaledX[7] &
                                                                 ScaledY <= BinCoordAssign$ScaledY[7], 7,
                                                               # Bin 8
                                                               if_else(ScaledX >= BinCoordAssign$ScaledX[9] & 
                                                                         ScaledX <= BinCoordAssign$ScaledX[8] &
                                                                         ScaledY <= BinCoordAssign$ScaledY[9], 8, 0
                                                               )))))))))
  # Binning circle nest coordinates 
  # Filtering out the circle data for the selected colony
  # The code is the same as above, except its only y coordinates since xlims don't matter for the binning to work 
  Colony20BinnedCircle <- data_table %>%
    filter(Colony == "20" & Nest == "Circle") %>%
    mutate(Bin =
             # Bin 1
             if_else(ScaledY <= BinCoordAssign$ScaledY[10], 1,
                     # Bin 2
                     if_else(ScaledY >= BinCoordAssign$ScaledY[10] & 
                               ScaledY <= BinCoordAssign$ScaledY[11], 2,
                             # Bin 3
                             if_else(ScaledY >= BinCoordAssign$ScaledY[11] & 
                                       ScaledY <= BinCoordAssign$ScaledY[12], 3,
                                     # Bin 4
                                     if_else(ScaledY >= BinCoordAssign$ScaledY[12] & 
                                               ScaledY <= BinCoordAssign$ScaledY[13], 4,
                                             # Bin 5
                                             if_else(ScaledY >= BinCoordAssign$ScaledY[13] & 
                                                       ScaledY <= BinCoordAssign$ScaledY[14], 5,
                                                     # Bin 6
                                                     if_else(ScaledY >= BinCoordAssign$ScaledY[14] & 
                                                               ScaledY <= BinCoordAssign$ScaledY[15], 6,
                                                             # Bin 7
                                                             if_else(ScaledY >= BinCoordAssign$ScaledY[15] & 
                                                                       ScaledY <= BinCoordAssign$ScaledY[16], 7,
                                                                     # Bin 8
                                                                     if_else(ScaledY >= BinCoordAssign$ScaledY[16], 8, 0
                                                                     )))))))))
  Colony20Binned <<- full_join(Colony20BinnedTube, Colony20BinnedCircle)
}

# Run the eight nest section binning function for the combined colony member data set
CoordBinned(FullDataCoordAll)

# The following can be used to check how many coordinates have zero bin values, meaning they were not placed in a bin
# These coordinates need to be removed as they are most likely an error (a coordinate where no ant exists)
# This also checks whether the correct colony assignment was used throughout
# Each issue was examined in Excel 

# e.g.Colony1
Colony1Binned %>%
  filter(Bin == "0") %>%
  group_by(Bin) %>%
  summarise(n = n())

# Combine all data sets into the final one for workers, colonies 1-10
# This is because functions in other scripts keep colonies 1-10 (high nest density) and 11-20 (low nest density) separate
FullDataCoordWorkers <- Colony1Binned %>%
  full_join(Colony2Binned) %>%
  full_join(Colony3Binned) %>%
  full_join(Colony4Binned) %>%
  full_join(Colony5Binned) %>% 
  full_join(Colony6Binned) %>% 
  full_join(Colony7Binned) %>%
  full_join(Colony8Binned) %>% 
  full_join(Colony9Binned) %>%
  full_join(Colony10Binned) %>%
  # Remove all of the zeros, since some coordinates exist that may be accidental or error. 
  filter(ColonyMember == "Workers" & Colony < 11) %>%
  select(Colony, Nest, Day, ScaledX, ScaledY, Bin, AntID, Density)

# Combine all data sets into the final one for workers, colonies 11-20
# This is because functions in other scripts keep colonies 1-10 (high nest density) and 11-20 (low nest density) separate
FullDataCoordWorkersRD2 <- Colony11Binned %>%
  full_join(Colony12Binned) %>%
  full_join(Colony13Binned) %>%
  full_join(Colony14Binned) %>%
  full_join(Colony15Binned) %>%
  full_join(Colony16Binned) %>%
  full_join(Colony17Binned) %>%
  full_join(Colony18Binned) %>%
  full_join(Colony19Binned) %>%
  full_join(Colony20Binned) %>%
  # Remove all of the zeros, since some coordinates exist that may be accidental or error. 
  filter(ColonyMember == "Workers" & Colony > 10) %>%
  select(Colony, Nest, Day, ScaledX, ScaledY, Bin, AntID, Density)

# Join all data
FullDataCoordWorkersRD1_RD2 <- full_join(FullDataCoordWorkers, FullDataCoordWorkersRD2)

# Write dataset
write.csv(FullDataCoordWorkersRD1_RD2, here("analysis", "data", "processed", "FullDataCoordWorkersRD1_RD2.csv"), row.names = FALSE)

# Write dataset
write.csv(FullDataCoordWorkers, here("analysis", "data", "processed", "FullDataCoordWorkers.csv"), row.names = FALSE)

# Write dataset
write.csv(FullDataCoordWorkersRD2, here("analysis", "data", "processed", "FullDataCoordWorkersRD2.csv"), row.names = FALSE)

# Combine all data sets into the final one for brood, colonies 1-10
# This is because functions in other scripts keep colonies 1-10 (high nest density) and 11-20 (low nest density) separate
FullDataCoordBrood <- Colony1Binned %>%
  full_join(Colony2Binned) %>%
  full_join(Colony3Binned) %>%
  full_join(Colony4Binned) %>%
  full_join(Colony5Binned) %>% 
  full_join(Colony6Binned) %>% 
  full_join(Colony7Binned) %>%
  full_join(Colony8Binned) %>% 
  full_join(Colony9Binned) %>%
  full_join(Colony10Binned) %>%
  # Remove all of the zeros, since some coordinates exist that may be accidental or error. 
  filter(ColonyMember == "Brood" & Colony < 11) %>%
  select(Colony, Nest, Day, ScaledX, ScaledY, Bin, Density)

# Combine all data sets into the final one for brood, colonies 11-20
# This is because functions in other scripts keep colonies 1-10 (high nest density) and 11-20 (low nest density) separate
FullDataCoordBroodRD2 <- Colony11Binned %>%
  full_join(Colony12Binned) %>%
  full_join(Colony13Binned) %>%
  full_join(Colony14Binned) %>%
  full_join(Colony15Binned) %>%
  full_join(Colony16Binned) %>%
  full_join(Colony17Binned) %>%
  full_join(Colony18Binned) %>%
  full_join(Colony19Binned) %>%
  full_join(Colony20Binned) %>%
  #Remove all of the zeros, since some coordinates exist that may be accidental or error. 
  filter(ColonyMember == "Brood" & Colony > 10) %>%
  select(Colony, Nest, Day, ScaledX, ScaledY, Bin, Density)

# Join all data
FullDataCoordBroodRD1_RD2 <- full_join(FullDataCoordBrood, FullDataCoordBroodRD2)

# Write dataset
write.csv(FullDataCoordBroodRD1_RD2, here("analysis", "data", "processed", "FullDataCoordBroodRD1_RD2.csv"), row.names = FALSE)

# Write dataset
write.csv(FullDataCoordBrood, here("analysis", "data", "processed", "FullDataCoordBrood.csv"), row.names = FALSE)

# Write dataset
write.csv(FullDataCoordBroodRD2, here("analysis", "data", "processed", "FullDataCoordBroodRD2.csv"), row.names = FALSE)

# Combine all data sets into the final one for queens, colonies 1-10
# This is because functions in other scripts keep colonies 1-10 (high nest density) and 11-20 (low nest density) separate
FullDataCoordQueen <- Colony1Binned %>%
  full_join(Colony2Binned) %>%
  full_join(Colony3Binned) %>%
  full_join(Colony4Binned) %>%
  full_join(Colony5Binned) %>% 
  full_join(Colony6Binned) %>% 
  full_join(Colony7Binned) %>%
  full_join(Colony8Binned) %>% 
  full_join(Colony9Binned) %>%
  full_join(Colony10Binned) %>%
  #Remove all of the zeros, since some coordinates exist that may be accidental or error. 
  filter(ColonyMember == "Queens" & Colony < 11) %>%
  select(Colony, Nest, Day, ScaledX, ScaledY, Bin, Density)

# Combine all data sets into the final one for queens, colonies 11-20
# This is because functions in other scripts keep colonies 1-10 (high nest density) and 11-20 (low nest density) separate
FullDataCoordQueenRD2 <- Colony11Binned %>%
  full_join(Colony12Binned) %>%
  full_join(Colony13Binned) %>%
  full_join(Colony14Binned) %>%
  full_join(Colony15Binned) %>%
  full_join(Colony16Binned) %>%
  full_join(Colony17Binned) %>%
  full_join(Colony18Binned) %>%
  full_join(Colony19Binned) %>%
  full_join(Colony20Binned) %>%
  # Remove all of the zeros, since some coordinates exist that may be accidental or error. 
  filter(ColonyMember == "Queens" & Colony > 10) %>%
  select(Colony, Nest, Day, ScaledX, ScaledY, Bin, Density)

# Join all data
FullDataCoordQueenRD1_RD2 <- full_join(FullDataCoordQueen, FullDataCoordQueenRD2)

# Write dataset
write.csv(FullDataCoordQueenRD1_RD2, here("analysis", "data", "processed", "FullDataCoordQueenRD1_RD2.csv"), row.names = FALSE)

# Write dataset
write.csv(FullDataCoordQueen, here("analysis", "data", "processed", "FullDataCoordQueen.csv"), row.names = FALSE)

# Write dataset
write.csv(FullDataCoordQueenRD2, here("analysis", "data", "processed", "FullDataCoordQueenRD2.csv"), row.names = FALSE)

# Combine all data sets into the final one for alates
# Note that alates are only found in the low density treatment so there is only one script to combine alates
FullDataCoordAlates <- Colony11Binned %>%
  full_join(Colony12Binned) %>%
  full_join(Colony13Binned) %>%
  full_join(Colony14Binned) %>%
  full_join(Colony15Binned) %>%
  full_join(Colony18Binned) %>%
  full_join(Colony19Binned) %>%
  # Remove all of the zeros, since some coordinates exist that may be accidental or error. 
  filter(ColonyMember == "Alates" & Colony > 10) %>%
  select(Colony, Nest, Day, ScaledX, ScaledY, Bin, Sex, Ratio)

# Write dataset
write.csv(FullDataCoordAlates, here("analysis", "data", "processed", "FullDataCoordAlates.csv"), row.names = FALSE)
