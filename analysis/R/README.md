# README for Nest shape influences colony organization in ants: spatial distribution and connectedness of colony members differs from that predicted by random movement and is affected by available space

***

## Overview
RScript for the manuscript: Nest shape influences colony organization in ants: spatial distribution and connectedness of colony members differs from that predicted by random movement and is affected by available space

***

## Purpose of the study
### Investigating how nest shape influences how _Temnothorax rugatulus_ colonies spatially organize in their nests. This includes physical location of colony members and their distances from the entrance, mobile colony member distance to the brood center, worker distance to the physical center of the nest, and comparing worker distributions with those predicted by a random walk model. 

## Dependencies 
##### Scripts for this manuscript should be executed in the following order: 
0. Stat_boxplot_custom.R - Custom boxplot function, replacing geom_boxplot() in ggplot2. The function extends the whisker range to the data range
1. Bins_Working.r - Functions to bin colony member and Netlogo simulation coordinates into nest sections
2. DistanceFunctions.R - Functions to find each colony member and Netlogo simulation coordinates shortest distances in the nest
3. SFZFunctions.R - Functions towards marker worker site fidelity (spatial fidelity & occurrence zones)
4. NestArchFuncts.Analyses.R - All analyses and plots
##### Several packages are required, however all are loaded through the package "pacman", so be certain to install this package before running any other code.
##### See the following documentation for further information on the "pacman" package: https://www.rdocumentation.org/packages/pacman/versions/0.5.1 

***
