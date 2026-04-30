# Sargassum Dynamics using Sentinel-2 MSI (LPNMR, Puerto Rico)

This repository contains R scripts used for environmental data for the detection, quantification, and analysis of surface Sargassum dynamics in the La Parguera Natural Marine Reserve (LPNMR), Puerto Rico.

## Overview

This project supports the analysis presented in the manuscript on Sargassum dynamics using Sentinel-2 MSI imagery from 2016 to 2024.

The workflow integrates Sentinel-2 data time-series analysis, and environmental drivers including wind and surface currents.

## Repository Structure

scripts/
- time_series_analysis.R
- environmental_analysis.R
- current_analysis.R
- regression_analysis.R
- figure_generation.R

## Data Sources

Sentinel-2 MSI (Level-1C): https://scihub.copernicus.eu/  
NOAA NDBC Buoy Data (Station 42085): https://www.ndbc.noaa.gov/  
Copernicus Marine Service (CMEMS): https://marine.copernicus.eu/

## Methods Summary

- Time-series analysis (2016–2024)
- Environmental data integration (wind, wave, currents)
- Statistical analysis (R)

## R Packages Used

- tidyverse
- dplyr
- lubridate
- ggplot2
- terra
- patchwork

## Reproducibility

Scripts are provided to reproduce the main analysis steps. File paths may need to be adapted to local systems.

## Contact

Jenniffer Pérez-Pérez  
jenniffer.perez1@upr.edu
University of Puerto Rico, Mayagüez  

## License

For academic and research use. Please cite the associated manuscript when using this code.
