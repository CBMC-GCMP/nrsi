# Normalized Reef Status Index (NRSI)

## Overview

The Normalized Reef Status Index (NRSI) is a novel metric developed to assess the health of marine rocky reef ecosystems by analyzing the trophic structure of reef fish communities. This repository contains the code and data used to calculate the NRSI and apply it to reef sites across the Mexican Pacific, as detailed in our recent publication in *Ecological Indicators*.

The NRSI is designed to be a rapid, scalable, and standardized tool for evaluating reef health. It enables comparisons across different spatial and temporal scales and can be used to assess the effectiveness of varying levels of marine protection.

## Contents

- **`data/`**: Contains the datasets used in the study, including fish biomass data, reef site locations, and protection levels.
- **`01-analysis.R/`**: Includes R script used for calculating the NRSI.
- **`figs/`**: Contains the figures generated for the publication.
- **`supplementary_materials/`**: Includes additional materials referenced in the paper, such as supplementary tables and detailed methodological descriptions.

## Installation

To replicate the analysis and generate the figures, you will need to have R installed on your system. The `tidyverse` R package is required. You can install it by running the following command in your R console:

```R
install.packages(c("tidyverse"))
```

## Acknowledgments

We acknowledge the contributions of the research teams and funding support from the International Community Foundation, the David and Lucile Packard Foundation, and National Geographic Pristine Seas. Special thanks to all collaborators who contributed to data collection and analysis over the years.

## Contact

For any questions or issues, please contact:

- Fabio Favoretto (ffavoretto@ucsd.edu)

