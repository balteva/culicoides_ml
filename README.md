# _Culicoides_ML_

# 2025 Internship subject: **Spatio-temporal modeling of Culidcoides populations in France using Machine Learning**

## Scripts
- `covariates_hist_timeseries.R`
    **Rstudio script for primary data preprocessing and some basic visualisation such as histograms and timeseries**
- `covariates_boxplots.R`
    **Rstudio script for boxplots**
- `spatial_covariates.R`
    **Rstudio script for spatial data visualisation + some boxplots (need to transfer them to covariates_jeuxdedonnees script)**
## Description

## Visuals
**<p align="center"> <mark>1. Boxplots</mark>** </p>

        Culidoides spp. counts per trap per night 2009-2012
![*Culicoides spp.* counts per trap per night 2009-2012](./Visualised_Data_basic/Boxplots/boxplot_pertrap_pernight_simple_years.jpeg)

        With Ecolimatic zones
![*Culicoides spp.* counts per trap per night 2009-2012 with Ecolimatic zones](./Visualised_Data_basic/Boxplots/boxplot_pertrap_pernight_simple_years_eco_cli.jpeg)
-----------------------------------
        Culicoides spp. counts per trap per night for each month
![*Culicoides spp.* counts per trap per night for each month](./Visualised_Data_basic/Boxplots/boxplot_pertrap_pernight_simple_months_years.jpeg)

        With Ecoclimatic zones
![*Culicoides spp.* counts per trap per night for each month with Ecoclimatic zones](./Visualised_Data_basic/Boxplots/culicoides_boxplot_all_species_separate_eco_cli_years.jpeg)
-----------------------------------
**<p align="center"> <mark>2. Histograms</mark>** </p>

        Histogram of Culicoides spp. counts per trap per night 2009-2012 with mean and median
![Histogram of *Culicoides spp* counts per trap per night 2009-2012 with mean and median](./Visualised_Data_basic/Histograms/hist_pertrap_pernight_with_stats_cornflower_2009_2012.jpeg)

        With separate years
![Histogram of *Culicoides spp* counts per trap per night with mean and median (separate years)](./Visualised_Data_basic/Histograms/hist_pertrap_pernight_with_stats_cornflower_indiv_years.jpeg)

        With separate years and Ecoclimatic zones
![Histogram of *Culicoides spp* counts per trap per night with Ecoclimatic zones (separate years)](./Visualised_Data_basic/Histograms/hist_pertrap_pernight_with_stats_cornflower_indiv_years_eco_cli.jpeg)
-----------------------------------
**<p align="center"> <mark>3. Timeseries</mark>** </p>
        Temporal changes of TOTAL daily counts of Culidoides spp. per each sampling day
![Temporal changes of total daily counts of *Culidoides spp.* per each sampling day](./Visualised_Data_basic/Spatio-temporal/temporal_simple_cumulative.jpeg)

        With Ecoclimatic Zones
![Temporal changes of total daily counts of *Culicoides spp.* per each sampling day with Ecoclimatic zones](./Visualised_Data_basic/Spatio-temporal/temporal_changes_all_species.jpeg)
----------------------------------
        Temporal changes of mean daily counts of Culidoides spp. per trap per each sampling day
![Temporal changes of mean daily counts of *Culidoides spp.* per trap per each sampling day](./Visualised_Data_basic/Spatio-temporal/temporal_simple_per_trap_per_day_mean.jpeg)
----------------------------------
**<p align="center">Various ways to present the same data** </p>
        Standardized counts of Culidoides spp. per trap per each sampling day (month's mean)
![Standardized counts of *Culidoides spp.* per trap per each sampling day (month's mean)](./Visualised_Data_basic/Spatio-temporal/temporal_month_avg_per_trap.jpeg)

         With Ecoclmatic Zone
![Timeseries of standardized counts of *Culidoides spp.* per trap per each sampling day (month's mean) with Ecoclimatic zones](./Visualised_Data_basic/Spatio-temporal/temporal_month_avg_per_trap_ECO_CLI.jpeg)

        Same data but barcharts
![Barchart timeseries of standardized counts of *Culidoides spp.* per trap per each sampling day (month's mean)](./Visualised_Data_basic/Spatio-temporal/temporal_month_avg_per_trap_bars.jpeg)

         Barcharts with Ecoclimatic zone
![Barchart timeseries of standardized counts of *Culidoides spp.* per trap per each sampling day (month's mean) with Ecoclimatic zones](./Visualised_Data_basic/Spatio-temporal/temporal_month_avg_per_trap_ECO_CLI_bars.jpeg)
-----------------------------------
**<p align="center"> <mark>4. Maps</mark>** </p>>

        Culicoides counts across France during different years visualised in a map
![Culicoides counts across France during different years visualised in a map](./Visualised_Data_basic/Spatio-temporal/france2009_2012_maps_page-0001.jpg)
----------------------------------
## Roadmap of the **Culicoides project**

### **Backround**
- **Bibliography**
    - Culicoides in France since 2006
        - What trends were seen?
        - How were BTV and EHF introduced?
    - 2008 outbreak and 2015 reintroduction of arboviruses
        - Economic losses, current approaches to reduce spread
- Literature review of studies
    - Studies conducted so far
        - 2009-2012 period for France
        - 2015-2023 period for France
        - Foreign studies
    - Relation to Climate change
    - Challenges for studying *Culicoides*
        - Lifestyle related (cannot conduct lab experiments)
    - Findings so far
        - Species
        - Serotypes
        - Distribution
    - Current predictive models

### **Problematic**
- Current models not sufficient
    - Lack of robustness?
    - Low accuracy?
- Current projects/approaches to improve prediction/surveillance
- Alternative approaches
    - intro into ML

### **Relevance of this project**
- other projects with ML that have shown success (*Aedes spp*.)
- Could provide new information about environmental variables for epidemiological data and surveillance tactics:
    - will extract knowledge about variables at the very least, at most can predict

### **Methods**
- Data Exploration
    - Plots for visualising data distribution
        - Heatmaps
        - Histograms
        - Time series
    - Correlation maps
    - Feature weight plots
- VectorNet archives for new data
- Variable selection
- Model construction

### **Results**
- Describe temporal and spatial distribution of culicoides
- Data driven instead of knoweldge based - VectorNet data

### **Questions**
- Ecological determinants in culicoides pop. distribution?
- What contributed to BTV + EHD reemergence? Due to Culicoides population changes or introduction of new serotypes of virus?
- Why is there heterogeneity in distribution in ***local*** small scale?
- Do the ML models make sense in ecological and epidemiological terms?
- Am I predicting *Culicoides* complex distribution OR specific species?
- regions or times when the predictions work less well (AOA)

----

## Contributing

## Authors and acknowledgment

## Project status

