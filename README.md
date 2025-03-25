# _Culicoides_ML_

## 2025 Internship Subject: 
### **Spatio-temporal Modeling of Culicoides Populations in France using Machine Learning**

---
## Project Structure
- **`/updated_scripts/`** – Contains updated scripts, model outputs, and interpretation.
- **`/scripts/`** – Legacy scripts for data preparation, cross-correlation mapping, and multivariate analysis.
- **`/datapaper_viz/`** – Visualizations of the Ocapi database (histograms, boxplots).
---
## **Data Summary**
**Study Period:** 2009-2012  
**Methodology:** UV light traps across metropolitan France  

- **1-2 traps per department**
- **~160 traps per year**
- **Total of 14,895 trapping sessions**
- **Collection of species, landscape, and microclimatic data**
**Trap Locations and Ecoclimatic Zones**  
![France Traps](./updated_scripts/plots/qgis/France_EcoCli_Departments_Traps.png)

**Grid for Spatial Cross-Validation (150km x 150km)**  
![Grid Map](./updated_scripts/plots/qgis/grid_map_france.png)

---

##  Project Objectives
### **1. Presence/Absence and Abundance Modeling**
- Predict *Culicoides* populations in France across different time periods.
- Identify key environmental factors influencing population changes.

### **2. Model Interpretation**
- Which factors lead to an earlier seasonal activity of *Culicoides spp.* ?
- What are the drivers of increased *Culicoides* abundance?

## **VARIABLE SELECTION**
### **CROSS-CORRELATION MAPS (CCM) for Meteorological Data**
- Brighter red values indicate stronger correlation;
- Lighter values indicate weak correlation;
- Grey's indicate no correlation;
- Red-bordered squares highlight the most correlated values
- Black bordered squares are at least 90% correlated as the highest value.</p>
*Note: Cross-Correlation Maps (CCM) show association strength, but not direction!*
---

####  **Presence/Absence Model CCM (Distance)**
![Distance Correlation](./updated_scripts/plots/var_selection/presence/france_presence_distance_ccm.jpeg)
---

###  **Feature Selection Process:**
- General Linear Mixed Models used for initial variable selection (**p < 0.05**).
- Features with correlation **> 0.7** in pairwise comparisons were filtered out, maintaining features with biological importance
- Categorical features (wind strength, farm type) were analyzed through Fisher’s Exact Test and Cramer's V.

### **Potential features for the Presence/Absence model which are significantly associated with the response (presence):**

![Plots of numerical vars for presence](./updated_scripts/plots/var_selection/presence/presence_vs_numerical_vars.jpeg)

*However, using a heatmap, we can see that a lot of them are highly correlated between each other:*
![heatmap correlation presence](./updated_scripts/plots/var_selection/presence/presence_var_correlation_heatmap.jpeg)

### **Final selection of numerical variables:**
![heatmap correlation presence](./updated_scripts/plots/var_selection/presence/selected_presence_vars_heatmap.jpeg)

---

## **Model output and Evaluation**
### **1. Presence Model Predictions**
#### **Observed vs. Predicted (2009-2012, LTO CV)**
![binary presence LTO output](./updated_scripts/plots/interpretation/model/presence/binary_LTO.jpeg)
---
#### **With Ecolimatic zones**
![binary presence LTO with ecoclimatic zones](./updated_scripts/plots/interpretation/model/presence/eco_cli_year/binary_LTO_ecocli.jpeg)
---
#### **Observed vs. Predicted probabilities for presence (2009 - 2012 LTO CV)**
![presence probability LTO](./updated_scripts/plots/interpretation/model/presence/presence_LTO_probability.jpeg)

#### **Comparing presence model with LTO and LLO CV**
![presence probabilitY LTO vs LLO](./updated_scripts/plots/interpretation/model/presence/LLTO_probability.jpeg)
---

### **2. Model Performance Metrics**
####  **ROC Curve for LLO & LTO Models**
![ROC Curve](./updated_scripts/plots/interpretation/model/presence/ROC_AUC_LLO_LTO.jpeg)
---
#### **Prediction Error (Weeks)**
- If error > 0 --> model predicts too late;
- If error < 0 --> model predicts too early
![Performance as error in weeks](./updated_scripts/plots/interpretation/model/presence/prediction_error_in_weeks_LLTO.jpeg)
---

#### **Variable Importance with categories**
![Variable importance without metrics)](./updated_scripts/plots/interpretation/model/presence/VIP_presence_no_metrics.jpeg)

#### **Scaled  Variable Importance**
![Variable importance with scale)](./updated_scripts/plots/interpretation/model/presence/VarImp_presence.jpeg)

---
## **Variable Interaction Analysis**
- Partial Dependence Plots (PDPs)
- Individual Conditional Expectation (ICE).
**PDPs and ICE show how  prediction changes when varying one predictor while keeping all other features fixed. Main difference:** 
- ICE shows how each individual instance's prediction changes when varying one feature's values
- PDP's show the average predction of all instances.
- Feature Interaction plots assessed with **H-statistic** which measures feature interaction strength
- **0 = no interaction, 1 = full interaction**

### **Partial Dependence Plot (PDP)**
![Presence PDPs)](./updated_scripts/plots/interpretation/model/presence/PDP_presence.jpeg)
---
### **Centered ICE-c plots.**
![ICE - centered with Ecoclim. zones)](./updated_scripts/plots/interpretation/model/presence/ICE_presence_ecocli.jpeg)
####  *By separating different ecoclimatic regions, we see that the response to differing predictor values is heterogenous for each instance (i.e. for each prediction), indicating that some feature interaction.*
---

### **Variable interaction analysis with H-statistic**
![feature interaction](./updated_scripts/plots/interpretation/model/presence/feature_interaction_presence.jpeg)
#### *Ex: H-stat of 0.3 indicates that 30% of its influence on the presence/absence prediction comes from interactions with other features, while 70% comes from its independent effect.*
---

### **Altitude vs Other Variables**
![Altitude interaction](./updated_scripts/plots/interpretation/model/presence/altitude_feature_int.jpeg)
---

### **2D Partial Depenence Plot**
![2D PDP](./updated_scripts/plots/interpretation/model/presence/betail_altitude_pdp.jpeg)
#### *Shows the strength and direction of the relationship between two interacting features*
---


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

