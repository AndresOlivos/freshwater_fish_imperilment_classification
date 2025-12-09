
# Global Prediction of Freshwater Fish Conservation Status  
### Integrating Environmental, Taxonomic, and Socioeconomic Drivers

This repository supports the research article:  
**Murphy, C.A., Olivos, J.A., et al. (2025). _Environment, Taxonomy, and Socioeconomics Predict Non-Imperilment in Freshwater Fishes_** (in review).

The project develops a global-scale, machine learning-based framework to predict the conservation status of freshwater fish species based on extrinsic environmental conditions, anthropogenic pressures, and intrinsic biological characteristics. It includes workflows for spatial data attribution, species trait compilation, and Random Forest classification modeling.

---

## Objectives

- Attribute 50+ predictor variables to the ranges of 10,000+ freshwater fish species worldwide using open-access global datasets.
- Identify key ecological and socioeconomic correlates of IUCN conservation status.
- Implement a reproducible Random Forest classification model to assess species imperilment.
- Assess predictive performance on a holdout validation dataset of 20% of the species.

---

## Repository Structure

```
.
├── jupyter_notebooks/                     # Python-based geospatial attribution workflows
├── R_code/                                # R scripts (for trait data and Random Forest modeling)
└── README.md                              # Project documentation
```

---

## Data Attribution Workflows

### Python-Based Geospatial Attribution

Each Jupyter notebook summarizes one or more spatial datasets across freshwater fish species’ native ranges.

| Notebook | Description | Core dependencies
|----------|-------------|----------|
| `Pre-processing_IUCN_Ranges.ipynb` | Merges and filters native freshwater species range polygons from the IUCN Red List spatial database. | `arcpy` |
| `GDW_Attribution.ipynb` | Calculates impoundment metrics (e.g., number of dams, blocked discharge, reservoir area) from Global Dam Watch datasets. | `arcpy` |
| `GloRiC_Attribution.ipynb` | Computes hydro-geomorphic diversity, discharge, and stream power using the GloRiC global river classification dataset. | `arcpy` |
| `HumanFootprint_Attribution.ipynb` | Summarizes the Human Footprint Index (2009) over each species’ range. | `arcpy` and `exactextract` |
| `HydroWASTE_Attribution.ipynb` | Quantifies the number and impact of wastewater treatment plants intersecting species ranges. | `arcpy` |
| `LandCover_Attribution.ipynb` | Computes proportional land cover types (forest, grassland, cropland, etc.) using Copernicus land cover rasters. | `arcpy` and `exactextract` |
| `WorldClim_Attribution.ipynb` | Attributes 19 bioclimatic variables (e.g., precipitation seasonality, temperature extremes) from WorldClim v2.1. | `arcpy` and `exactextract` |
| `WorldPop_Attribution.ipynb` | Derives population density trends (2000–2020) using WorldPop. | `arcpy` and `exactextract` |
| `WorldBank_Attribution.ipynb` | Integrates national economic indicators (GDP trends, per capita change) from the World Bank WDI dataset. | `arcpy` |
| `WDPA_Attribution.ipynb` | Measures the overlap between species ranges and protected areas from the WDPA database, including Ramsar sites. | `arcpy` |

---

### R-Based Species Traits, Occurrence, and IUCN Attribution

The following R scripts (provided as PDFs) extract species-specific attributes from global biodiversity databases.

| Script | Description |
|--------|-------------|
| `IUCN_Attribution.R` | Extracts taxonomic classification, threat categories, population trends, and habitat types using the IUCN API and spatial range data. |
| `FishBase_Attribution.R` | Retrieves physiological, life-history, ecological, and use-related traits (e.g., trophic level, pH range, salinity, longevity, commercial use) from FishBase via the `rfishbase` package. |
| `GBIF_Attribution.R` | Downloads and filters GBIF occurrence records; computes elevation statistics (mean, min, max) for each species. |
| `Dataset_Preparation.R` | Merges all predictor variables into a final modeling dataset; standardizes and range-corrects spatial variables; handles missing values and correlation filtering. |

---

## Modeling Framework

### Random Forest Classification Model (R)

Final conservation status predictions were produced using Random Forest models implemented in R. Key modeling steps included:

- **Response variables**: Ordinal (LC>NT>VU>EN>CR) and Binary (Imperiled vs. Non-Imperiled).
- **Predictive variables**: 50+ environmental, socioeconomic, and intrinsic species variables.
- **Imputation**: Missing values imputed using `na.roughfix`.
- **Training/test split**: 80% training / 20% hold-out test.
- **Modeling tools**: `party` and `randomForest` packages in R, called through `caret` for `mtry` tuning.
- **Evaluation metrics**: macro-averaged mean absolute error, Precision-Recall AUC, F1, among others.
- **Weighting**: Binary classes weighted (3:1) to address imbalance between imperiled and non-imperiled species.

- **Classification accuracy**: Ordinal Random Forest
  - Overall accuracy: **73.7%**
  - LC: **71%**
  - NT: **50.4%**
  - VU: **56.2%**
  - EN: **69.7%**
  - CR: **60%**

- **Classification accuracy**: Binary Categorical Random Forest
  - Overall accuracy: **88%**
  - Non-imperiled accuracy: **90.1%**
  - Imperiled accuracy: **81.2%**

| Script | Description |
|--------|-------------|
| `Random_Forests.R` | Trains and evaluates the Random Forest model; generates confusion matrices and partial dependence plots to visualize predictor effects on conservation status. |

---

## Technical Requirements

### Python Environment

Requires Python 3.9+ and the following packages:

```bash
pip install geopandas rasterio shapely pandas numpy matplotlib pyproj earthpy fiona rtree
```

### R Environment

- `rfishbase`
- `rgbif`
- `CoordinateCleaner`
- `randomForest`
- `caret`
- `pdp`
- `party`
- `dplyr`
- `ggplot2`

Modeling and dataset integration were executed in R 4.2+ using RStudio.

---

## Usage Instructions

1. Clone the repository:
   ```bash
   git clone https://github.com/yourusername/freshwater-fish-conservation.git
   cd freshwater-fish-conservation
   ```

2. Run the Jupyter notebooks in the `/notebooks/` directory sequentially to reproduce spatial attribution.

3. Refer to the R script PDFs in `R_code/` for data integration, trait assignment, and modeling workflows.

---

## Applications and Relevance

This repository is designed to support transparent, scalable, and reproducible global assessments of species conservation status. It provides:

- A platform for exploring proactive conservation indicators.
- A framework for applying consistent predictors to species not yet evaluated by IUCN.
- An example of combining environmental, socioeconomic, and biological information using modern data science tools.

---

## Citation

If you use this codebase or methodology, please cite:

> Murphy, C.A., Olivos, J.A., et al. (2025). *Environment, Taxonomy, and Socioeconomics Predict Non-Imperilment in Freshwater Fishes*. (In review).

---

## Contact

For questions, modeling scripts, or collaboration inquiries:

- **J. Andrés Olivos**  
  Department of Fisheries, Wildlife, and Conservation Sciences  
  Oregon State University  
  📧 andres.olivos@oregonstate.edu
