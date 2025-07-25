
# Global Prediction of Freshwater Fish Conservation Status  
### Integrating Environmental, Taxonomic, and Socioeconomic Drivers

This repository supports the research article:  
**Murphy, C.A., Olivos, J.A., et al. (2025). _Environment, Taxonomy, and Socioeconomics Predict Non-Imperilment in Freshwater Fishes_** (in review).

The project develops a global-scale, machine learning-based framework to predict the conservation status of freshwater fish species based on extrinsic environmental conditions, anthropogenic pressures, and intrinsic biological characteristics. It includes workflows for spatial data attribution, species trait compilation, and Random Forest classification modeling.

---

## Objectives

- Attribute 50+ predictor variables to the ranges of 10,000+ freshwater fish species worldwide using open-access global datasets.
- Identify key ecological and socioeconomic correlates of IUCN conservation status—specifically, non-imperilment.
- Implement a reproducible Random Forest classification model to assess species vulnerability.

---

## Repository Structure

```
.
├── notebooks/                             # Python-based geospatial attribution workflows
├── manuscript/                            # Manuscript and supplementary material
├── R_code_pdfs/                           # PDF exports of R scripts (for trait data and modeling)
└── README.md                              # Project documentation
```

---

## Data Attribution Workflows

### 🐍 Python-Based Geospatial Attribution (Jupyter Notebooks)

Each Jupyter notebook summarizes one or more spatial datasets across freshwater fish species’ native ranges.

| Notebook | Description |
|----------|-------------|
| `Pre-processing_IUCN_Ranges.ipynb` | Merges and filters native freshwater species range polygons from the IUCN Red List spatial database. |
| `GDW_Attribution.ipynb` | Calculates impoundment metrics (e.g., number of dams, blocked discharge, reservoir area) from Global Dam Watch datasets. |
| `GloRiC_Attribution.ipynb` | Computes hydro-geomorphic diversity, discharge, and stream power using the GloRiC global river classification dataset. |
| `HumanFootprint_Attribution.ipynb` | Summarizes the Human Footprint Index (2009) over each species’ range. |
| `HydroWASTE_Attribution.ipynb` | Quantifies the number and impact of wastewater treatment plants intersecting species ranges. |
| `LandCover_Attribution.ipynb` | Computes proportional land cover types (forest, grassland, cropland, etc.) using Copernicus land cover rasters. |
| `WorldClim_Attribution.ipynb` | Attributes 19 bioclimatic variables (e.g., precipitation seasonality, temperature extremes) from WorldClim v2.1. |
| `WorldPop_Attribution.ipynb` | Derives population density trends (2000–2020) using WorldPop. |
| `WorldBank_Attribution.ipynb` | Integrates national economic indicators (GDP trends, per capita change) from the World Bank WDI dataset. |
| `WDPA_Attribution.ipynb` | Measures the overlap between species ranges and protected areas from the WDPA database, including Ramsar sites. |

---

### 🧬 R-Based Trait, Occurrence, and Threat Attribution

The following R scripts (provided as PDFs) extract species-specific attributes from global biodiversity databases.

| Script | Description |
|--------|-------------|
| `IUCN_Attribution.R` (`IUCN_Attribution.pdf`) | Extracts taxonomic classification, threat categories, population trends, and habitat types using the IUCN API and spatial range data. |
| `FishBase_Attribution.R` (`FishBase_Attribution.pdf`) | Retrieves physiological, life-history, ecological, and use-related traits (e.g., trophic level, pH range, salinity, longevity, commercial use) from FishBase via the `rfishbase` package. |
| `GBIF_Attribution.R` (`GBIF_Attribution.pdf`) | Downloads and filters GBIF occurrence records; computes elevation statistics (mean, min, max) for each species. |
| `Dataset_Preparation.R` (`Dataset_Preparation.pdf`) | Merges all predictor variables into a final modeling dataset; standardizes and range-corrects spatial variables; handles missing values and correlation filtering. |

---

## Modeling Framework

### 🧠 Random Forest Classification Model (R)

Final conservation status predictions were produced using Random Forest models implemented in R. Key modeling steps included:

- **Response variable**: Binary (Imperiled vs. Non-Imperiled) based on IUCN categories.
- **Modeling tool**: `randomForest` package in R, tuned with `caret`.
- **Imputation**: Missing values imputed using `na.roughfix`.
- **Weighting**: Classes weighted (3:1) to address imbalance between imperiled and non-imperiled species.
- **Evaluation**:
  - Overall accuracy: **88%**
  - Non-imperiled accuracy: **90.1%**
  - Imperiled accuracy: **81.2%**

| Script | Description |
|--------|-------------|
| `Random_Forests_PDPs.R` (`Random_Forests_PDPs.pdf`) | Trains and evaluates the Random Forest model; generates confusion matrices and partial dependence plots to visualize predictor effects on conservation status. |

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

3. Refer to the R script PDFs in `R_code_pdfs/` for data integration, trait assignment, and modeling workflows.

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

- **Christina A. Murphy**  
  U.S. Geological Survey / University of Maine  
  📧 christina.murphy@maine.edu
