# 📘 *Linguistic-Diversity-Thesis*

This repository contains all R scripts and datasets used for the thesis  
**"The Impact of Digitization on Language Diversity and Endangerment: A Statistical Cluster and Correlation Analysis Using R"**  
by *Katharina Zeh*, submitted in the Digital Humanities Master's degree program at the University of Vienna.

---

## 📁 Repository Structure

### `analysis/`
This folder contains all scripts related to the analytical steps of the thesis. It is subdivided into:

- **`cluster_analysis/`**  
  Scripts for the hierarchical cluster analysis of digitization measures, organized by year (2004–2023).

- **`correlation_analysis/`**  
  Scripts for year-by-year (2004-2023) correlation analyses between digitization indices and diversity measures. Subdivided by measure:
  - `entropy/`
  - `language_count/`
  - `RLI/` (Red List Index)

- **`breadth_analysis/`**  
  Script analyzing the conceptual breadth of each digitization index by year and in total (2004–2023).

---

### `datasets/`
Contains the datasets used in the thesis in csv format:
- Digitization indices
- Linguistic diversity measures
📖 See [datasets/data_sources.md](datasets/data_sources.md) for source information.
---

### `EDA/` *(Exploratory Data Analysis)*
Scripts for exploratory analysis, including:
- QQ plots for distribution checks of digitization indices  
- World maps visualizing language diversity at the national level  
- Overall correlation analysis of digitization indices across all years

---

## 📦 Dependencies

To run the scripts in this project, the following R packages are required or recommended:

### 📊 Core Analysis (Cluster, Correlation, Breadth)
- `dplyr`, `tidyr`, `readxl` – data manipulation & loading
- `clValid`, `corrplot`, `ggcorrplot`, `factoextra`, `dendextend`, `gridExtra` – statistical & visual analysis

### 📈 Visualization
- `ggplot2`, `viridis`, `scales` – general plots and styling
- `stringr`, `purrr`, `rlang` – used in helper functions (mostly in EDA)

### 🗺️ Mapping *(optional – used in EDA only)*
- `rnaturalearth` – required for mapping functions
- `rnaturalearthhires` – adds high-resolution shapefiles *(install via GitHub)*

---


## 📬 Contact
If you have any questions about the code or analysis, feel free to contact: **[katharina.zeh@univie.ac.at]**
