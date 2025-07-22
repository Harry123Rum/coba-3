# SIVANA - Social Index Vulnerability Analysis Navigator Application

## Overview
SIVANA is a comprehensive R Shiny dashboard designed for analyzing Social Vulnerability Index (SOVI) data. This dashboard provides a complete statistical analysis toolkit for exploring social vulnerability patterns across Indonesian districts.

## Features

### 🏠 **Beranda (Home)**
- Dashboard metadata and information
- Data source details and references
- User guide and navigation help

### 📊 **Manajemen Data (Data Management)**
- Convert continuous variables to categorical
- Create new derived variables
- Data transformation tools
- Interactive data categorization with interpretation

### 🔍 **Eksplorasi Data (Data Exploration)**
- Descriptive statistics
- Interactive visualizations (charts and maps)
- Correlation analysis
- Data tables with filtering
- OpenStreetMap integration for spatial analysis

### 📈 **Uji Asumsi Data (Data Assumption Tests)**
- Normality tests (Shapiro-Wilk, Anderson-Darling, etc.)
- Homogeneity tests (Levene's test, Bartlett's test)
- Statistical interpretations for each test

### 🧮 **Statistik Inferensia - Metode Dasar (Basic Statistical Methods)**
- Proportion tests (1 and 2 groups)
- Variance tests (1 and 2 groups)
- Detailed statistical interpretations

### 📊 **Statistik Inferensia - ANOVA**
- One-way ANOVA
- Two-way ANOVA
- Post-hoc tests
- Effect size calculations

### 🔗 **Regresi Linear Berganda (Multiple Linear Regression)**
- Multiple regression analysis
- Assumption testing (linearity, independence, normality, homoscedasticity)
- Model diagnostics and interpretation
- Variable selection techniques

### 💾 **Download Features**
- Export charts as JPG images
- Download analysis results as Word documents
- Export interpretations as formatted reports
- Bulk download by menu section

## Data Requirements

### Required Files:
1. **sovi_data.csv** - Main SOVI dataset with variables:
   - DISTRICTCODE: District identifier
   - CHILDREN, FEMALE, ELDERLY: Demographic variables
   - FHEAD, FAMILYSIZE: Family structure variables
   - NOELECTRIC, LOWEDU, GROWTH: Infrastructure and education
   - POVERTY, ILLITERATE, NOTRAINING: Socioeconomic indicators
   - DPRONE, RENTED, NOSEWER: Vulnerability indicators
   - TAPWATER, POPULATION: Additional demographic data

2. **sovi_administrasi_kabupaten.shp** - Shapefile for mapping
   - Administrative boundaries for districts
   - Geographic data for spatial visualization

## Installation & Setup

### Prerequisites
- R (version 4.0 or higher)
- RStudio (recommended)

### Required R Packages
```r
# Install required packages
install.packages(c(
  "shiny", "shinydashboard", "DT", "ggplot2", "plotly", 
  "leaflet", "sf", "dplyr", "corrplot", "psych", "car", 
  "nortest", "flextable", "officer", "shinyWidgets", 
  "shinycssloaders", "htmlwidgets", "RColorBrewer", 
  "viridis", "reshape2", "broom", "gridExtra"
))
```

### Running the Dashboard

1. **Clone or download** the project files
2. **Ensure data files** are in the same directory as SIVANA_Dashboard.R:
   - sovi_data.csv
   - sovi_administrasi_kabupaten.shp
3. **Open R/RStudio** and set working directory to project folder
4. **Run the dashboard**:
   ```r
   source("SIVANA_Dashboard.R")
   ```
5. **Access the dashboard** at `http://localhost:3838` (or the displayed URL)

### Alternative Running Method
```r
# Load the required libraries first
library(shiny)

# Run the app
runApp("SIVANA_Dashboard.R")
```

## Dashboard Structure

```
SIVANA Dashboard
├── Beranda (Home)
├── Manajemen Data (Data Management)
├── Eksplorasi Data (Data Exploration)
├── Uji Asumsi Data (Assumption Tests)
├── Statistik Inferensia - Dasar (Basic Statistics)
├── Statistik Inferensia - ANOVA (ANOVA Analysis)
└── Regresi Linear Berganda (Multiple Regression)
```

## Key Features

### 🎯 **Smart Interpretations**
Every statistical output includes:
- Automated interpretation of results
- Statistical significance explanations
- Practical implications
- Recommendations for further analysis

### 📱 **Responsive Design**
- Works on desktop, tablet, and mobile devices
- Intuitive navigation
- Clean, professional interface

### 🔄 **Interactive Analysis**
- Real-time data filtering
- Dynamic visualizations
- Customizable analysis parameters
- Export capabilities for all outputs

### 🗺️ **Spatial Analysis**
- Interactive maps using OpenStreetMap
- Choropleth mapping of vulnerability indices
- Geographic pattern analysis
- Spatial correlation visualization

## Data Source
This dashboard is designed to work with Social Vulnerability Index (SOVI) data as described in:
- **Publication**: "A district-level dataset for assessing social vulnerability to natural hazards in Indonesia"
- **URL**: https://www.sciencedirect.com/science/article/pii/S2352340921010180
- **Journal**: Data in Brief

## Troubleshooting

### Common Issues:
1. **Package Installation Errors**: Ensure you have the latest R version and try installing packages one by one
2. **Shapefile Loading Issues**: Verify all shapefile components (.shp, .shx, .dbf, .prj) are present
3. **Memory Issues**: For large datasets, consider increasing R memory limit
4. **Port Conflicts**: If port 3838 is busy, Shiny will automatically find an available port

### Getting Help:
- Check the R console for error messages
- Ensure all data files are in the correct location
- Verify package installations are complete

## License
This dashboard is created for educational and research purposes. Please cite the original SOVI dataset when using this tool for academic work.

---

**SIVANA Dashboard** - Making social vulnerability analysis accessible and comprehensive.