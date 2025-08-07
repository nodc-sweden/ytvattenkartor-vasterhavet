# Surface water maps for the Västerhavet Info Centre

This repository contains a Shiny web application provides interactive mapping and visualization of surface water anomalies in the Västerhavet region, based on data exported for InfoC. The app allows users to upload data, select parameters, and generate customized maps and summary plots based on historical statistics.

## ✨ Features

- Upload `.txt` files from a InfoC export from **SHARKtoolbox**
- Dynamic selection of:
  - Year and month
  - Water quality parameters (e.g., Temperature, Salinity, Oxygen, etc.)
  - Geographic extent (Bohuslän, Halland, Bohuslän + Halland or dynamic based on available coordinates)
  - Plot size
- Calculates DIN from NO₂, NO₃, and NH₄
- Compares measurements to historical statistics
- Uses values from specific depths:
  - **Surface values (0 m)** are used for all parameters except for **oxygen concentration**
  - **Bottom values** are used for **oxygen concentration** to capture deep-water conditions
- Categorizes anomalies (e.g., "Högre än normalt", "Lägre än normalt")
- Downloads:
  - Current plot as PNG
  - All parameter plots for the current month as a ZIP archive
  - Monthly reports as a PDF

### Screenshot

![Screenshot of the app](assets/screenshot.png)

## 📦 Installation

Clone this repository and install required R packages:

```r
install.packages(c("shiny", "tidyverse", "sf", "R.matlab", "ggrepel", "png", "tiff", "jpeg", "grid", "gridExtra", "ggpubr"))
```

## 🚀 Running the App

From the R console or RStudio:

```r
shiny::runApp()
```

Or click **Run App** in RStudio.

## 📁 File Structure

```
.
├── R/
│   └── helper.R             # Contains helper functions like create_plot(), assign_pie_fill(), etc.
├── data/
│   └── stat_stations.mat    # MATLAB file with historical station statistics
├── app.R # Main Shiny app file
└── README.md
```

## 📄 Data Requirements

- **Uploaded file**: Export from InfoC in `.txt` format (tab-separated), encoded in ISO-8859-1.
- Must include columns like `Year`, `Month (calc)`, `Lat`, `Lon`, `Depth`, `Station`, and selected parameters (e.g., `Temp CTD (prio CTD)`).
- Automatically harmonizes station naming and calculates DIN values.

## 📤 Exports

- **PNG**: Download the currently displayed plot.
- **ZIP**: Download all parameter plots for the selected year and month.
- **PDF**: Download all parameter plots for the selected year and month in a PDF, including a logo page.

## 🔧 Customization

You can adjust:
- `parameter_map` to add or remove parameters.
- Color palettes in `anomaly_colors_swe`.
- Bounding box settings in `helper.R` depending on `bbox_option`.

## 🧪 Dependencies

### SHARKtoolbox

**SHARKtoolbox** is required to create the infoC export `.txt` file from LIMS exports.

### R packages
- `shiny`
- `dplyr`
- `ggplot2`
- `stringr`
- `readr`
- `purrr`
- `tibble`
- `R.matlab`
- `ggrepel`
- `sf`
- `png`
- `tiff`
- `jpeg`
- `grid`
- `gridExtra`
- `ggpubr`

## 📄 License

This project is licensed under the MIT License.
