# Ytvattenkartor för Infocentralen Västerhavet

This Shiny web application provides interactive mapping and visualization of surface water anomalies in the Västerhavet region, based on data exported from InfoC. The app allows users to upload data, select parameters, and generate customized maps and summary plots based on historical statistics.

## ✨ Features

- Upload `.txt` files from InfoC export
- Dynamic selection of:
  - Year and month
  - Water quality parameters (e.g., Temperature, Salinity, Oxygen, etc.)
  - Depth
  - Geographic extent (Bohuslän, Halland, or dynamic)
- Automatically calculates DIN from NO₂, NO₃, and NH₄
- Compares measurements to historical statistics
- Categorizes anomalies (e.g., "Högre än normalt", "Lägre än normalt")
- Downloads:
  - Current plot as PNG
  - All parameter plots as a ZIP archive

## 📦 Installation

Clone this repository and install required R packages:

```r
install.packages(c("shiny", "tidyverse", "sf", "R.matlab", "ggrepel"))
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

## 🗺️ Plot Interpretation

- Color-coded anomalies:
  - 🔴 **Mycket högre än normalt**: `#d73027`
  - 🟠 **Högre än normalt**: `#fdae61`
  - 🟦 **Normala värden**: `#91bfdb`
  - 🔵 **Lägre än normalt**: `#4575b4`
  - 🟣 **Mycket lägre än normalt**: `#313695`
  - ⚪ **Ingen provtagning**: `grey70`
- Anomalies are based on comparison with historical means and standard deviations from `stat_stations.mat`.

## 📤 Exports

- **PNG**: Download the currently displayed plot.
- **ZIP**: Download all parameter plots for the selected year and month.

## 🔧 Customization

You can adjust:
- `parameter_map` to add or remove parameters.
- Color palettes in `anomaly_colors_swe`.
- Bounding box settings in `helper.R` depending on `bbox_option`.

## 🧪 Dependencies

- `shiny`
- `tidyverse`
- `sf`
- `R.matlab`
- `ggrepel`

## 📄 License

This project is licensed under the MIT License.
