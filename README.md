🌍 climatic4economist

Work with climatic data and surveys

This repository contains functions and workflows designed to support extraction of climate data at survey locations, computation of weather extreme indicators, and merging of climatic information with survey data by location and interview date.
The climatic4economist package simplifies the process of bridging survey data with environmental data — a common need in socio-economic and impact studies where climatic conditions influence human and environmental outcomes.
The package builds on existing spatial data ecosystems (e.g., terra) and provides utilities that wrap up many steps so users can follow routine workflows with minimal effort.

📘 Features

* Extract climatic and other spatial data at survey locations
* Compute weather extreme indicators
* Generate lagged climatic variables relative to interview dates
* Merge climate metrics with socio-economic survey data
* Utility functions that wrap tedious spatial processing steps
  
This approach lets researchers:

* Focus on analysis rather than data wrangling
* Ensure reproducibility across projects
* Integrate climatic context into survey-based research

📥 Installation

Install directly from GitHub:

```
remotes::install_github("jmr-fao/climatic4economist")
```

📦 Dependencies

The package builds on the following packages:

* terra — raster and vector spatial operations
* sf — simple features support
* data.table — efficient data manipulation
* tidyvers -  data manipulation
* checkmate — robust input validation

📚 Background

This package was inspired by routines for linking large climate datasets with socio-economic surveys — a challenge often faced in impact studies and climate risk analysis.
climatic4economist focuses on workflows common in economist, social science, and development research where climate data must be extracted and aligned with survey metadata.
