# Interactively Explore the MAP-FRAC Database

This repository houses the underlying data and scripts for the MAP-FRAC Shiny App. You can explore the MAP-FRAC database and MAG-linking tool here: [**https://apps.gis.colostate.edu/MAP-FRAC-Database/**](https://apps.gis.colostate.edu/MAP-FRAC-Database/)

MAP-FRAC is a database of metagenome assembled genomes (MAGs) recovered from hydraulically fractured shales globally. Currently, the database is composed of 978 unique genomes from 9 shale basins in North America, as well as one in China (Sichuan) and the United Kingdom (Bowland). The MAP-FRAC shiny app is composed of three layers: 

(1) **‘Sample Explorer’** enables browsing of the geospatial distribution of samples and relevant metadata such as sample type, shale basin, salinity measurements, and timeseries information. For example, users can easily filter for samples by salinity range or days post-frack to explore the dataset composition. 

(2) **‘Genome Explorer’** allows investigation of microbial taxonomic distributions across shale basins. Users may search for a specific taxon at any taxonomic classification level, such as the genus Halanaerobium, to display the detection locations, colored by average relative abundance. 

(3) **‘MAG Linking Tool’** provides insights into the functional composition of shale microbiomes from amplicon datasets without needing to generate costly metagenomic sequencing data. This is an online-executable tool that connects 16S rRNA gene amplicon datasets to the MAG database presented here based on taxonomic classification. Users upload a standardized ASV table and the tool outputs interactive visualizations, downloadable data tables, and a PDF report summarizing successful linkages with caveats for interpretation of results.

## Repository Structure
```
MAP-FRAC-Database/
├── app.R                        # Main Shiny application script
├── data_explore_clean.Rmd       # R Markdown file for prepping the app data and data exploration
├── data/                        # Spatial and tabular data for the Sample and Genome Explorer layers
│   ├── SAMPLE_geospatial_metadata_3.28.24.xlsx   # Metadata for samples included in the study
│   ├── GENOME_geospatial_metadata_02.17.2026.xlsx # Metadata for the 978 MAGs
│   └── [shapefiles for shale basins and formations]
├── src/                         # Helper R scripts sourced by app.R
│   ├── generate_plots.R         # Generates diagnostic and summary plots for ASV-MAG linking results
│   └── run_matching_tool.R      # Links ASVs to MAGs across taxonomic levels
├── tool/                        # Files underlying the MAG linking tool
│   ├── shale_MAGS_978_v2_02.17.2026.txt          # MAG metadata database
│   └── permian_feature_table_w_tax.txt            # Example ASV feature table for testing
└── www/                         # Static web assets for the Shiny app
    └── style.css                # Custom CSS styling
```
**Description of data in this repository:** 

`data/` contains all shapefiles for shale basin and formations and underlying data for the Sample Explorer and Genome Explorer layers of the shiny app. Specifically, SAMPLE_geospatial_metadata_3.28.24.xlsx contains metadata on samples included in this study, while GENOME_geospatial_metadata_02.17.2026.xlsx provides information on the 978 MAGs described here. 

`tool` contains all files underlying the execution of the MAG linking tool, including MAG metadata (shale_MAGS_978_v2_02.17.2026.txt) and an example dataset (permian_feature_table_w_tax.txt) that can be directly input into the MAG linking tool on the MAP-FRAC interface. 

More information on the MAG linking tool and R code that can be executed independently from the MAP-FRAC shiny app is documented here: https://github.com/kkamundson/shale_biogeo/tree/main/MAG_linking_tool 


**If you use the MAG-linking MAP-FRAC tool, please cite Amundson et al., 2026 (In review).**
