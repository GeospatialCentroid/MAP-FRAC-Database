# Interactively Explore the MAP-FRAC Database

This repository houses the underlying data and scripts for the MAP-FRAC Shiny App. You can explore the MAP-FRAC database and MAG-linking tool here: [**https://apps.gis.colostate.edu/MAP-FRAC-Database/**](https://apps.gis.colostate.edu/MAP-FRAC-Database/)

MAP-FRAC is a database of metagenome assembled genomes (MAGs) recovered from hydraulically fractured shales globally. Currently, the database is composed of 978 unique genomes from 9 shale basins in North America, as well as one in China (Sichuan) and the United Kingdom (Bowland). The MAP-FRAC shiny app is composed of three layers: 

(1) **‘Sample Explorer’** enables browsing of the geospatial distribution of samples and relevant metadata such as sample type, shale basin, salinity measurements, and timeseries information. For example, users can easily filter for samples by salinity range or days post-frack to explore the dataset composition. 

(2) **‘Genome Explorer’** allows investigation of microbial taxonomic distributions across shale basins. Users may search for a specific taxon at any taxonomic classification level, such as the genus Halanaerobium, to display the detection locations, colored by average relative abundance. 

(3) **‘MAG Linking Tool’** provides insights into the functional composition of shale microbiomes from amplicon datasets without needing to generate costly metagenomic sequencing data. This is an online-executable tool that connects 16S rRNA gene amplicon datasets to the MAG database presented here based on taxonomic classification. Users upload a standardized ASV table and the tool outputs interactive visualizations, downloadable data tables, and a PDF report summarizing successful linkages with caveats for interpretation of results.


**If you use the MAG-linking MAP-FRAC tool, please cite Amundson et al., 2026 (In review).**
