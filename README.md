
This repository contains the code, analysis workflows, and supporting materials 
used in the study on spatial-scale climatic variability in climate-health research.

## Data availability

- **Emergency department visit** data were obtained from the 
[Virginia All-Payer Claims Database](https://www.vhi.org/data/all-payer-claims-database-data/) 
(APCD), managed by Virginia Health Information (VHI). 
These data are confidential and cannot be redistributed through this repository. 
Interested researchers should request access directly from VHI.  

- **Weather station data** from the Richmond/Byrd Field station (RIC) are 
publicly available through the [Iowa Environmental Mesonet ASOS](https://mesonet.agron.iastate.edu/request/download.phtml?network=IN) 
archive.  

- **ERA5-Land reanalysis data** are publicly available through the 
[Copernicus Climate Data Store](https://doi.org/10.24381/cds.e2161bac).  

- **Social Vulnerability Index (SVI) data** are publicly available from the 
[CDC/ATSDR website](https://www.atsdr.cdc.gov/place-health/php/svi/svi-data-documentation-download.html).  

The code used for data processing and analysis is available in the `code/` directory.

## Reproducibility

All data preparation, statistical analyses, and visualizations were conducted in R. 
The workflow can be reproduced using the provided `Makefile`, 
which automates the execution order and dependencies of the analysis pipeline.  

## License

Code in this repository is released under the MIT License.

## Repo overview

	project
	|- README.md            # the top level description of content (this doc)
	|
	|- data                   # raw and primary data, are not changed once created
	| |- raw/                 # raw data, will not be altered
	| |- process/             # cleaned data, will not be altered once created
	| |- shapefile_zipcode/   # geographic boundary shapefiles for the study area
	| |- RDSmodel/            # modeling outputs
	| +- for_manuscript/      # intermediate outputs for manuscript preparation
	|
	|- code/                # any programmatic code
	| |- fn_*.R             # functions
        | |- get_*.R 	        # code for data processing
        | |- model_*.R          # code for model fitting
        | |- plot_*.R	        # code for figures generation
        | +- table_*.R          # code for tables generation
        |
        |- results		# all output from workflows and analyses
        | |- figures/		# graphs, likely designated for manuscript figures
        | +- pictures/		# diagrams, images, and other non-graph graphics
        |
        |- manuscript/
        | |- manuscript.Rmd	# executable Rmarkdown for this study
        | |- manuscript.tex	# TeX version of *.Rmd file
        | |- manuscript.pdf	# PDF version of *.Rmd file
        | |- manuscript.docx	# Word version of *.Rmd file
        | |- my_header.tex	# LaTeX header file to format pdf version of manuscript
        | |- bibliography.bib	# BibTeX formatted references
        | |- XXXX.csl		# csl file to format references for journal XXXX
        | +- *.Rmd		# child documents
        |
	|- exploratory/         # exploratory data analysis
	| |- nb_*/              # preliminary data analyses
	| |- text/              # notes and related documents
	| +- scratch/           # temporary files that can be safely deleted or lost
	|
	+- Makefile             # executable Makefile for this study

