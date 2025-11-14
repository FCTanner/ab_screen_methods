# ab_screen_methods

Phenotyping data analysis pipeline for Ascochyta blight screens of chickpea

## Description

Methods for the analysis of RGB and multipectral images acquired in an Ascochyta Blight disease resistance screen of chickpea and wild Cicer material.
[Link to article]()

## Getting started

### Contents

- Raw data 
	* Raw image data (can be found here: )
		* 2020: RGB images
		* 2021: RGB images and multispectral images
		* 2022: RGB images and multispectral images
	* Annotated data? (can be found here: )
		* For segmentation
		* For object detection 
	* Scores
- Pre-processing methods
	* Trait extraction
		* Weights for yolov5 lesion detection
		* Multispectral segmentation
		* Vegetation index calculation
		* RGB segmentation
		* FGCC calculation
	* 
- Pre-processed data
	* 2020 main
	* 2021 main
	* 2021 subset
- Spatial and temporal modelling
	* Scripts
		* Pull data sources

- Prediction / Classification of disease scores
- Visualization
- GxE analysis


### Data dictionary

[2020_main.csv]()
[2021_main.csv]()
[2022_main.csv]()

| variable     | class     | description                                        |
|--------------|-----------|----------------------------------------------------|
| pot          | character | Pot ID                                             |
| date         | date      | Imaging date                                       |
| genotype_id  | character | Genotype ID                                        |
| type         | character | Type of genotype                                   |
| row          | double    | Row - design                                       |
| column       | double    | Column - design                                    |
| rep          | double    | Rep                                                |
| fgcc         | double    | Fractional Green Canopy Cover (Canon)                  |
| n_lesions    | double    | Number of lesions detected with yolov5             |
| size_lesions | double    | Ratio of area of pot covered with lesions (yolov5) |


Additional variables for:
[2021_fungicide.csv]()
[2022_fungicide.csv]()

| variable   | class     | description                                     |
|------------|-----------|-------------------------------------------------|
| …          | …         | …                                               |
| treatment  | character | +/- Fungicide treatment                         |
| canon_fgcc | double    | FGCC calculated from Canon DSLR                 |
| fgcc       | double    | FGCC calculated from Micasense RedEdge          |
| ndvi       | double    | Normalized Difference Vegetation Index          |
| endvi      | double    | Enhanced Normalized Difference Vegetation Index |
| rendvi     | double    | Normalized Difference RedEdge Index             |
| ngrdi      | double    | Normalized Green-Red Difference Index           |
| gndvi      | double    | Green NDVI                                      |
