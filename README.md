## Analysis of inter-rater agreement for labels of post-storm coastal images

To learn more about the labeling process for earth and environmental science imagery images, we solicited multiple labels for aerial images of US coastal environments after three hurricanes ( Florence, Michael, Isaias). We ask a range of questions about storm impacts, and each image is labeled between two and seven times by a group of 22 coastal scientists. The multiple-choice questions vary between two and four possible answers, and some questions allow for multiple answers. The corpus of images are from the large repository of Emergency Response Imagery collected by the National Geodetic Survey Remote Sensing Division of the US National Oceanographic and Atmospheric Administration. From all the labels, we computed inter-rater reliability.

To use the scripts in this repository, please download the data from:

 [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.4694325.svg)](https://doi.org/10.5281/zenodo.4694325)

Please make a `/data` folder in this directory, and put the downloaded csvs in this folder.

There are 3 scripts here to use on the release data:

- `Prepdata.R`, must be run first. This script loads the data and pivots it into the needed format.
- `IRRcalcKA.R`, which calculates the IRR stat (Krippendorf's alpha) for each category.
- `Analysis.R`, which makes some plots.

To see details about the larger project, look here at our previous paper: "An Active Learning Pipeline to Detect Hurricane Washover in Post-Storm Aerial Images": [![Earth ArXiv Preprint
DOI](https://img.shields.io/badge/%F0%9F%8C%8D%F0%9F%8C%8F%F0%9F%8C%8E%20EarthArXiv-doi.org%2F10.31223%2FX5JW23-%23FF7F2A)](https://doi.org/10.31223/X5JW23)

To see details on the labeling interface: https://github.com/UNCG-DAISY/Coastal-Image-Labeler

You can merge these labels with post-storm images, which can be downloaded from [NOAA NGS](https://storms.ngs.noaa.gov/) or using [this nifty python command line downloading tool](https://github.com/UNCG-DAISY/psi-collect). 

If you would like to help label imagery, check out this site: https://coastalimagelabeler.science/