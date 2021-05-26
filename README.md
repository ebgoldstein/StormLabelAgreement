## IRR analysis of labeled post-storm images from NOAA

This is A repository for analysis of inter-rater agreement for labels of post-storm coastal images

To use this, please download the data from [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.4694325.svg)](https://doi.org/10.5281/zenodo.4694325)

You need a `/data` folder, and please put the downlaoded csv files in this folder.

There are 3 scripts here to use on the 2nd version of release data:

- `Prepdata.R`, must be run first. This script loads the data and pivots it into the needed format.
- `IRRcalcKA.R`, which calculates the IRR stat (Krippendorf's alpha) for each category.
- `Analysis.R`, which makes some plots and does some data analysis.


