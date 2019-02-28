# cmartr
R functions for the Community Mangement of Amazonian River Turtles

Provides functions for the demographic analysis of 
    river turtle management options. 
    Functions are organized by name in three groups: 
    `prep..` , `Pop...` and `res...` .
    `prep..` are functions used to generate spatial layers 
    (vector and raster files), with countries, catchments, rivers,
    protected areas and human accessibility.
    `Pop...` are functions used for modelling turtle population demographics.
    `res...` are functions used to generate results, 
    producing tables and figures.
    
Example showing the use of these functions is available here: 
  Norris,D.; Peres, C.A.; Michalski, F.; Gibbs, J.P. Prospects for freshwater turtle population recovery are catalyzed by pan-Amazonian community-based management. Biological Conservation, Volume 233. 2019.         https://doi.org/10.1016/j.biocon.2019.02.022. 

and http://myturtlebrazil.wixsite.com/whereismyturtle 
and https://www.researchgate.net/profile/Darren_Norris

## Installation to R
Install development version from github.

1. install current devtools package from CRAN: `install.packages("devtools")` .

2. Use devtools to install the parcelareadev package from github: `devtools::install_github("darrennorris/cmartr")` .

3. load: `library("cmartr")`
