# Ritger-Corynactis-urchin-deterrence

*Corynactis californica as a deterrent to purple urchin foraging*

I am studying how intraspecific variation might play a role in the ability of the strawberry anemone (*Corynactis californica*) to act as a deterrent to urchin foraging.

Methods methods methods


This repo is maintained by Stier Lab graduate student Amelia Ritger (GitHub: [@ameliaritger](https://github.com/ameliaritger)) at the University of California, Santa Barbara in the Department of Ecology, Evolution, & Marine Biology. Readme.md structure provided by Samatha Csik (GitHub: [@samanthacsik](https://github.com/@samanthacsik)). 

# Code

file name | analysis overview | description 
---|---|-----------
name | name | name


# Respirometry Data 
*/data/respirometry/*

Files and .pngs within the 'respirometry' subdirectory are generated in the 1_MR_data_processing.Rmd script using source code contained in MR_firesting_v1_1.R. Currently, the source code requires that all files be in the home directory for functions to run. For now, it is important to first ensure any .csv file called within MR_firesting_v1_1.R functions (txt_csv_convert(), MMR(), SMR(), MMR_SMR_AS_EPOC()) is found in the home ('project-lob') directory. Any .csvs and .pngs produced can then be sorted by hand into more intuitive and organized folders within the 'respirometry' subdirectory. 

# Functional Response Data 
*/data/functional_response/*

# LTER Data
*/data/LTER*

Data files exceed 100 MB, but can be downloaded from the Santa Barbara Coastal Long Term Ecological Research webpage at: 

 - [knb-lter-sbc.2002.24](http://sbc.lternet.edu/cgi-bin/showDataset.cgi?docid=knb-lter-sbc.2002) 
 - [knb-lter-sbc.13.21](http://sbc.lternet.edu/cgi-bin/showDataset.cgi?docid=knb-lter-sbc.13) 
 
Store downloaded files in your local repository within the LTER subdirectory to run associated code.

# Curious what the critters look like?
![Alt text](/media/Corynactis_urchin.jpg?raw=true "Urchin in contact with Corynactis californica on settlement tiles")