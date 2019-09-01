# Ritger-Corynactis-urchin-deterrence

*Corynactis californica as a deterrent to purple urchin foraging*

I am studying how intraspecific variation might play a role in the ability of the strawberry anemone (*Corynactis californica*) to act as a deterrent to urchin foraging.

Urchins, starved for a minimum of 7 days, were placed in aquaria containing kelp (*Macrocystis pyrifera*) protected by a barrier of strawberry anemones. 3 color morphs of strawberry anemones were used (Red, Pink, and Orange) in trials to assess any differences in the ability of genets (genetically identical individuals) to deter urchins from grazing on kelp. Kelp consumption by urchins was measured using photos taken of kelp blades before and after behavioral trials, which lasted between 19:00 and 7:00 (30 minutes before and after sunrise and sunset, respectively). Urchin behavior was tracked using photos (taken by GoPro Hero3+) of the experimental set up each minute for approximately 5 hours at the start of each experiment. Urchin behavior was compared between genets to examine how intraspecific variation might play a role in the associational defense offered by the strawberry anemone to macroalgae in temperate kelp forests.

This repo is maintained by Stier Lab graduate student Amelia Ritger (GitHub: [@ameliaritger](https://github.com/ameliaritger)) at the University of California, Santa Barbara in the Department of Ecology, Evolution, & Marine Biology. Readme structure provided by Samatha Csik (GitHub: [@samanthacsik](https://github.com/@samanthacsik)). 

# Code

file name | analysis overview | description 
---|---|-----------
kelp consumption analysis.R | Analyze kelp consumption | This works through data organization and analysis of urchin foraging behavior minus video analysis; analysis with mixed model ANOVA.
video analysis.R | Analyze trial videos | This works through data organization and analysis of urchin foraging behavior using videos shot by GoPro Hero 3+; each tile containing Corynactis was subject to one trial for video analysis.
power test.R | Power test | This uses data from Xavius Boone's 2019 OGC REU experiment (Github link: [@Boone-2019-urchin-anemone-fed-unfed](https://github.com/stier-lab/Boone-2019-urchin-anemone-fed-unfed)) to run a power test to estimate minimum sample sizes needed to find significance in the data.

# Data 
*/data/raw.xlsx* | raw data
*/data/kelp/* | kelp blade images before and after experiment

# Curious what the critters look like?
Urchin in contact with Corynactis californica on settlement tiles
![Alt text](/media/Corynactis_urchin.jpg?raw=true "Urchin in contact with Corynactis californica on settlement tiles")