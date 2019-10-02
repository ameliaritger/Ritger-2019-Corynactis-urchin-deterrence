# Ritger-Corynactis-urchin-deterrence

*Corynactis californica as a deterrent to purple urchin foraging*

I am studying how intraspecific variation might play a role in the ability of the strawberry anemone (*Corynactis californica*) to act as a deterrent to urchin foraging.

Purple urchins, starved for a minimum of 7 days, were placed in aquaria containing kelp (*Macrocystis pyrifera*) protected by a barrier of strawberry anemones. 3 color morphs of strawberry anemones were used (Red, Pink, and Orange) in trials to assess any differences in the ability of genets (genetically identical individuals) to deter urchins from grazing on kelp. Kelp consumption by urchins was measured using photos taken of kelp blades before and after experimental trials, which lasted between 19:00 and 7:00 (30 minutes before and after sunrise and sunset, respectively) in late August-early September. Urchin behavior was tracked using photos (taken by GoPro Hero3+) of the experimental set up each minute for approximately 5 hours at the start of each experiment. Urchin behavior was compared between genets to examine how intraspecific variation might play a role in the associational refuge offered by the strawberry anemone to macroalgae in temperate kelp forests.

This repo is maintained by Stier Lab graduate student Amelia Ritger (GitHub: [@ameliaritger](https://github.com/ameliaritger)) at the University of California, Santa Barbara in the Department of Ecology, Evolution, & Marine Biology. Readme structure provided by Samatha Csik (GitHub: [@samanthacsik](https://github.com/@samanthacsik)). 

# Code

file name | analysis overview | description 
---|---|-----------
kelp consumption analysis.R | Analyze kelp consumption | This works through data organization and analysis of urchin foraging behavior minus video analysis; analysis using logistic regression treating binary kelp consumption (yes/no) as a Bernoulli distribution and mixed model ANOVA treating tile as a random effect in kelp consumption by urchins
video analysis.R | Analyze trial videos | This works through data organization and analysis of urchin foraging behavior using videos shot by GoPro Hero 3+; each tile containing Corynactis was subject to one trial for video analysis.
water temperature.R | Average water temperatures | This works through data analysis averaging water temperature during each day experiments were run, separated by location (either campus point wet lab or Marine Biotech balcony.
power test.R | Power test | This uses data from Xavius Boone's 2019 OGC REU experiment (Github link: [@Boone-2019-urchin-anemone-fed-unfed](https://github.com/stier-lab/Boone-2019-urchin-anemone-fed-unfed)) to run a power test to estimate minimum sample sizes needed to find significance in the data.

# Data 
*/data/raw.csv*  raw data

*/data/kelp/*  kelp blade images before and after experiment, organized by date

# Curious what the experiment looks like?
Purple urchin in contact with Corynactis californica on settlement tiles
![Alt text](/media/Corynactis_urchin.jpg?raw=true)

Urchin in contact with Corynactis californica on settlement tiles during experiment
![Alt text](/media/urchin_in_contact.JPG?raw=true)

Example of Corynactis deterring urchins
<img src="https://media.giphy.com/media/vFKqnCdLPNOKc/giphy.gif" width="624" height="466.7" /> *Just kidding.*
<img src="https://media.giphy.com/media/kBe4VrggiK8etQ7eW4/giphy.gif" width="624" height="351" />

Sometimes Corynactis polyps don't like being walked on by urchins
<img src="https://media.giphy.com/media/W3fUlH7bOT2PX2UPjY/giphy.gif" width="624" height="351" />

Kelp blade before (left) and after (right) consumption by an urchin
![Alt text](/media/12before.jpg?raw=true)
![Alt text](/media/12after.jpg?raw=true)

Kelp blade before (left) and after (right) image analysis in ImageJ
![Alt text](/media/8before.png?raw=true)
![Alt text](/media/8after.png?raw=true)
