# PILOT DATA - August 2021
# this is the next step for thinking about sampling. what do we want to measure, how should we choose pieces? 
#   
#   goals for sampling: 
# * gas flux (LGR chamber measurements)
# * microbes (sawdust, CTAB, then sequencing)
# * also potentially some kind of moisture content (?)
# 
# considerations for sampling: 
# * in my grant proposals, I wanted to key into 1-2 species and then sample across all 5 decay classes 
# * search time could be a little intense - to sample, id be going out into forestGEO with quads and coordinates, but it would 
#   still be a search process
# * right now, sawing out little cubes seems to be the best bet for subsampling logs. still up for debate is multiple samples
#   along the length of the log to better spatially represent decay, if a single cube could be sufficient for flux, moisture content, and also what best methods can be used in drilling for microbes 
# 
# what we are looking for in the dataframe:
# * samples that have DC for all 3 'timepoints' (?) (2014, 2017, 2021) -> 'complete cases'
# * alternatively, is it ok if there are measures for JUST 2014 and 2021, or JUST 2017 and 2021 -> 'partial cases'
# * we also should take a look at how many complete cases exist for the target species that we named (oak, QUERC and tulip poplar, LITU) 

# libraries
library(dplyr)
library(ggplot2)
library(tidyr)

# read in data 
new.df <- read.csv("/Users/abbeyyatsko/Desktop/repos/serc_deadwood/data_FORESTGEO/final_data_FORESTGEO/cleaned_survey_14-21.csv")

# from new.df: want PIECETAG, SPCODE, and then all records for DC across the 3 surveys 
dw_select <- new.df %>%
  select(c('STEMTAG', 'PIECETAG', "QUADNAME", "QX.x", "QY.x", 'SPCODE', 'DC.2014', 'DC.2017', 'DC.2021'))
# what kind of species are there?
unique(dw_select$SPCODE)

# what is the distribution of species counts? 
dw_select %>% 
  drop_na(SPCODE) %>%
  ggplot(aes(x = SPCODE))+
  geom_bar(stat="count")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
# this figure shows species and frequency of dw pieces 

# potential candidates:
# tulip poplar (LITU) 
# oaks (QUFA, QUERC, QUAL)
# red maple (ACRU)
# american beech (FAGR)
# sweetgum (LIST2)

# NOTE that FRPE has high hits, but this is probably because of the ash dieback that is pretty widespread in the marsh region of the plot 
ash <- subset(dw_select, SPCODE == "FRPE")
# there are mainly 1s and 2s, some 3s, a handful of 4s, and like a single 5s im pretty sure

ashplot <- ggplot(data = ash, aes(x = DC.2021)) + 
  geom_bar(position="stack", stat="count")

# BUILD PILOT DATA (Aug 2021)

# SPECIES 1: LITU
# what is decay class distribution (2021) for all LITUs 
dw_LITU <- dplyr::filter(dw_select, SPCODE %in% c("LITU"))

# distribution of decay classes 
ggplot(dw_LITU, aes(x=DC.2021)) + geom_histogram()

# SPECIES 2: LIST2
# what is decay class distribution (2021) for all QURU 
dw_LIST2 <- dplyr::filter(dw_select, SPCODE %in% c("LIST2"))

# distribution of decay classes 
ggplot(dw_LIST2, aes(x=DC.2021)) + geom_histogram()

# PILOT DATA GOAL: sample at least 3 replicates from DC 1, 3, 5
# for pilot data sampling - pull from LITU and LIST2

pilot <- rbind(dw_LITU, dw_LIST2)
# remove rows that have NA for DC.2021 - not helpful 
pilot <- pilot[complete.cases(pilot[ , 9]),]

#export .csv
write.csv(pilot,"/Users/abbeyyatsko/Desktop/repos/serc_deadwood/FINAL_DATA/pilot.csv", row.names = FALSE)


