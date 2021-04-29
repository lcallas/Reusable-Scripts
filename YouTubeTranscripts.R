## ---------Downloading YouTube Captions------------####
## Project: No specific project
## Author: Lisa Callas
## Email: lisa.callas@edmonton.ca
## Date Created: 2021-04-29
## Description: Code to download the captions from a YouTube video
## Notes:
## -------------------###


## -----Load Packages--------------####

library(youtubecaption)
library(stringr)
library(tidyverse)

##--------end packages-----###


url <- "https://www.youtube.com/watch?v=74G38tA85B0" #link to video
captions <- get_caption(url)

captions %>% 
  filter(segment_id>2000& segment_id<2428)->cap1 #slice out segment for transcript

transcript <-paste(str_to_sentence(cap1$text), sep="", collapse = "") #paste all segments into a string


write_file(transcript, "C:\\Users\\liscal\\Documents\\EnviroStratAudit\\transcript.txt") #export to a txtfile
