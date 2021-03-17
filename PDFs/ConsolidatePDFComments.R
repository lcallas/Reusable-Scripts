###---Script Header---###
#Date: January 12, 2021
#Author: Lisa Callas

#Description: This script writes a function that takes a set of Summary of Comment pdfs that are created from making comments
#in a variety of .pdf documents, then merges and reformats all of the comments into a single spreadsheet 

#Instructions before use: 
#1. After adding comments to a .pdf, use the comments feature to create and save a new .pdf with just the comments.
#2. Save all Summary of Comments .pdfs into one folder that only contains them - nothing else.

###---------------------###


#Load Libraries
library(tidyverse)
library(pdftools)
library(openxlsx)


#---Function---#
#This function requires 2 arguments: x=the path to the folder that contains all of the pdfs
# and y= the path and filename with the .xlsx filetype for an output spreadsheet.
#example: ConsolidatedComments(x="C://MyDocuments/PdfFiles/", y="C://MyDocuments/consolidatedcomments.xlsx")

ConsolidatedComments <- function(x=importpath, y=exportpath){

file_names <- paste0(x,dir(x)) #creates a list of filenames with path for import

list_names <- dir(x) #creates a list of file names to identify imported files

my_list <- lapply(file_names, pdf_text)  #import files into a list

names(my_list) <- list_names #assign each list item its name from the imported document

my_list %>% 
  map_df(as_tibble, .id="Source_Document")->allSummaries #create a tibble from the list of lists

#cleaning data and separating into columns
allSummaries %>% 
  mutate(value=str_squish(value)) %>%
  mutate(value= str_extract(value, "Page:(?s)(.*$)")) %>% 
  separate(value, into = c("Page", "value"), sep="\\s(?=[^,\\s]*Author:)", extra="merge" ) %>% 
  separate(value, into = c("Author", "value"), sep="\\s(?=[^,\\s]*Subject:)", extra="merge" ) %>% 
  separate(value, into = c("Subject", "value"), sep="\\s(?=[^,\\s]*Date:)", extra="merge" ) %>% 
  separate(value, into = c("Date", "value"), sep="\\s(?=[^,\\s][[:alpha:]])", extra="merge" ) %>% 
  mutate(value=str_sub(value,4))->AS1

#identify the max number of individual comments in a combined text block for separating
n_vars <- AS1$value %>% str_count(pattern = "Author:") %>% max(na.rm = T) + 1

#separate a text block with multiple comments into individual comment rows
AS1 %>% 
  separate(value, into=c(head(LETTERS,n_vars)), sep="\\s(?=[^,\\s]*Author:)", extra="merge") %>% 
  pivot_longer(cols=c(head(LETTERS,n_vars)), names_to="Head", values_to= "Text" ) %>% 
  filter(!is.na(Text)) %>% 
  select(-Head)->AS2

#Subset into dataframes that had multiple comments that need cleaning and rows with single comments that are fine
needSeparate <- 
  AS2 %>% 
  filter(grepl("Author:",Text))

completed <- 
  AS2 %>% 
  filter(!grepl("Author:",Text))

#separate and clean subset that had multiple comments
needSeparate %>% 
  separate(Text, into = c("Author1", "Text"), sep="\\s(?=[^,\\s]*Subject:)", extra="merge" ) %>% 
  separate(Text, into = c("Subject1", "Text"), sep="\\s(?=[^,\\s]*Date:)", extra="merge" ) %>% 
  separate(Text, into = c("Date1", "Text"), sep="\\s(?=[^,\\s][[:alpha:]])", extra="merge" ) %>% 
  mutate(Author=Author1,
         Subject=Subject1,
         Date=Date1) %>% 
  select(-Author1, -Subject1, -Date1)->separated


CompleteList <- bind_rows(completed, separated) #combine cleaned tables into one

#remove unnecessary header information from column contents
CompleteList %>% 
  mutate(Page= str_extract(Page,"Page:\\s"),
         Author= str_remove(Author, "Author:\\s"),
         Subject= str_remove(Subject,"Subject:\\s"),
         Date= str_remove(Date, "Date:\\s"))->ListofComments

ListofComments$Page <- as.numeric(ListofComments$Page)#Change Page column from character to numeric

write.xlsx(ListofComments, y) #export a .xlsx file

}
  
#---End of Function---#



# Call Function for testing
# ConsolidatedComments(x = "C:/Users/liscal/Downloads/AcrobatSummaries/",
#       y= "C:/Users/liscal/Downloads/testfile.xlsx")
