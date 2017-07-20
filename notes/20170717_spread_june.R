library(ggplot2)
library(readr)
library(tidyverse)
files <- Sys.glob('/data/nycdoe/June Biog/*.csv')

read_csv_with_year <- function(filename) {
  year <- as.numeric(gsub('-.*$', '', basename(filename)))
  df <- read_csv(filename, col_types = cols(student_id_scram = col_character(), grade_level = col_character()))
  df$year <- year
  df <- select(df, year, student_id_scram, dbn, grade_level, contains("date"))
}

setwd('/data/nycdoe/')
files <- Sys.glob('/data/nycdoe/June Biog/*.csv')
bios <- map_df(files, read_csv_with_year)

spread_bios <-
  bios %>% 
  group_by(student_id_scram) %>% 
  spread(year, grade_level)
