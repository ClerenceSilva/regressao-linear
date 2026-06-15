library(tidyverse)


base28 <- read_delim(
  "lancamentos-comerciais-por-distribuidoras 28",
  delim = ";",                      
  locale = locale(
    encoding  = "Latin1",          
    decimal_mark    = ",",
    grouping_mark   = "."
  ),
  show_col_types = FALSE
)

glimpse(base28)
