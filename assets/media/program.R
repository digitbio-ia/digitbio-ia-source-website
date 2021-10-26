library(tidyverse)
library(kableExtra)
# my_palette <- spec_color(1:5, end = 0.9, option = "A", direction = -1)
my_palette <- wesanderson::wes_palette("Darjeeling1", type = "discrete")
get_color <- function(vec){
  map_chr(vec, function(x) 
    if(x %in% c("Ouverture"))
      return(my_palette[1])
    else if(x %in% c("Étude de cas"))
      return(my_palette[2])
    else if(x %in% c("Cours", "Introduction"))
      return(my_palette[3])
    else if(x == "Discussion")
      return(my_palette[4])
    else
      return(my_palette[5]))
}
print_function <- function(df){
  df %>% 
    mutate(type = cell_spec(
      type, color = "white", bold = TRUE,
      background = get_color(type)
    )) %>% 
  kableExtra::kbl(format = "html", escape = FALSE,
             col.names = c("", "", "", "", "Orateur", "Titre")) %>%
  kableExtra::kable_classic("striped")
}

program <- 
  read.csv("program.csv", sep = ",", header = TRUE) %>% 
  mutate(link = paste("sequences", sequence, seance, sep = "/")) %>% 
  mutate(titre = paste0("<a href=", link,">", titre , "</a>")) %>% 
    dplyr::select(-link, -seance, -sequence) 

print_function(program) %>% knitr::render_markdown()

