library(tidyverse)
library(here)
library(palmerpenguins)
library(janitor)
library(here)
library(dplyr)
library(ggplot2)

#gets functions from different file
source(here("functions","cleaning.R"))

penguins_raw<-read.csv(here("data","penguins.raw.csv"))
colnames(penguins_raw)


#Shorten species funct is from our own files, the others are from libraries
cleaning_penguin_columns <-function(raw_data){
  print("Cleaned names, removed comments, removed empty row and cols, removed delta")
  raw_data %>%
    clean_names() %>%
    shorten_species() %>%
    remove_empty(c("rows","cols")) %>%
    select(-starts_with("delta"))
}

colnames(penguins_raw)
penguins_clean<-cleaning_penguin_columns(penguins_raw)
colnames(penguins_clean)


#Renv---------------------
#initialise Renv
install.packages("renv")

#creates folder called renv in your folder - renv::init()

#renv::snapshot() says which libraries we've installed and put them in renv folder

#so when we install a new package, 

#so someone else using code can do "renv::restore" without manually installing all the packages I used
#everytime you load R, it auto loads the packages I use

#If there's already renv in folder then don't need init
renv::init()
#this tells you to select something from menu, write 1. Takes a screenshot of 
#the packages you have at the moment and keeps everything up to date so that packages installed is kept track but renv
renv::snapshot()
renv::install("table1")
renv::restore()

renv::diagnostics()


#####Y3 MT W4 Reproducible figures######------------------------------------------

shorten_species <- function(penguins_data) {
  penguins_data %>%
    mutate(species = case_when(
      species == "Adelie Penguin (Pygoscelis adeliae)" ~ "Adelie",
      species == "Chinstrap penguin (Pygoscelis antarctica)" ~ "Chinstrap",
      species == "Gentoo penguin (Pygoscelis papua)" ~ "Gentoo"
    ))
}

#Making a boxplot (exploratory figure, shows raw data without any models on top e.g trend line)
#It's good as you can visualise data, 

colnames(penguins_raw)

species_colours <- c("Adelie" = "darkorange",
                     "Chinstrap" = "purple",
                     "Gentoo" = "cyan4")


flipper_boxplot <- ggplot(
  data = penguins_flippers, 
  aes(x = Species, y = Flipper.Length..mm.)
) +
  geom_boxplot(aes(color = Species), width = 0.3, show.legend = FALSE) +  
  geom_jitter(aes(color = Species),
              alpha = 0.3,
              show.legend = FALSE,
              position = position_jitter(
                width = 0.2,
                seed = 0)) +
  scale_color_manual(values = species_colours) +
  labs(x = "Penguin species",
       y = "Flipper length (mm)") +
  theme_bw()
  
flipper_boxplot


#subset columns species and flipper length mm

colnames(penguins_raw)

penguins_flippers <- select(penguins_raw, c("Species", "Flipper.Length..mm."))

colnames(penguins_flippers)                      

#Can use pipes to get rid of NaN values instead of the above option and subsets spp and flipper length data

penguins_flippers <- penguins_raw %>%
  select(Species,Flipper.Length..mm.) %>%
  drop_na()

#I then went back to above bit and rewrote penguins raw with penguins flippers to get rid of error message when we load plot

#I then added instructions to change aesthetics and legend details and put data on top inside the geom_boxplot() funct

#Can turn the ggplot bit into a function so we can reuse it to make another similar graph e.g in this case boxplot

source(here)
source(here("functions","plotting.R"))

data<-data %>%
  drop_na(Flipper.Length..mm.)

git init

git remote add origin https://github.com/Harveet/Penguin_Project.git


