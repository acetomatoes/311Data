
#################################################
## NYC restaurant health grades in 2015
setwd("./Github/311Data")
library(tidyverse)
library(ggvis)
library(DT)
library(lubridate)

# Read in the data from a local Copy
rests <- read_csv("Datasets/NYCRest2015.csv")

### Do some basic cleaning on the data to get everything in a useable format ###

# Standardize the column names
names(rests) <- make.names(names(rests))

rests$LAT <- as.numeric(rests$LAT) # convert to numeric for mapping purposes
rests$GRADE.DATE <- mdy(rests$GRADE.DATE) # Convert to date format using mdy() from lubridate

# Take a look and see if we want to convert anything else
str(rests)

### End cleaning (for now) ###


### Begin analysis using dplyr ###

# Have a look at the distribution of grades by zip code
rests %>%
  group_by(GRADE, ZIPCODE) %>%
  summarise(grades = n(), grade_prop =  n()/grades)%>%
  arrange(desc(grades))
  
# Pull out the top performing zips, those with the most A grades
gradesbyzip <- rests %>%
  group_by(ZIPCODE) %>%
  summarise(Restaurants = n(),
            A_percent = paste0(round(sum(GRADE == "A")/n(),2)*100,"%"),
            B_percent = paste0(round(sum(GRADE == "B")/n(),2)*100,"%"),
            C_percent = paste0(round(sum(GRADE == "C")/n(),2)*100,"%"),
            P_percent = paste0(round(sum(GRADE == "P")/n(),2)*100,"%"),
            Z_percent = paste0(round(sum(GRADE == "Z")/n(),2)*100,"%")) %>%
  arrange(desc(A_percent))

# Put the results into a datatable
datatable(gradesbyzip, 
          filter = 'bottom',
          caption = 'Table 1: Percentages of each health grade by zip codes.')


# Create grades oobject, casting GRADE as a factor for charting
grades <- rests %>% mutate(grade = factor(GRADE))

# Create a bar chart of the count by each grade
grades %>%
        ggvis(~grade)%>%
        layer_bars(fill = ~factor(grade))



# Create the classNames function to get the class names of each factor for the tooltips
classNames <- function(x) {
        if(is.null(x)) return(NULL)
        paste0(x[1], collapse = "<br />")
}       


# Let's create an interactive chart using the ggvis package
rests %>%
        group_by(GRADE) %>%
        ggvis(~GRADE, fill = ~GRADE)%>%
        layer_bars()%>%
        scale_nominal("fill", range = c("#2850AD", # set the colors to the official NYC palette
                                        "#00933C", 
                                        "#FF6319", 
                                        "#676867")) %>%
        add_axis("y", title = "Number of Restaurants",
                 title_offset = 60)%>%
        add_axis("x", orient = "top", ticks = 0, title = "Distribution of Health Grade",
                 properties = axis_props(
                         axis = list(stroke = "white"),
                         labels = list(fontSize = 0)))%>%
        add_axis("x", title = "Grade")%>%
        add_tooltip(classNames, "hover")

### End analysis ###
