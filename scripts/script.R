library(tidyverse)
library(ggradar)
library(devtools)
library(scales)
# Define the column names based on the dataset documentation
column_names <- c(
  "id", "diagnosis",
  "radius_mean", "texture_mean", "perimeter_mean", "area_mean", "smoothness_mean", 
  "compactness_mean", "concavity_mean", "concave_points_mean", "symmetry_mean", "fractal_dimension_mean",
  "radius_se", "texture_se", "perimeter_se", "area_se", "smoothness_se", 
  "compactness_se", "concavity_se", "concave_points_se", "symmetry_se", "fractal_dimension_se",
  "radius_worst", "texture_worst", "perimeter_worst", "area_worst", "smoothness_worst", 
  "compactness_worst", "concavity_worst", "concave_points_worst", "symmetry_worst", "fractal_dimension_worst"
)

# Read the data
breast_cancer_data <- read_csv("Data/wdbc.data", col_names = column_names)

glimpse(breast_cancer_data)

breast_cancer_data |> 
select("diagnosis", "texture_se", "area_se")


# SE = Standard Error 
head(breast_cancer_data)



# I want to do which features are the most accurate for the ML to use 
# That way they only need to use some of the meausres not all of them 

breast_cancer_data |> 
  count(diagnosis) |> 
  ggplot(aes(x = diagnosis, y = n)) + 
  geom_col()

breast_cancer_data |> 
  ggplot(aes(x = area_se, y = perimeter_mean, color = diagnosis)) +
  geom_point(alpha = 0.5)

breast_cancer_data |> 
  select("diagnosis", "symmetry_se", "concavity_se", "compactness_se") 
  

breast_cancer_data |> 
  ggplot(aes(x = diagnosis)) + 
  geom_bar()

breast_cancer_data |> 
  ggplot(aes(x = perimeter_mean, y = area_mean)) +
  geom_point(alpha = 0.3, aes(color = diagnosis))+
  geom_smooth(method = "lm")

breast_cancer_data |> 
  ggplot(aes(x = perimeter_worst, y = area_worst, color = diagnosis))+
  geom_point(alpha = 0.3)

breast_cancer_data |> 
  ggplot(aes(x = perimeter_mean, y = area_mean)) +
  geom_point(alpha = 0.3, aes(color = diagnosis))

breast_cancer_data |> 
  group_by(diagnosis) |> 
  summarise(
    avg_radius_mean =  mean(radius_mean),
    avg_radius_worst = mean(radius_worst),
    avg_radius_se = mean(radius_se),
    avg_texture_mean = mean(texture_mean),
    avg_area_worst = mean(area_worst)
  )

library(devtools)

devtools::install_github("ricardo-bion/ggradar", dependencies = TRUE)



# Ensure you have the core dependencies first
install.packages(c("ggplot2", "dplyr", "scales", "tibble"))

# Increase timeout for GitHub downloads
options(timeout = 600)

# Try installing without building from source
remotes::install_github("ricardo-bion/ggradar", 
                        build_vignettes = FALSE, 
                        upgrade = "never")


renv::install("ricardo-bion/ggradar")


radar_mean <- breast_cancer_data |> 
  select(c(diagnosis, contains("_mean"))) |> 
  mutate(across(where(is.numeric), rescale)) |> 


group_by(diagnosis) |> 
  summarise(across(everything(), .fns = mean))
 

ggradar(radar_mean,
        values.rad = c("0", "0.5", "1"),
        group.line.width = 1,
        group.point.size = 3,
        legend.position = "bottom")
      
  
radar_se <- breast_cancer_data |> 
  select(c(diagnosis, contains("_se"))) |> 
  mutate(across(where(is.numeric), rescale)) |> 

# Median Values by vehicle Class 
group_by(diagnosis) |> 
  summarise(across(everything(), .fns = mean))
 

ggradar(radar_se,
        values.rad = c("0", "0.5", "1"),
        group.line.width = 1,
        group.point.size = 3,
        legend.position = "bottom")
      


radar_worst <- breast_cancer_data |> 
  select(c(diagnosis, contains("_worst"))) |> 
  mutate(across(where(is.numeric), rescale)) |> 

group_by(diagnosis) |> 
  summarise(across(everything(), .fns = mean))
 

ggradar(radar_worst,
        values.rad = c("0", "0.5", "1"),
        group.line.width = 1,
        group.point.size = 3,
        legend.position = "bottom")
      