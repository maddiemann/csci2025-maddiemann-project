library(tidyverse)


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

breast_cancer_data |> 
select("diagnosis", "texture_se", "area_se")


# SE = Standard Error 
head(breast_cancer_data)

breast_cancer_data |> 
  select("")

# I want to do which features are the most accurate for the ML to use 
# That way they only need to use some of the meausres not all of them 

breast_cancer_data |> 
  count(diagnosis) |> 
  ggplot(aes(x = diagnosis, y = n)) + 
  geom_col()

breast_cancer_data |> 
  ggplot(aes(x = area_se, y = perimeter_mean, color = diagnosis)) +
  geom_point(alpha = 0.5)

