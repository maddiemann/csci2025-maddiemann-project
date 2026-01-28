library(tidyverse)
library(factoextra)
library(scales)
library(ISLR2)
library(tidyclust)
library(tidymodels)
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




breast_cancer_data_rec <- recipe(~ ., data = breast_cancer_data) |> 
  update_role(id, diagnosis, new_role = "diagnosis") |> 
  step_normalize(all_numeric_predictors()) |> 
  step_pca(all_numeric_predictors()) |> 
  prep()

breast_cancer_data_pca <- bake(breast_cancer_data_rec, new_data = NULL)

bcd_pca_summary <- summary(breast_cancer_data_pca)
bca_importance <- bcd_pca_summary$importance

bcd_pca_variance <- summary(breast_cancer_data_pca) |>
  as.data.frame() |> 
  pivot_longer(everything(), 
  names_to = "PC",
  values_to = "values") |> 
  filter(row_number() <= 30)

ggplot(breast_cancer_data_pca, aes(x = PC1, y = PC2, color = diagnosis)) +
  geom_point()

breast_cancer_data_pca |> 
  ggplot(aes(x = breast_cancer_data_pca))+
  geom_histogram()











