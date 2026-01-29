library(tidyverse)
library(ggradar)
library(ggplot2)
library(devtools)
library(scales)
library(patchwork)

column_names <- c(
  "id", "diagnosis",
  "radius_mean", "texture_mean", "perimeter_mean", "area_mean", "smoothness_mean", 
  "compactness_mean", "concavity_mean", "concave_points_mean", "symmetry_mean", "fractal_dimension_mean",
  "radius_se", "texture_se", "perimeter_se", "area_se", "smoothness_se", 
  "compactness_se", "concavity_se", "concave_points_se", "symmetry_se", "fractal_dimension_se",
  "radius_worst", "texture_worst", "perimeter_worst", "area_worst", "smoothness_worst", 
  "compactness_worst", "concavity_worst", "concave_points_worst", "symmetry_worst", "fractal_dimension_worst"
)

wpbc_column_names <- c(
  "id", "outcome", "time", "radius_mean", "texture_mean", "perimeter_mean", "area_mean", "smoothness_mean", 
  "compactness_mean", "concavity_mean", "concave_points_mean", "symmetry_mean", "fractal_dimension_mean",
  "radius_se", "texture_se", "perimeter_se", "area_se", "smoothness_se", 
  "compactness_se", "concavity_se", "concave_points_se", "symmetry_se", "fractal_dimension_se",
  "radius_worst", "texture_worst", "perimeter_worst", "area_worst", "smoothness_worst", 
  "compactness_worst", "concavity_worst", "concave_points_worst", "symmetry_worst", "fractal_dimension_worst", "tumor_size", "lymph_node_status"
)

breast_cancer_data <- read_csv("Data/wdbc.data", col_names = column_names)
wpbc_data <- read_csv("Data/wpbc.data", col_names = wpbc_column_names)


wpbc_data |> 
  ggplot(aes(x = tumor_size, y = area_mean, color = diagnosis)) + 
  geom_point()


combined_data <- breast_cancer_data |> 
  left_join(wpbc_data) |> 
  drop_na(outcome)




combined_data |> 
  ggplot(aes(x = tumor_size, y = area_mean, color = outcome)) + 
  geom_point()

combined_data |> 
  ggplot(aes(x = outcome, y = area_worst))+
  geom_jitter(width = 0.2)




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







options(timeout = 600)

remotes::install_github("ricardo-bion/ggradar", 
                        build_vignettes = FALSE, 
                        upgrade = "never")


renv::install("ricardo-bion/ggradar")


  
radar_se <- breast_cancer_data |> 
  select(c(diagnosis, contains("_se"))) |> 
  mutate(across(where(is.numeric), rescale)) |> 


group_by(diagnosis) |> 
  summarise(across(everything(), .fns = mean))
 

se_plot <- ggradar(radar_se,
        values.rad = c("0", "0.5", "1"),
        group.line.width = 1,
        group.point.size = 3,
        legend.position = "bottom")
      


radar_worst <- breast_cancer_data |> 
  select(c(diagnosis, contains("_worst"))) |> 
  mutate(across(where(is.numeric), rescale)) |> 

group_by(diagnosis) |> 
  summarise(across(everything(), .fns = mean))
 

worst_plot <- ggradar(radar_worst,
        values.rad = c("0", "0.5", "1"),
        group.line.width = 1,
        group.point.size = 3,
        legend.position = "bottom")

worst_plot
radar_mean <- breast_cancer_data |> 
  select(c(diagnosis, contains("_mean"))) |> 
  mutate(across(where(is.numeric), rescale)) |> 


group_by(diagnosis) |> 
  summarise(across(everything(), .fns = mean))
 

mean_plot <- ggradar(radar_mean,
        values.rad = c("0", "0.5", "1"),
        group.line.width = 1,
        group.point.size = 3,
        legend.position = "bottom")





radar_worst_explore <- breast_cancer_data |> 
  select(c(diagnosis, radius_worst, perimeter_worst, concave_points_worst, area_worst, texture_worst)) |> 
  mutate(across(where(is.numeric), rescale)) |> 

group_by(diagnosis) |> 
  summarise(across(everything(), .fns = mean))
 

worst_plot_explore <- ggradar(radar_worst_explore,
        values.rad = c("0", "0.5", "1"),
        axis.label.size = 2,
        group.line.width = 1,
        group.point.size = 3,
        legend.position = "bottom")

worst_plot_explore

radar_optimal <- breast_cancer_data |> 
  select(c(diagnosis, concave_points_mean, concavity_mean, perimeter_worst, radius_worst, area_worst)) |> 
  mutate(across(where(is.numeric), rescale)) |> 

group_by(diagnosis) |> 
  summarise(across(everything(), .fns = mean))
 

optimal_plot <- ggradar(radar_optimal,
        values.rad = c("0", "0.5", "1"),
        axis.label.size = 2,
        group.line.width = 1,
        group.point.size = 3,
        legend.position = "bottom")

optimal_plot



tumor_size_plot <- combined_data |> 
  pivot_longer(
    cols = c(concave_points_mean, concavity_mean, perimeter_worst, radius_worst, area_worst),
    names_to = "measurement",
    values_to = "value"
  )

ggplot(tumor_size_plot, aes(x = tumor_size, y = value)) +
  geom_point(alpha = 0.4, color = "red") +
  geom_smooth(method = "lm", color = "black", linetype = "dashed") +
  facet_wrap(~measurement, scales = "free_y", ncol = 1) +
  labs(title = "Correlation of Nuclei Measurements with Tumor Size",
       x = "Tumor Size (cm)",
       y = "Measurement Value")
