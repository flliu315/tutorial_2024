# --------------------------------------------
# Script Name: machine learning for geodata 
# Purpose: This section introduces how to model geodata 
#                using machine learning algorithms.
#
# Author:     Fanglin Liu
# Email:      flliu315@163.com
# Date:       2024-03-18
#
# --------------------------------------------
cat("\014") # Clears the console
rm(list = ls()) # Remove all variables

#######################################################
# 01-load data as data frame and define some args
## https://blasbenito.github.io/spatialRF/

# A) merging coordinates with env-spe
library(sf)
DES <- st_read("data/SPdata/doubs_env_spe.shp")
DES_df <- sf::st_drop_geometry(DES)
pts <- st_read("data/SPdata/doubs_point_geo.shp")
pts_df <- st_drop_geometry(pts)
DES_pts <- cbind(pts_df, DES_df[,-1]) |>
  dplyr::relocate(Y, .after = X) |>
  dplyr::rename(x=X, y=Y)

# B) naming the target and features (predictors) 
names(DES_pts)
dependent.variable.name <- colnames(DES_pts)[18]
predictor.variable.names <- colnames(DES_pts)[4:17]

# C) defining several arguments 

xy <- DES_pts[, c("x", "y")] # coordinates

distance.matrix <- as.matrix(dist(DES_pts)) #distance matrix
min(distance.matrix) # based on min and max to set thresholds
max(distance.matrix)
distance.thresholds <- c(0, 2000, 4000, 8000, 10000)

# random seed for reproducibility
random.seed <- 1

# 02- exploratory spatial data analysis
# note: spatial autocorrelation, interactions among features
#       and collinearity among features

# A) visualizing target and features on a map

doubs_river <- sf::st_read("data/SPdata/doubs_river.shp")

ggplot2::ggplot() +
  ggplot2::geom_sf(
    data = doubs_river, 
    fill = "white"
  ) + # Le Doubs river
  ggplot2::geom_point(
    data = DES_pts,
    ggplot2::aes(
      x = x,
      y = y,
      color = spe_abund
    ),
    size = 2.5
  ) + # sampling sites
  ggplot2::scale_color_viridis_c(
    direction = -1, 
    option = "F"
  ) +
  ggplot2::theme_bw() +
  ggplot2::labs(color = "spe_abund") +
  ggplot2::ggtitle("fish abund of Le Doubs") + 
  ggplot2::xlab("Longitude") + 
  ggplot2::ylab("Latitude")

# checking the relationship between target and features
spatialRF::plot_training_df(
  data = DES_pts,
  dependent.variable.name = dependent.variable.name,
  predictor.variable.names = predictor.variable.names,
  ncol = 3,
  point.color = viridis::viridis(100, option = "F"),
  line.color = "gray30"
)

# B) assessing spatial autocorrelation of target and features

spatialRF::plot_training_df_moran(
  data = DES_pts,
  dependent.variable.name = dependent.variable.name,
  predictor.variable.names = predictor.variable.names,
  distance.matrix = distance.matrix,
  distance.thresholds = distance.thresholds,
  fill.color = viridis::viridis(
    100,
    option = "F",
    direction = -1
  ),
  point.color = "gray40"
)

# C) reducing multicollinearity among features

preference.order <- c(
  "ele",
  "slo",
  "oxy",
  "bod"
)

# automatically selecting features
predictor.variable.names <- spatialRF::auto_cor(
  x = DES_pts[, predictor.variable.names],
  cor.threshold = 0.6,
  preference.order = preference.order
) |> # correlation 
  spatialRF::auto_vif(
    vif.threshold = 2.5,
    preference.order = preference.order
  ) # variance inflation factor

names(predictor.variable.names)
predictor.variable.names$selected.variables

# D) finding possible interactions as new features

interactions <- spatialRF::the_feature_engineer(
  data = DES_pts,
  dependent.variable.name = dependent.variable.name,
  predictor.variable.names = predictor.variable.names,
  xy = xy,
  importance.threshold = 0.50, # 50% best features
  cor.threshold = 0.6, # also used for collinearity above
  seed = random.seed,
  repetitions = 100,
  verbose = TRUE
)

kableExtra::kbl(
  head(interactions$screening, 10),
  format = "html"
) %>%
  kableExtra::kable_paper("hover", full_width = F)

# adding interaction columns to training data
DES_pts <- interactions$data

#  updating predictor.variable.names
predictor.variable.names <- interactions$predictor.variable.names

# 03-Fitting a non-spatial Random Forest model

model.non.spatial <- spatialRF::rf(
  data = DES_pts,
  dependent.variable.name = dependent.variable.name,
  predictor.variable.names = predictor.variable.names,
  distance.matrix = distance.matrix,
  distance.thresholds = distance.thresholds,
  xy = xy, # not needed by rf, but by other functions from the model
  seed = random.seed,
  verbose = FALSE
)

spatialRF::plot_residuals_diagnostics(
  model.non.spatial,
  verbose = FALSE
)

# global variable importance

spatialRF::plot_importance(
  model.non.spatial,
  verbose = FALSE
)

importance.df <- randomForestExplainer::measure_importance(
  model.non.spatial,
  measures = c("mean_min_depth", "no_of_nodes", "times_a_root", "p_value")
)

kableExtra::kbl(
  importance.df %>% 
    dplyr::arrange(mean_min_depth) %>% 
    dplyr::mutate(p_value = round(p_value, 4)),
  format = "html"
) %>%
  kableExtra::kable_paper("hover", full_width = F)

# Contribution of predictors to model transferability

model.non.spatial <- spatialRF::rf_importance(
  model = model.non.spatial
)

names(model.non.spatial$importance)

model.non.spatial$importance$per.variable %>% 
  ggplot2::ggplot() +
  ggplot2::aes(
    x = importance.oob,
    y = importance.cv
  ) + 
  ggplot2::geom_point(size = 3) + 
  ggplot2::theme_bw() +
  ggplot2::xlab("Importance (out-of-bag)") + 
  ggplot2::ylab("Contribution to transferability") + 
  ggplot2::geom_smooth(method = "lm", formula = y ~ x, color = "red4")

## Local variable importance

local.importance <- spatialRF::get_importance_local(model.non.spatial)
kableExtra::kbl(
  round(local.importance[1:10, 1:5], 0),
  format = "html"
) %>%
  kableExtra::kable_paper("hover", full_width = F)

#adding coordinates
local.importance <- cbind(
  xy,
  local.importance
)

#colors
color.low <- viridis::viridis(
  3,
  option = "F"
)[2]

color.high <- viridis::viridis(
  3,
  option = "F"
)[1]

#plot of ele
p1 <- ggplot2::ggplot() +
  ggplot2::geom_sf(
    data = doubs_river,
    fill = "white"
  ) +
  ggplot2::geom_point(
    data = local.importance,
    ggplot2::aes(
      x = x,
      y = y,
      color = ele
    )
  ) +
  ggplot2::scale_color_gradient2(
    low = color.low, 
    high = color.high
  ) +
  ggplot2::theme_bw() +
  ggplot2::theme(legend.position = "bottom") + 
  ggplot2::ggtitle("ele") +
  ggplot2::theme(
    plot.title = ggplot2::element_text(hjust = 0.5),
    legend.key.width = ggplot2::unit(1,"cm")
  ) + 
  ggplot2::labs(color = "Importance") + 
  ggplot2::xlab("Longitude") + 
  ggplot2::ylab("Latitude")

p2 <- ggplot2::ggplot() +
  ggplot2::geom_sf(
    data = doubs_river,
    fill = "white"
  ) +
  ggplot2::geom_point(
    data = local.importance,
    ggplot2::aes(
      x = x,
      y = y,
      color = slo
    )
  ) +
  ggplot2::scale_color_gradient2(
    low = color.low, 
    high = color.high
  ) +
  ggplot2::theme_bw() +
  ggplot2::theme(legend.position = "bottom") +
  ggplot2::ggtitle("slo") +
  ggplot2::theme(
    plot.title = ggplot2::element_text(hjust = 0.5),
    legend.key.width = ggplot2::unit(1,"cm")
  ) + 
  ggplot2::labs(color = "Importance") + 
  ggplot2::xlab("Longitude") + 
  ggplot2::ylab("Latitude")

p1 + p2

# Response curves and surfaces
spatialRF::plot_response_curves(
  model.non.spatial,
  quantiles = c(0.1, 0.5, 0.9),
  line.color = viridis::viridis(
    3, #same number of colors as quantiles
    option = "F", 
    end = 0.9
  ),
  ncol = 3,
  show.data = TRUE
)

spatialRF::plot_response_curves(
  model.non.spatial,
  quantiles = 0.5,
  ncol = 3
)

pdp::partial(
  model.non.spatial, 
  train = DES_pts, 
  pred.var = "ele", 
  plot = TRUE, 
  grid.resolution = 200
)

reponse.curves.df <- spatialRF::get_response_curves(model.non.spatial)

kableExtra::kbl(
  head(reponse.curves.df, n = 10),
  format = "html"
) %>%
  kableExtra::kable_paper("hover", full_width = F)

spatialRF::plot_response_surface(
  model.non.spatial,
  a = "ele",
  b = "slo"
)

pdp::partial(
  model.non.spatial, 
  train = DES_pts, 
  pred.var = c("ele", "slo"), 
  plot = TRUE
)

# model performance and Spatial cross-validation

spatialRF::print_performance(model.non.spatial)

model.non.spatial <- spatialRF::rf_evaluate(
  model = model.non.spatial,
  xy = xy,                  #data coordinates
  repetitions = 30,         #number of spatial folds
  training.fraction = 0.75, #training data fraction on each fold
  metrics = "r.squared",
  seed = random.seed,
  verbose = FALSE
)

names(model.non.spatial$evaluation)
spatialRF::print_evaluation(model.non.spatial)

# 04-Fitting a spatial model 
model.spatial <- spatialRF::rf_spatial(
  model = model.non.spatial,
  method = "mem.moran.sequential", #default method
  verbose = FALSE,
  seed = random.seed
)

class(model.spatial)

# A) building a spatial model
dependent_variable_name <- colnames(DES_pts)[18]
predictor_variable_names <- colnames(DES_pts)[4:17]
xy <- DES_pts[, c("x", "y")] # coordinates
distance_matrix <- as.matrix(dist(DES_pts)) #distance matrix
distance_thresholds <- c(0, 2000, 4000, 8000, 10000)

library(spatialRF)
spatial.model <- rf_spatial(
  DES_pts,
  dependent.variable.name = dependent_variable_name,
  predictor.variable.names = predictor_variable_names,
  distance.matrix = distance_matrix,
  distance.thresholds = distance_thresholds,
  n.cores = 1,
  method = "mem.moran.sequential"
 )
class(spatial.model)
#getting data frame with the selected spatial predictors
spatial.predictors <- get_spatial_predictors(spatial.model)
head(spatial.predictors)

# B) determining if SA reduction by spatial model
spatialRF::plot_moran( # Moran's I
  model.non.spatial, # non-spatial
  verbose = FALSE
)

spatialRF::plot_moran(
  model.spatial, # spatial
  verbose = FALSE
)

# C) reordering the importance of features

if(interactive()){
  #loading example data
  data(distance_matrix)
  data(plant_richness_df)
  #fittind spatial model
  model <- rf_spatial(
    data = plant_richness_df,
    dependent.variable.name = "richness_species_vascular",
    predictor.variable.names = colnames(plant_richness_df)[5:21],
    distance.matrix = distance_matrix,
    distance.thresholds = c(0, 1000),
    n.cores = 1,
    method = "mem.moran.sequential"
  )
  #getting data frame with the selected spatial predictors
  spatial.predictors <- get_spatial_predictors(model)
  head(spatial.predictors)
}
  
 class(model)
 
spatialRF::plot_importance(
  model.spatial,
  verbose = FALSE) + 
  ggplot2::ggtitle("Spatial model")

spatial.predictors <- spatialRF::get_spatial_predictors(model.spatial)
pr <- data.frame(spatial.predictors, DES_pts[, c("x", "y")])

p1 <- ggplot2::ggplot() +
  ggplot2::geom_sf(data = doubs_river, fill = "white") +
  ggplot2::geom_point(
    data = pr,
    ggplot2::aes(
      x = x,
      y = y,
      color = spatial_predictor_0_2
    ),
    size = 2.5
  ) +
  ggplot2::scale_color_viridis_c(option = "F") +
  ggplot2::theme_bw() +
  ggplot2::labs(color = "Eigenvalue") +
  ggplot2::scale_x_continuous(limits = c(-170, -30)) +
  ggplot2::scale_y_continuous(limits = c(-58, 80))  +
  ggplot2::ggtitle("Variable: spatial_predictor_0_2") + 
  ggplot2::theme(legend.position = "bottom")+ 
  ggplot2::xlab("Longitude") + 
  ggplot2::ylab("Latitude")

p2 <- ggplot2::ggplot() +
  ggplot2::geom_sf(data = world, fill = "white") +
  ggplot2::geom_point(
    data = pr,
    ggplot2::aes(
      x = x,
      y = y,
      color = spatial_predictor_0_5,
    ),
    size = 2.5
  ) +
  ggplot2::scale_color_viridis_c(option = "F") +
  ggplot2::theme_bw() +
  ggplot2::labs(color = "Eigenvalue") +
  ggplot2::scale_x_continuous(limits = c(-170, -30)) +
  ggplot2::scale_y_continuous(limits = c(-58, 80))  +
  ggplot2::ggtitle("Variable: spatial_predictor_0_5") + 
  ggplot2::theme(legend.position = "bottom") + 
  ggplot2::xlab("Longitude") + 
  ggplot2::ylab("")

p1 | p2