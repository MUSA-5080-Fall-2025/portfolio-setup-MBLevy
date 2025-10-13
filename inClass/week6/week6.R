# Load packages and data
library(tidyverse)
library(sf)
library(here)

# Load Boston housing data
boston <- read_csv(here("inClass/week6/data/boston.csv"))

# Quick look at the data
glimpse(boston)

#Very basic model
baseline <- lm(formula = SalePrice ~ LivingArea, data = boston)
summary(baseline)

#make a better model
betterModel <- lm(SalePrice ~ LivingArea + R_FULL_BTH, data = boston)
summary(betterModel)

#compare the 2
cat("Baseline R²:", summary(baseline)$r.squared, "\n")
cat("With bathrooms R²:", summary(betterModel)$r.squared, "\n")


#convert csv to sf
bostonSF <- boston %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
  st_transform('ESRI:102286') #MA State Plane (ft)
head(bostonSF)

# Load neighborhood boundaries
neighBound <- read_sf(here("inClass/week6/data/BPDA_Neighborhood_Boundaries.geojson")) %>%
  st_transform('ESRI:102286')  # Match CRS!

# Check the neighborhoods
head(neighBound)

#Checking out quick map
ggplot() +
  geom_sf(data = neighBound) +
  geom_sf(data = bostonSF)

#join the two
bostonSF <- bostonSF %>%
  st_join(neighBound, join = st_intersects)


#Categorical/Dummy Var 
bostonSF <- bostonSF %>% mutate(
  name = as.factor(name)
)

modelNeigh <- lm(SalePrice ~ LivingArea + name, 
                          data = bostonSF)

# Show just first 10 coefficients
summary(modelNeigh)$coef[1:10, ]

#Theory: wealthier neighborhoods have higher costs per sq foot

# Define wealthy neighborhoods based on median prices
wealthy_hoods <- c("Back Bay", "Beacon Hill", "South End", "Bay Village")

# Create binary indicator
bostonSF <- bostonSF %>%
  mutate(
    wealthy_neighborhood = ifelse(name %in% wealthy_hoods, "Wealthy", "Not Wealthy"),
    wealthy_neighborhood = as.factor(wealthy_neighborhood)
  )

# Check the split
bostonSF %>%
  st_drop_geometry() %>%
  count(wealthy_neighborhood)


model_no_interact <- lm(SalePrice ~ LivingArea + wealthy_neighborhood, 
                        data = bostonSF)

summary(model_no_interact)$coef

model_interact <- lm(SalePrice ~ LivingArea * wealthy_neighborhood, 
                     data = bostonSF)

summary(model_interact)$coef

#compare the two
cat("Model WITHOUT interaction R²:", round(summary(model_no_interact)$r.squared, 4), "\n")
cat("Model WITH interaction R²:", round(summary(model_interact)$r.squared, 4), "\n")
cat("Improvement:", round(summary(model_interact)$r.squared - summary(model_no_interact)$r.squared, 4), "\n")



#Non linear

# Calculate age from year built
bostonSF <- bostonSF %>%
  mutate(Age = 2025 - YR_BUILT)%>% filter(Age <2000)


# Check the distribution of age
summary(bostonSF$Age)

# Simple linear relationship
model_age_linear <- lm(SalePrice ~ Age + LivingArea, 
                       data = bostonSF)

summary(model_age_linear)$coef

# Quadratic model (Age²)
model_age_quad <- lm(SalePrice ~ Age + I(Age^2) + LivingArea, 
                     data = bostonSF)

summary(model_age_quad)$coef

# R-squared comparison
r2_linear <- summary(model_age_linear)$r.squared
r2_quad <- summary(model_age_quad)$r.squared
cat("Linear model R²:", round(r2_linear, 4), "\n")
cat("Quadratic model R²:", round(r2_quad, 4), "\n")
cat("Improvement:", round(r2_quad - r2_linear, 4), "\n\n")
anova(model_age_linear, model_age_quad)

# Compare residual plots
par(mfrow = c(1, 2))

# Linear model residuals
plot(fitted(model_age_linear), residuals(model_age_linear),
     main = "Linear Model Residuals",
     xlab = "Fitted Values", ylab = "Residuals")
abline(h = 0, col = "red", lty = 2)

# Quadratic model residuals  
plot(fitted(model_age_quad), residuals(model_age_quad),
     main = "Quadratic Model Residuals",
     xlab = "Fitted Values", ylab = "Residuals")
abline(h = 0, col = "red", lty = 2)

#read crimes
crime <- read_csv("inClass/week6/data/bostonCrimes.csv")
head(crime)

crime <- crime %>% st_as_sf(coords = "Location", crs = 4326) %>%
  st_transform('ESRI:102286') #MA State Plane (ft)

# k-nearest neighborhoods methods
ist_matrix <- st_distance(bostonSF, crime)

# Function to get mean distance to k nearest neighbors
get_knn_distance <- function(dist_matrix, k) {
  apply(dist_matrix, 1, function(distances) {
    # Sort and take first k, then average
    mean(as.numeric(sort(distances)[1:k]))
  })
}

# Create multiple kNN features
bostonSF <- bostonSF %>%
  mutate(
    crime_nn1 = get_knn_distance(dist_matrix, k = 1),
    crime_nn3 = get_knn_distance(dist_matrix, k = 3),
    crime_nn5 = get_knn_distance(dist_matrix, k = 5)
  )

# Check results
summary(boston.sf %>% st_drop_geometry() %>% select(starts_with("crime_nn")))
