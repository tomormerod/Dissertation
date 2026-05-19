
# Program Packages --------------------------------------------------------

#| echo: true
#| eval: false
library(tidyverse) 
library(sf) 
library(here) 
# install.packages("tidymodels") 
library(tidymodels) 

# Data Read In ------------------------------------------------------------

la_data <- read_csv(here("master_data.csv"))
la_boundaries <- st_read(here("district_boundaries.geojson"))
lookup <- read_csv(here("Lookup_table.csv"))

# Test Combinations -------------------------------------------------------

dependent <- 'stock'
independents <- c('level_two','crime','black','bad_very_bad','flat_hmo','five_plus','single',
                  'single_parent','smoking','unemployed','inactive','children',
                 'adult_under_thirty','sixtyfive_plus') 

# Function to fit and store models for each combination of predictors
fit_models <- function(dependent, independents, la_data) {
  models <- list()
  index <- 1
  for (i in 1:length(independents)) {
    combs <- combn(independents, i)
    for (j in 1:ncol(combs)) {
      predictors <- combs[, j]
      formula <- as.formula(paste(dependent, "~", paste(predictors, collapse = " + ")))
      model <- lm(formula, data = la_data)
      models[[index]] <- model
      index <- index + 1
    }
  }
  return(models)
}

# Fit models for all combinations of predictors
all_models <- fit_models(dependent, independents, la_data)

# Extract model statistics for comparison
model_stats <- lapply(all_models, function(model) {
  summary_model <- summary(model)
  list(
    formula = as.character(formula(model)),
    Adjusted_R_squared = summary_model$adj.r.squared
  )
})

# Convert to data frame for easy viewing
model_stats_df <- do.call(rbind, lapply(model_stats, as.data.frame))
model_stats_ordered <- model_stats_df %>% arrange(desc(Adjusted_R_squared))
print(model_stats_ordered)

# Testing for Regional Term -----------------------------------------------

#To plot model and residuals for each region. 

#Create model and extract residuals

model <- la_data |>
  #select(c(level_two + crime + bad_very_bad + flat_hmo + single + single_parent + inactive + sixtyfive_plus + stock)) |>
  mutate(
    across(c(level_two, crime, bad_very_bad, flat_hmo, single, single_parent, inactive, sixtyfive_plus), ~(.x-mean(.x))/sd(.x)), type = "full_dataset"
  ) |>
  nest(data=-type) |>
  mutate(
    model=map(data,
              ~lm(stock ~ level_two + crime + bad_very_bad + flat_hmo + single + single_parent + inactive + sixtyfive_plus, data=.x)
    ),
    # augment() for predictions / residuals
    values=map(model, augment)
  )

#Generated permuted data 

permuted_data <- model |>
  mutate(
    resids=map(values, ~.x |>  select(.resid))
  ) |>
  select(-c(model, values)) |>
  unnest(cols=c(data,resids)) |>
  #As before, but showing (deviation from) the best fit but for LAs rather than variables
  select(local_authority, .resid) |>
  permutations(permute=c(local_authority), times=8, apparent=TRUE) |>
  # Randomly shuffle the residual values around the LAs and create 8 possibilities.
  mutate(data=map(splits, ~rsample::analysis(.))) |>
  select(id, data) |>
  # Turn these "splits" objects into dataframes. Each dataframe is then labelled according to an id 
  unnest(cols=data)
# Add residual values for each LA, as calculated from these dataframes, grouped according to which dataframe has done the calculation. 

# Store max value of residuals for setting limits in map colour scheme.
max_resid <- max(abs(permuted_data$.resid))
# Store vector of permutation IDs for shuffling facets in the plots.
ids <- permuted_data |> pull(id) |> unique()

la_regions <- la_boundaries |> 
  left_join(lookup |> select(LAD23NM, RGN23NM), by=join_by(LAD23NM == LAD23NM))|>
  group_by(RGN23NM,LAD23NM)|>
  unique()

la_regions |>
  select(LAD23NM, RGN23NM) |>
  right_join(permuted_data, by=c("LAD23NM"="local_authority")) |>
  #Add the residuals to each LA 
  mutate(id=factor(id, levels=sample(ids))) |>
  # Randomly change the order of the ids (aka dataframes)
  ggplot() +
  geom_sf(aes(fill=.resid), colour="#636363", linewidth=0.05)+
  #Second set of outlines details the regions rather than just cons.
  geom_sf(
    data=. %>% group_by(RGN23NM) %>% summarise(),
    colour="black", linewidth=0.5, fill="transparent"
  )+
  # Create the lineup of maps showing different randomly assigned residuals 
  facet_wrap(~id, ncol=3) +
  scale_fill_distiller(palette="RdBu", direction=1,
                       limits=c(-max_resid, max_resid), guide="none") 
