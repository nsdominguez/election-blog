#' @title GOV 1347: Introductory Blog Post/Laboratory Session
#' @author Matthew E. Dardet
#' @date August 29, 2024

####----------------------------------------------------------#
#### Preamble
####----------------------------------------------------------#

# Load libraries.
## install via `install.packages("name")`
library(ggplot2)
library(maps)
library(tidyverse)

## set working directory here
setwd("~/Documents/GOV 1347/GOV 1347/content/post/2024-09-05-prediction-1")

####----------------------------------------------------------#
#### Read and clean presidential popular vote.
####----------------------------------------------------------#

# Read presidential popular vote. 
d_popvote <- read_csv("popvote_1948-2020.csv")

# Subset data to most recent past election year. 
d_popvote |> 
  filter(year == 2020) |> 
  select(party, candidate, pv2p)

# Pivot data to wide format with party names as columns and two-party vote share as values.
(d_popvote_wide <- d_popvote |>
    select(year, party, pv2p) |>
    pivot_wider(names_from = party, values_from = pv2p))

# Modify winner column to show "D" if Democrats win and "R" if Republicans win. 
(d_popvote_wide <- d_popvote_wide |> 
    mutate(winner = case_when(democrat > republican ~ "D",
                              TRUE ~ "R")))

# Summarize data with respect to winners. 
d_popvote_wide |> 
  group_by(winner) |>
  summarise(races = n())

####----------------------------------------------------------#
#### Visualize trends in national presidential popular vote. 
####----------------------------------------------------------#

# Visualize the two-party presidential popular over time. 
d_popvote |> 
  ggplot(aes(x = year, y = pv2p, color = party)) + 
  geom_line() + 
  scale_color_manual(values = c("dodgerblue4", "firebrick1")) + 
  theme_bw()

my_custom_theme <- 
  theme_bw() + 
  theme(panel.border = element_blank(),
        plot.title = element_text(size = 15, hjust = 0.5), 
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 18),
        axis.line = element_line(colour = "black"),
        legend.position = "top",
        legend.text = element_text(size = 12))

d_popvote |> 
  ggplot(aes(x = year, y = pv2p, color = party)) + 
  geom_line() + 
  scale_color_manual(values = c("dodgerblue4", "firebrick1")) + 
  my_custom_theme

####----------------------------------------------------------#
#### State-by-state map of presidential popular votes.
####----------------------------------------------------------#

# Sequester shapefile of states from `maps` library.
states_map <- map_data("state")

# Read wide version of dataset that can be used to compare candidate votes with one another. 
d_pvstate_wide <- read_csv("clean_wide_state_2pv_1948_2020.csv")

# Merge d_pvstate_wide with state_map.
d_pvstate_wide$region <- tolower(d_pvstate_wide$state)

pv_map <- d_pvstate_wide |>
  filter(year == 2020) |>
  left_join(states_map, by = "region")

# Make map grid of state winners for each election year available in the dataset. 
pv_win_map <- pv_map |> 
  mutate(winner = ifelse(R_pv2p > D_pv2p, "republican", "democrat"))
  
pv_win_map |> 
  ggplot(aes(long, lat, group = group)) + 
  geom_polygon(aes(fill = winner)) + 
  scale_fill_manual(values = c("dodgerblue4", "firebrick1")) + 
  theme_void()

d_pvstate_wide |> 
  filter(year >= 1980) |> 
  left_join(states_map, by = "region") |> 
  mutate(winner = ifelse(R_pv2p > D_pv2p, "republican", "democrat")) |> 
  ggplot(aes(long, lat, group = group)) +
  facet_wrap(facets = year ~.) + 
  geom_polygon(aes(fill = winner), color = "white") + 
  scale_fill_manual(values = c("dodgerblue4", "firebrick1")) + 
  theme_void() + 
  ggtitle("Presidential Vote Share (1980-2020)") + 
  theme(strip.text = element_text(size = 12), 
        aspect.ratio = 1)
  
####----------------------------------------------------------#
#### Forecast: simplified electoral cycle model. 
####----------------------------------------------------------#

# Create prediction (pv2p and margin) based on simplified electoral cycle model: 
# vote_2024 = 3/4*vote_2020 + 1/4*vote_2016 (lag1, lag2, respectively). 
pv2p_2024_states <- d_pvstate_wide |> 
  filter(year == 2020) |> 
  group_by(state) |> 
  summarize(R_pv2p_2024 = 0.75*R_pv2p + 0.25*R_pv2p_lag1, 
            D_pv2p_2024 = 0.75*D_pv2p + 0.25*D_pv2p_lag1) |> 
  mutate(pv2p_2024_margin = R_pv2p_2024 - D_pv2p_2024, 
         winner = ifelse(R_pv2p_2024 > D_pv2p_2024, "R", "D"), 
         region = tolower(state))

# Plot the margin of victory in a U.S. state map.
pv2p_2024_states |> 
  left_join(states_map, by = "region") |> 
  ggplot(aes(long, lat, group = group)) + 
  geom_polygon(aes(fill = pv2p_2024_margin), color = "black") + 
  scale_fill_gradient2(high = "firebrick1", 
                       low = "dodgerblue4", 
                       mid = "white", 
                       name = "win margin", 
                       breaks = c(-50, -25, 0, 25, 50), 
                       limits = c(-50, 50)) + 
  theme_void()
 
# Generate projected state winners and merge with electoral college votes to make 
# summary of electoral college vote distributions. 
ec <- read_csv("ec_full.csv")

pv2p_2024_states <- pv2p_2024_states |> 
  mutate(year = 2024) |> 
  left_join(ec, by = c("state", "year"))

pv2p_2024_states |> 
  group_by(winner) |> 
  summarize(electoral_votes = sum(electors))

## Harris: 
## Trump: 




