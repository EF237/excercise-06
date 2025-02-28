### challenge 1 ###

# step 1 - loading data

library(tidyverse)

f <- "https://raw.githubusercontent.com/difiore/ada-datasets/main/IMDB-movies.csv"
d <- read_csv(f, col_names = TRUE)

library(dplyr)

# step 2 - new dataset

d <- d |>
  arrange(startYear) |>
  filter(startYear >= 1920 & startYear <= 1979, na.rm = TRUE) |>
  filter(runtimeMinutes >= 60 & runtimeMinutes <=180, na.rm = TRUE) |>
  mutate(decades = paste0(substr(as.character(startYear), 3, 3), "0s"))

# step 3 - histogram

library(ggplot2)

p <- ggplot(data = d, aes( x = (runtimeMinutes))) +
  geom_histogram(fill = "pink", color = "pink", bins = 50) +
  facet_wrap(~decades, ncol = 3) +
  theme(legend.position = "none")

# step 4 - std dev

results <- d |>
  group_by(decades) |>
  summarise(rm_mean = mean(runtimeMinutes, na.rm = TRUE), 
            rm_sd = sd(runtimeMinutes, na.rm = TRUE))

# step 5 - slice_sample

sample <- d |>
  group_by(decades) |>                         
  slice_sample(n = 100, replace = FALSE) |>
  summarise(
    rm_mean = mean(runtimeMinutes, na.rm = TRUE),
    rm_sd = sd(runtimeMinutes, na.rm = TRUE)
  )

# step 6 - std error sample

sample_se <- sample |>
  mutate(rm_se = rm_sd/sqrt(100))

# step 7 - std error results

results_se <- results |>
  mutate(rm_se = rm_sd/sqrt(100))

combined <- results_se |>
  left_join(sample_se, by = "decades", suffix = c("_results", "_sample"))

comparison <- combined |> 
  mutate(
    mean_diff = abs(rm_mean_sample - rm_mean_results),
    se_diff = abs(rm_se_sample - rm_se_results)) |>
  select(decades, mean_diff, se_diff)


# step 8 - sampling distribution

library(dplyr)

sample_distribution <- d |>
  rep_slice_sample(n = 100, reps = 1000)

s_dist <- sample_distribution |>
  group_by(decades) |>
  summarise(
    rm_mean = mean(runtimeMinutes, na.rm = TRUE),
    rm_sd = sd(runtimeMinutes, na.rm = TRUE)
  )

# step 9 - histogram

s_dist_se <- s_dist |>
  mutate(rm_se = rm_sd/sqrt(100))

p2 <- ggplot(data = sample_distribution, aes( x = (runtimeMinutes))) +
  geom_histogram(fill = "blue", color = "blue", bins = 50) +
  facet_wrap(~decades, ncol = 3) +
  theme(legend.position = "none")


# step 10 - compare

combined2 <- s_dist_se |>
  left_join(sample_se, by = "decades", suffix = c("_dist", "_sample"))

comparison2 <- combined2 |> 
  mutate(
    mean_diff = abs(rm_mean_sample - rm_mean_dist),
    se_diff = abs(rm_se_sample - rm_se_dist)) |>
  select(decades, mean_diff, se_diff)

### challenge 2 ###

# step 1
r <- "https://raw.githubusercontent.com/difiore/ada-datasets/refs/heads/main/zombies.csv"
z <- read_csv(r, col_names = TRUE)

# step 2

head(z)
colnames(z)

# height, weight, age, number of zombies killed, and years of education

zombie.world <- z |>
  summarise(height_mean = mean(height, na.rm = TRUE), 
            height_sd = sd(height, na.rm = TRUE),
            weight_mean = mean(weight, na.rm = TRUE), 
            weight_sd = sd(weight, na.rm = TRUE),
            age_mean = mean(age, na.rm = TRUE), 
            age_sd = sd(age, na.rm = TRUE),
            zombies_killed_mean = mean(zombies_killed, na.rm = TRUE), 
            zombies_killed_sd = sd(zombies_killed, na.rm = TRUE),
            years_of_education_mean = mean(years_of_education, na.rm = TRUE), 
            years_of_education_sd = sd(years_of_education, na.rm = TRUE),
            )

# step 3
p3 <- ggplot(data = z, aes( x = (id))) +
  geom_histogram(fill = "blue", color = "blue", bins = 50) +
  facet_wrap(~decades, ncol = 3) +
  theme(legend.position = "none")

# step 4

# step 5

# step 6

# step 7

# step 8

# step 9

# step 10




p <- ggplot(data = d, aes(x = (decades), y = (runtimeMinutes))) +
  xlab("decades") +
  ylab("runtimeMinutes") +
  geom_point(na.rm = TRUE) +
  theme(legend.position = "none") +
  geom_boxplot()




