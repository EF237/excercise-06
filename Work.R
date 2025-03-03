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

# step 8 - sampling distribution

library(infer)

sample_distribution <- d |>
  rep_slice_sample(n = 100, reps = 1000, replace = FALSE)

s_dist <- sample_distribution |>
  group_by(replicate, decades) |>
  summarise(
    rm_mean = mean(runtimeMinutes, na.rm = TRUE),
    rm_sd = sd(runtimeMinutes, na.rm = TRUE),
    .groups = "drop"
  )

# step 9 - histogram

s_dist_se <- s_dist |> 
  group_by(decades) |>
  mutate(rm_se = rm_sd/sqrt(100))

p2 <- ggplot(data = s_dist, aes( x = (rm_mean))) +
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

# step 1 - load
r <- "https://raw.githubusercontent.com/difiore/ada-datasets/refs/heads/main/zombies.csv"
z <- read_csv(r, col_names = TRUE)

# step 2 - mean/stdev

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

# step 3 - boxplot

library(ggplot2)
install.packages("gridExtra")
library(gridExtra)

p1 <- ggplot(z, aes(x = gender, y = weight, fill = gender)) +
  geom_boxplot(alpha = 1, outlier.shape = NA) +
  labs(title = "Weight by Gender", x = "Gender", y = "Weight") +
  theme(legend.position = "none")

p2 <- ggplot(z, aes(x = gender, y = height, fill = gender)) +
  geom_boxplot(alpha = 1, outlier.shape = NA) +
  labs(title = "Height by Gender", x = "Gender", y = "Height") +
  theme(legend.position = "none")

p3 <- ggplot(z, aes(x = gender, y = age, fill = gender)) +
  geom_boxplot(alpha = 1, outlier.shape = NA) +
  labs(title = "Age by Gender", x = "Gender", y = "Age") +
  theme(legend.position = "none")

p4 <- ggplot(z, aes(x = gender, y = years_of_education, fill = gender)) +
  geom_boxplot(alpha = 1, outlier.shape = NA) +
  labs(title = "Years of Education by Gender", x = "Gender", y = "Years of Education") +
  theme(legend.position = "none")

p5 <- ggplot(z, aes(x = gender, y = zombies_killed, fill = gender)) +
  geom_boxplot(alpha = 1, outlier.shape = NA) +
  labs(title = "Zombies Killed by Gender", x = "Gender", y = "Zombies Killed") +
  theme(legend.position = "none")

grid.arrange(p1, p2, p3, p4, p5, ncol = 2)

# step 4 - scatterplot

p1 <- ggplot(z, aes(x = age, y = height, color = gender)) +
  geom_point(alpha = 1, size = 1) + 
  labs(title = "Height vs. Age", x = "Age", y = "Height (cm)", color = "Gender") +
  scale_color_manual(values = c("pink", "blue"))

p2 <- ggplot(z, aes(x = age, y = weight, color = gender)) +
  geom_point(alpha = 1, size = 1) +
  labs(title = "Weight vs. Age", x = "Age", y = "Weight (kg)", color = "Gender") +
  scale_color_manual(values = c("pink", "blue"))  


grid.arrange(p1, p2, ncol = 2)


# step 5 -histogrm / qq
  # histogram

p1 <- ggplot(z, aes(x = weight)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "lightgreen", alpha = 1) +
  geom_density(color = "red", size = 1) +
  labs(title = "Histogram of Weight", x = "Weight", y = "Density")

p2 <- ggplot(z, aes(x = height)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "lightgreen", alpha = 1) +
  geom_density(color = "red", size = 1) +
  labs(title = "Histogram of Height", x = "Height", y = "Density")

p3 <- ggplot(z, aes(x = age)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "lightgreen", alpha = 1) +
  geom_density(color = "red", size = 1) +
  labs(title = "Histogram of Age", x = "Age", y = "Density")

p4 <- ggplot(z, aes(x = years_of_education)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "lightgreen", alpha = 1) +
  geom_density(color = "red", size = 1) +
  labs(title = "Histogram of Years Of Education", x = "Years Of Education", y = "Density")

p5 <- ggplot(z, aes(x = zombies_killed)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "lightgreen", alpha = 1) +
  geom_density(color = "red", size = 1) +
  labs(title = "Histogram of Zombies Killed", x = "Zombies Killed", y = "Density")

grid.arrange(p1, p2, p3, p4, p5, ncol = 2)

  # qq-plot

q1 <- ggplot(z, aes(sample = weight)) +  
  stat_qq(size = 2, alpha = 1) +    
  stat_qq_line(color = "red", size = 1) +  
  labs(title = "Q-Q Plot of Weight", x = "Theoretical Quantiles", y = "Sample Quantiles")

q2 <- ggplot(z, aes(sample = height)) +  
  stat_qq(size = 2, alpha = 1) +    
  stat_qq_line(color = "red", size = 1) +  
  labs(title = "Q-Q Plot of Height", x = "Theoretical Quantiles", y = "Sample Quantiles")

q3 <- ggplot(z, aes(sample = age)) +  
  stat_qq(size = 2, alpha = 1) +    
  stat_qq_line(color = "red", size = 1) +  
  labs(title = "Q-Q Plot of Age", x = "Theoretical Quantiles", y = "Sample Quantiles")

q4 <- ggplot(z, aes(sample = years_of_education)) +  
  stat_qq(size = 2, alpha = 1) +    
  stat_qq_line(color = "red", size = 1) +  
  labs(title = "Q-Q Plot of Years of Education", x = "Theoretical Quantiles", y = "Sample Quantiles")

q5 <- ggplot(z, aes(sample = zombies_killed)) +  
  stat_qq(size = 2, alpha = 1) +    
  stat_qq_line(color = "red", size = 1) +  
  labs(title = "Q-Q Plot of Zombies Killed", x = "Theoretical Quantiles", y = "Sample Quantiles")

grid.arrange(q1, q2, q3, q4, q5, ncol = 2)

# step 6

sample_z <- z |>
  group_by(gender) |>                         
  slice_sample(n = 50, replace = FALSE) |>
  summarise(
    height_mean = mean(height, na.rm = TRUE),
    height_sd = sd(height, na.rm = TRUE),
    weight_mean = mean(weight, na.rm = TRUE),
    weight_sd = sd(weight, na.rm = TRUE),
    age_mean = mean(age, na.rm = TRUE),
    age_sd = sd(age, na.rm = TRUE),
    years_of_education_mean = mean(years_of_education, na.rm = TRUE),
    years_of_education_sd = sd(years_of_education, na.rm = TRUE),
    zombies_killed_mean = mean(zombies_killed, na.rm = TRUE),
    zombies_killed_sd = sd(zombies_killed, na.rm = TRUE)
  ) 




print(sample_z)

quantile_z = quantile(sample_z, probs = c(0.025, 0.975))

print(quantile_z)


# step 7

sample_distribution <- d |>
  rep_slice_sample(n = 100, reps = 1000, replace = FALSE)

s_dist <- sample_distribution |>
  group_by(replicate, decades) |>
  summarise(
    rm_mean = mean(runtimeMinutes, na.rm = TRUE),
    rm_sd = sd(runtimeMinutes, na.rm = TRUE),
    .groups = "drop"
  )

# step 8




# step 9




# step 10




p <- ggplot(data = d, aes(x = (decades), y = (runtimeMinutes))) +
  xlab("decades") +
  ylab("runtimeMinutes") +
  geom_point(na.rm = TRUE) +
  theme(legend.position = "none") +
  geom_boxplot()




