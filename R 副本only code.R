# Libraries
library(readxl)
library(ggpubr)
library(tidyverse)

# Reading data
d <- read_excel("Dose_response1.xlsx")

# Combining species and herbicide names into a single column
d %>% 
  unite(species_herbicide, species, herbicide, sep = "_", remove = F) -> d1

# Creating a dataframe with the average value of the control group
d1 %>% 
  group_by(species_herbicide) %>% 
  filter(dose_uM == 0) %>%
  summarise(control_ave = mean(weight_mg)) -> control

# Joining the original data with the control averages
d1 %>% 
  left_join(control, by = "species_herbicide") -> d2

# Creating a new dataframe for line graph plotting
d2 %>% 
  mutate(rel = weight_mg / control_ave * 100) %>% 
  group_by(species_herbicide, species, herbicide, dose_uM) %>%
  summarise(ave = mean(rel),
            N = length(rel),
            sd = sd(rel),
            se = sd / sqrt(N)) -> d3

# Modifying dose values
d3$dose0 <- d3$dose_uM
d3$dose0[d3$dose0 == 0] <- 0.0001

# Plotting the graph for a single herbicide
d3 %>% 
  filter(herbicide == "BSM") -> d4

ggscatter(d4, x = "dose0", y = "ave",
          combine = T, palette = "aaas",
          color = "species",
          ylab = "Relative growth (% of control)",
          xlab = "Herbicide (μM)",
          font.y = c("bold"),
          font.x = c("bold"))  +
  geom_line(aes(group = species, color = species, linetype = species), alpha = 0.7) +
  geom_linerange(aes(ymin = ave - se,
                     ymax = ave + se,
                     color = species)) +
  scale_x_log10() +
  theme(axis.title = element_text(size = 9),
        text = element_text(size = 7), 
        legend.position = "right",
        legend.title = element_blank())

# Plotting the graph with facets for different herbicides
ggscatter(d3, x = "dose0", y = "ave",
          combine = T, palette = "jco",
          color = "species",
          ylab = "Relative growth (% of control)",
          xlab = "Herbicide (μM)",
          font.y = c("bold"),
          font.x = c("bold"), facet.by = "herbicide", scales = "free_x")  +
  geom_line(aes(group = species, color = species, linetype = species), alpha = 0.7) +
  geom_linerange(aes(ymin = ave - se,
                     ymax = ave + se,
                     color = species)) +
  scale_x_log10() +
  theme(axis.title = element_text(size = 9),
        text = element_text(size = 7), 
        legend.position = "right",
        legend.title = element_blank())

# Customizing the x-axis labels
breaks_values <- unique(d3$dose0)

custom_labels <- function(breaks) {
  return(format(breaks, scientific = FALSE, digits = 10))
}

# Assigning the plot to a variable
final_plot <- ggscatter(d3, x = "dose0", y = "ave",
                        combine = T, palette = "jco",
                        color = "species",
                        ylab = "Relative growth (% of control)",
                        xlab = "",  # Empty string for x-axis label
                        font.y = c("bold"),
                        font.x = c("bold"), facet.by = "herbicide", scales = "free_x")  +
  geom_line(aes(group = species, color = species, linetype = species), alpha = 0.7) +
  geom_linerange(aes(ymin = ave - se,
                     ymax = ave + se,
                     color = species)) +
  scale_x_log10(breaks = breaks_values, labels = custom_labels) + 
  scale_color_manual(values = c("gray", "red")) +
  theme(axis.title = element_text(size = 9),
        text = element_text(size = 7), 
        legend.position = "right",
        legend.title = element_blank())

# Saving the plot to a file
ggsave("final_plot.png", final_plot, width = 10, height = 8, dpi = 300)
