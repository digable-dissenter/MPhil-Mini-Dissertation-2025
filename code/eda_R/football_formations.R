# Load libraries
library(ggplot2)
library(dplyr)
library(ggforce) # for enhanced circle drawing
library(soccermatics)

# Create helper function for requested formation
get_formation_df <- function(formation_name) {
  switch(formation_name,
         "4-4-2" = data.frame(
           position = c("GK", "RB", "RCB", "LCB", "LB",
                        "RM", "RCM", "LCM", "LM",
                        "RF", "LF"),
           x = c(5, 25, 25, 25, 25,
                 55, 45, 45, 55,
                 70, 65),
           y = c(34, 10, 26, 42, 58,
                 10, 24, 44, 58,
                 24, 44),
           zone = c("DEF", "DEF", "DEF", "DEF", "DEF",
                    "MID", "MID", "MID", "MID",
                    "ATT", "ATT"),
           group = c("Goalkeeper", rep("Defenders", 4),
                     rep("Midfielders", 4),
                     rep("Forwards", 2)),
           formation = "4-4-2"
         ),
         "4-3-3" = data.frame(
           position = c("GK", "RB", "RCB", "LCB", "LB",
                        "RCM", "CM", "LCM",
                        "RW", "ST", "LW"),
           x = c(5, 25, 25, 25, 25,
                 50, 45, 50,
                 70, 70, 70),
           y = c(34, 10, 26, 42, 58,
                 20, 34, 48,
                 10, 34, 58),
           zone = c("DEF", "DEF", "DEF", "DEF", "DEF",
                    "MID", "MID", "MID",
                    "ATT", "ATT", "ATT"),
           group = c("Goalkeeper", rep("Defenders", 4),
                     rep("Midfielders", 3),
                     rep("Forwards", 3)),
           formation = "4-3-3"
         ),
         "4-2-3-1" = data.frame(
           position = c("GK", "RB", "RCB", "LCB", "LB",
                        "CDM1", "CDM2", 
                        "LAM", "CAM", "RAM",
                        "ST"),
           x = c(5, 25, 25, 25, 25,     # Defensive line
                 45, 45,                         # CDMs
                 65, 65, 65,                   # Attacking mids
                 85),                               # Striker
           y = c(34, 10, 26, 42, 58,       # evenly spaced across 68m width
                 20, 48,
                 18, 34, 50,
                 34),              # forward
           zone = c("DEF", "DEF", "DEF", "DEF", "DEF", 
                    "MID", "MID",
                    "MID", "MID", "MID",
                    "ATT"),
           group = c("Goalkeeper", rep("Defenders", 4),
                     rep("Midfielders", 5),
                     rep("Forwards", 1)),
           formation = "4-2-3-1"
         ),
         "3-5-2" = data.frame(
           position = c("GK", "RCB", "CB", "LCB",
                        "RWB", "LWB",
                        "RCM", "CM", "LCM",
                        "RF", "LF"),
           x = c(5, 25, 25, 25,
                 35, 35,
                 38, 45, 52,
                 70, 70),
           y = c(34, 20, 34, 48,
                 10, 58,
                 20, 34, 48,
                 28, 40),
           zone = c("DEF", "DEF", "DEF", "DEF", "DEF", "DEF",
                    "MID", "MID", "MID",
                    "ATT", "ATT"),
           group = c("Goalkeeper", rep("Defenders", 5),
                     rep("Midfielders", 3),
                     rep("Forwards", 2)),
           formation = "3-5-2"
         ),
         stop("Formation not supported")
  )
}

# Create a plotting function
plot_formation <- function(formation_name, theme = "grass", save = FALSE) {
  formation_df <- get_formation_df(formation_name)
  
  p <- soccerPitch(lengthPitch = 105, widthPitch = 68,
                   arrow = "r", theme = theme, 
                   title = paste(formation_name, "Formation"),
                   subtitle = "Player roles and spatial zones") +
  # Shaded thirds
    # Defensive zone
  geom_rect(aes(xmin = 0, xmax = 35, ymin = 0, ymax = 68), fill = "darkgreen", alpha = 0.2) +
    # Midfield zone
  geom_rect(aes(xmin = 35, xmax = 70, ymin = 0, ymax = 68), fill = "green", alpha = 0.2) +
    # Attacking zone
  geom_rect(aes(xmin = 70, xmax = 105, ymin = 0, ymax = 68), fill = "lightgreen", alpha = 0.2) +
  
  # Player positions
  geom_point(data = formation_df, aes(x = x, y = y, color = group), size = 10) +
  geom_text(data = formation_df, aes(x = x, y = y, label = position), 
              color = "white", size = 3, hjust = 0.5, vjust = 0.5) +
  # Zone labels
  annotate("text", x = 20, y = 34, label = "Defensive Zone", color = "white", size = 4, angle = 270) +
  annotate("text", x = 52.5, y = 34, label = "Midfield Zone", color = "white", size = 4, angle = 270) +
  annotate("text", x = 80, y = 34, label = "Attacking Zone", color = "white", size = 4, angle = 270) +
  scale_colour_manual(values = c("Goalkeeper" = "#FFA500", 
                                  "Defenders" = "#3498DB", 
                                  "Midfielders" = "#2ECC71", 
                                  "Forwards" = "#E74C3C")) +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.background = element_rect(fill = "white", color = NA),
        plot.title = element_text(hjust = 0.5, color = 'white', size = 16),
        plot.subtitle = element_text(hjust = 0.5, color = 'white', size = 12))
    
  
  if (save) {
    ggsave(paste0(gsub("-","", formation_name), "_formation.png"), 
           plot = p, width = 10, height = 8, dpi = 300)
  }
  
  return(p)
}


formation_4231 <- get_formation_df("4-3-3")

# Example usage - Define the 4-2-3-1 formation
# plot_formation(formation_4231$formation[1], save = TRUE)
plot_formation("4-2-3-1", save = TRUE)

plot_formation("4-3-3", save = TRUE)

plot_formation("4-4-2", save = TRUE)

plot_formation("3-5-2", save = TRUE)                                


# Create position clusters visualisation
position_clusters_plot <- create_pitch() +
  # Add position points
  geom_point(data = positions_df, aes(x = x, y = y, color = group), size = 5) +
  # Add position labels
  geom_text(data = positions_df, aes(x = x, y = y, label = position), 
            color = "white", size = 3) +
  # Customise colours
  scale_color_manual(values = c("Goalkeeper" = "#FF5700", 
                                  "Defenders" = "#3498DB", 
                                  "Midfielders" = "#2ECC71", 
                                  "Forwards" = "#E74C3C")) +
  # Add title and theme
  labs(title = "Football Player Position Clusters",
       subtitle = "Spatial distribution of player roles across the pitch",
       color = 'Position Group') +
  theme(plot.title = element_text(hjust = 0.5, color = 'white', size = 16),
        plot.subtitle = element_text(hjust = 0.5, color = 'white', size = 12),
        legend.position = "bottom",
        legend.background = element_rect(fill = "white"),
        legend.title = element_blank(),
        legend.text = element_text(size = 10))

# Display the plot
print(position_clusters_plot)
# Save the plot as an image
ggsave("position_clusters.png", position_clusters_plot, width = 10, height = 8, dpi = 300)