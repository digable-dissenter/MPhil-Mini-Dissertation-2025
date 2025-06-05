library(ggplot2)
library(soccermatics)

formation_442 <- data.frame(
  x = c(20, 20, 20, 20,  # Back 4: RB, RCB, LCB, LB
        45, 45, 45, 45,  # Midfield 4: RM, CM1, CM2, LM
        70, 70),         # Front 2: RF, LF
  y = c(12, 24, 44, 56,
        12, 24, 44, 56,
        24, 44),
  label = c("RB", "RCB", "LCB", "LB",
            "RM", "RCM", "LCM", "LM",
            "RF", "LF")
)

soccerPitch(arrow = "r", theme = "grass", title = "4-4-2 Formation") +
  geom_point(data = formation_442, aes(x = x, y = y), size = 10, color = "#1f77b4") +
  geom_text(data = formation_442, aes(x = x, y = y, label = label),
            color = "white", size = 3, hjust = 0.5, vjust = 0.5)
# Save the plot
