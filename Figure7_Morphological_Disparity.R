#Morphological Disparity Visualization
#Figure 7 - Eastern Newt Vertebrae Analysis

library(ggplot2)
library(tidyverse)
library(patchwork)
library(knitr)
library(ggpubr)
library(readxl)

setwd("F:/Eastern Newt Project/Vertebrae_Analysis_Corrections")

# Load data
data <- read_excel("F:/Eastern Newt Project/Vertebrae_Analysis_Corrections/Vert_Total_Morph_Disparity.xlsx")

# Reshape to long format for life stages
data_long <- data %>%
  pivot_longer(cols = c(Adult, `Terr. Juvenile`, `Aq. Juvenile`, Paedomorph),
               names_to = "LifeStage", values_to = "Disparity")

# Define colors
stage_colors <- c("Adult" = "chartreuse2",
                  "Terr. Juvenile" = "darkorange1",
                  "Aq. Juvenile" = "deepskyblue",
                  "Paedomorph" = "darkmagenta")




# Define order for Vertebral Position
order_positions <- c("Atlas", "T1", "T4", "T7", "T10", "T13", "Sacral", "Caud1", "Caud2", "Caud3")
data_long$`Vertebral Position` <- factor(data_long$`Vertebral Position`, levels = order_positions)
data$`Vertebral Position` <- factor(data$`Vertebral Position`, levels = order_positions)

# Plot


Fig7.ggp<-ggplot() +
  geom_col(data = data_long,
           aes(x = `Vertebral Position`, y = Disparity, fill = LifeStage),
           position = position_dodge(width = 0.8), width = 0.7,
           color = "black") +
  geom_line(data = data,
            aes(x = `Vertebral Position`, y = `Total Disparity`, group = 1, color = "Total Disparity"),
            linewidth = 1.2) +
  geom_point(data = data,
             aes(x = `Vertebral Position`, y = `Total Disparity`, color = "Total Disparity"),
             size = 3) +
  # Rename legend labels and set order
  scale_fill_manual(values = stage_colors,
                    breaks = c("Adult", "Terr. Juvenile", "Aq. Juvenile", "Paedomorph"),
                    labels = c("Adult", "Ter. Juv.", "Aq. Juv.", "Paed.")) +
  scale_color_manual(values = c("Total Disparity" = "black")) +
  theme_minimal() +
  labs(x = "Vertebral Position", y = "Morphological Disparity",
       fill = "Life Stage", color = "") +
  theme(
    axis.title.x = element_text(face = "bold", size = 14),
    axis.title.y = element_text(face = "bold", size = 14),
    axis.text.x  = element_text(face = "bold", size = 12, angle = 45, hjust = 1),
    axis.text.y  = element_text(face = "bold", size = 12),
    plot.title   = element_text(hjust = 0.5, face = "bold", size = 16),
    legend.title = element_text(face = "bold", size = 13),
    legend.text  = element_text(size = 12)
  )

Fig7.ggp

ggsave("Figure7_Morphol_Disp.jpg", plot=last_plot(), device="jpg",
       path="F:/Eastern Newt Project/Vertebrae_Analysis_Corrections/JOA_FirstRevisions", scale=1, width = 200, height=100,
       units=c("mm"),dpi=400, limitsize = TRUE)
