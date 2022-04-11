# Loading necessary libraries for data processing and analysis
library(readr)
library(dplyr)
library(ggplot2)
library(scales)

# Importing a csv file into R working environment
df4vis <- read_csv(
  file = "MOP_MIS_graph_data.csv"
)

df4vis <- df4vis %>%
  mutate(
    MIS_fct = as.factor(MIS),
    Region_fct = as.factor(Region)
  )

# Initialize the plot
plot1 <- ggplot(
  data = df4vis,
  aes(
    x = MOP,
    y = WCR,
  )
)

plot1

# Add panel for MIS
plot1 <- plot1 + 
  facet_wrap(
    ~MIS_fct, 
    ncol = 3
  )
plot1

# Making y-axis scale separate for each panel
plot1 <- plot1 + 
  facet_wrap(
    ~MIS_fct, 
    ncol = 3,
    scales = "free_y"
  )
plot1

# Adding lines for each Region
plot1 <- plot1 + 
  geom_line(
    aes(
      group = Region_fct
    )
  )
plot1

# Adding color for each lines for each region
plot1 <- plot1 + 
  geom_line(
    aes(
      col = Region_fct
    )
  )

plot1

# Customizing the plot
plot1 <- plot1 + 
  theme_bw()+
  scale_color_manual(
    name = "Region:",
    values = c("red", "green", "blue", "orange")
  ) 
plot1
# Changing legend position
plot1 <- plot1 +
  theme(
    legend.position = "top"
  )
plot1
# Removing grids

plot1 <- plot1 +
  theme(
    panel.grid = element_blank()
  )
plot1
# Removing scientific notation from y-axis
plot1 <- plot1 + 
  scale_y_continuous(labels = percent)
plot1
# Do we really need to add points?
# Adding points on the lines
plot1 <- plot1 + 
  geom_point(
    aes(
      group = Region_fct,
      col = Region_fct
    )
  )
plot1

# Adding shape of points on the lines
plot1 <- plot1 + 
  geom_point(
    aes(
      group = Region_fct,
      shape = Region_fct,
      col = Region_fct
    )
  )
plot1
# Customizing shapes
plot1 <- plot1 + 
  scale_shape_manual(
    name = "Region:",
    values = c(0,1,2,8)
  )
plot1

# Plot title

plot1 + labs(title = "MIS")

# Taking plot output as pdf file

pdf("MIS_by_Region.pdf", height = 6.5, width = 9.5)
 plot1
dev.off()