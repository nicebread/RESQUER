theme_singlebar <- function(base_size = 12, base_family = "") {
  theme_minimal(base_size = base_size, base_family = base_family) + theme(
  axis.text = element_blank(),       # Remove axis text
  axis.title = element_blank(),      # Remove axis title
  axis.ticks = element_blank(),      # Remove ticks
  panel.grid.major = element_blank(),# Remove major grid
  panel.grid.minor = element_blank(),# Remove minor grid
  axis.line = element_blank(),        # Remove axis line
  plot.title = element_text(face="bold", margin = margin(t = 0, b = -100, unit = "pt")),              # move plot title closer to bar
  panel.spacing.x=unit(1, "lines"),   # add some extra space on the left side to make text labels visible
  plot.margin = margin(t=0, b=-200, l=0, r=0, unit="pt")
)

}
