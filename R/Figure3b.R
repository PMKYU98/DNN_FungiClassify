benA <- c(0.9802, 0.9757, 0.9731, 0.9750)
ITS <- c(0.8262, 0.8100, 0.7992, 0.8039)

metrics <- c("Precision", "Recall", "F1-Score", "MCC")
model_names <- c("benA", "ITS")
values <- c(benA, ITS)

df <- data.frame(
  Model = factor(rep(model_names, each = length(metrics)), levels = model_names),
  Metric = factor(rep(metrics, times = length(model_names)), levels = metrics),
  Value = values
)

library(ggplot2)

p <- ggplot(df, aes(x = Metric, y = Value, fill=Model)) +
  geom_bar(stat = "identity", position = position_dodge(), color="black") +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        legend.position = "bottom",
        legend.text = element_text(size = 14), 
        legend.title = element_text(size = 16)) +
  xlab("") +
  ylab("") +
  ggtitle("") +
  coord_cartesian(ylim = c(0.7, 1.0))

# Print the plot
print(p)
ggsave("Figure3b.png", plot = p, width = 10, height = 6)
