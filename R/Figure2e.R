gaussian <- c(0.8146, 0.7765, 0.7734, 0.7840)
lenet <- c(0.8348, 0.8261, 0.8156, 0.8226)
resnet8 <- c(0.8463, 0.8393, 0.8255, 0.8338)
resnet10 <- c(0.8542, 0.8470, 0.8342, 0.8421)
resnet18 <- c(0.8690, 0.8603, 0.8496, 0.8569)
resnet34 <- c(0.8653, 0.8601, 0.8490, 0.8561)

color_palette <- c("GaussianNB" = "#1f78b4", 
                   "LeNet-5" = "#33a02c", 
                   "MyResNet8" = "#e31a1c", 
                   "MyResNet10" = "#ff7f00", 
                   "ResNet18" = "#6a3d9a", 
                   "ResNet34" = "#b15928")

metrics <- c("Precision", "Recall", "F1-Score", "MCC")
model_names <- c("GaussianNB", "LeNet-5", "MyResNet8", "MyResNet10", "ResNet18", "ResNet34")
values <- c(gaussian, lenet, resnet8, resnet10, resnet18, resnet34)

df <- data.frame(
  Model = factor(rep(model_names, each = length(metrics)), levels = model_names),
  Metric = factor(rep(metrics, times = length(model_names)), levels = metrics),
  Value = values
)

library(ggplot2)

p <- ggplot(df, aes(x = Metric, y = Value, fill = Model)) +
  geom_bar(stat = "identity", position = position_dodge(), color="black") +
  scale_fill_manual(values = color_palette) +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size = 14),
        axis.text.y = element_text(size = 14), 
        axis.title.x = element_text(size = 16), 
        axis.title.y = element_text(size = 16), 
        legend.position = "right",
        legend.text = element_text(size = 14), 
        legend.title = element_text(size = 16)) +
  xlab("") +
  ylab("") +
  ggtitle("") +
  coord_cartesian(ylim = c(0.7, 0.9))

print(p)
ggsave("Figure2e.png", plot = p, width = 20, height = 6)
