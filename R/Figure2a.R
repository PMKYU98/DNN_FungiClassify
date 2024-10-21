library(dplyr)
library(tidyr)
library(ggplot2)

dfData <- read.csv('consolidated_maximum_test_acc.tsv',header=FALSE,sep="\t")
dfData <- dfData[order(dfData[[1]]), ]
dfData$Mean <- rowMeans(dfData[, 2:11])

optimal <- c(
  "species_LENET5_ITS_4mer_32,32,4",
  "species_RESNET8_ITS_4mer_16,64,128,7",
  "species_RESNET10_ITS_4mer_16,64,128,256,7",
  "species_RESNET18_ITS_4mer_32,64,256,7",
  "species_RESNET34_ITS_4mer_64,128,256,7"
)

dfFiltered <- subset(dfData, dfData[[1]] %in% optimal)
dfFiltered <- dfFiltered[, 1:11]

dfFiltered <- dfFiltered %>%
  slice(match(c("GaussianNB", optimal), dfFiltered$V1))

gaussian <- c('GaussianNB', 0.777009, 0.774601, 0.770990, 0.779416, 0.775504, 
              0.780169, 0.778664, 0.775354, 0.777009, 0.776256)

dfFiltered <- rbind(gaussian, dfFiltered)
dfFiltered[,1] <- c('GaussianNB', 'LeNet-5', 'MyResNet8','MyResNet10', 'ResNet18', 'ResNet34')

dfFiltered <- as.data.frame(dfFiltered)
dfFiltered$V1 <- factor(dfFiltered$V1, levels = c('GaussianNB', 'LeNet-5', 'MyResNet8','MyResNet10', 'ResNet18', 'ResNet34'))
dfFiltered[, 2:11] <- lapply(dfFiltered[, 2:11], as.numeric)

plot_data <- dfFiltered %>%
  gather(key = "Metric", value = "Value", -V1)

color_palette <- c("GaussianNB" = "#1f78b4", 
                   "LeNet-5" = "#33a02c", 
                   "MyResNet8" = "#e31a1c", 
                   "MyResNet10" = "#ff7f00", 
                   "ResNet18" = "#6a3d9a", 
                   "ResNet34" = "#b15928")

p <- ggplot(plot_data, aes(x = V1, y = Value, fill=V1)) +
  geom_boxplot(aes(group = V1)) +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        legend.position = "none") +
  scale_fill_manual(values = color_palette) +
  xlab("") +
  ylab("Test accuracy") +
  ggtitle("")

print(p)
ggsave("Figure2a.png", plot = p, width = 10, height = 6)
