library(dplyr)

dfData <- read.csv('consolidated_maximum_test_acc.tsv',header=FALSE,sep="\t")
dfData <- dfData[order(dfData[[1]]), ]
dfData$Mean <- rowMeans(dfData[, 2:11])

optimal <- c(
  "species_RESNET8_ITS_4mer_32,64,128,7",
  "species_NORESNET8_ITS_4mer_32,64,128,7",
  "species_RESNET10_ITS_4mer_16,32,128,256,7",
  "species_NORESNET10_ITS_4mer_16,32,128,256,7",
  "species_RESNET18_ITS_4mer_32,64,256,7",
  "species_NORESNET18_ITS_4mer_32,64,256,7",
  "species_RESNET34_ITS_4mer_64,128,256,7",
  "species_NORESNET34_ITS_4mer_64,128,256,7"
)

dfFiltered <- subset(dfData, dfData[[1]] %in% optimal)
dfFiltered <- dfFiltered[, 1:11]

dfFiltered <- dfFiltered %>% slice(match(optimal, dfFiltered[[1]]))

dfFiltered[,1] <- c('MyResNet8','MyResNet8-X', 'MyResNet10','MyResNet10-X',
                    'ResNet18', 'ResNet18-X','ResNet34', 'ResNet34-X')

dfFiltered <- as.data.frame(dfFiltered)
dfFiltered$V1 <- factor(dfFiltered$V1, levels = c('MyResNet8','MyResNet8-X', 'MyResNet10','MyResNet10-X',
                    'ResNet18', 'ResNet18-X','ResNet34', 'ResNet34-X'))
dfFiltered[, 2:11] <- lapply(dfFiltered[, 2:11], as.numeric)

plot_data <- dfFiltered %>%
  gather(key = "Metric", value = "Value", -V1)

color_palette <- c( "MyResNet8" = "#e31a1c", 
                    "MyResNet8-X" = "#e31a1c",
                   "MyResNet10" = "#ff7f00", 
                   "MyResNet10-X" = "#ff7f00",
                   "ResNet18" = "#6a3d9a", 
                   "ResNet18-X" = "#6a3d9a", 
                   "ResNet34" = "#b15928",
                   "ResNet34-X" = "#b15928")

p <- ggplot(plot_data, aes(x = V1, y = Value, fill=V1)) +
  geom_boxplot(aes(group = V1)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        legend.position = "none") +
  scale_fill_manual(values = color_palette) +
  xlab("") +
  ylab("Test accuracy") +
  ggtitle("")

print(p)
ggsave("Figure2c.png", plot = p, width = 10, height = 6)
