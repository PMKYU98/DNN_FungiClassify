library(dplyr)

dfData <- read.csv('consolidated_maximum_test_acc.tsv',header=FALSE,sep="\t")
dfData <- dfData[order(dfData[[1]]), ]
dfData$Mean <- rowMeans(dfData[, 2:11])

optimal <- c(
  "genus_LENET5_ITS_4mer_32,64,4",
  "genus_RESNET8_ITS_4mer_32,64,128,7",
  "genus_RESNET10_ITS_4mer_16,64,128,256,7",
  "genus_RESNET18_ITS_4mer_32,64,256,7",
  "genus_RESNET34_ITS_4mer_64,128,256,512,512,7"
)

dfFiltered <- subset(dfData, dfData[[1]] %in% optimal)
dfFiltered <- dfFiltered[, 1:11]

dfFiltered <- dfFiltered %>%
  slice(match(c("GaussianNB", optimal), dfFiltered$V1))

gaussian <- c('GaussianNB', 0.879043062200957,0.8744497607655503,0.880956937799043,0.8795215311004785,0.8845933014354067,0.8822966507177034,0.8784688995215311,0.883444976076555,0.8816267942583732,0.875311004784689)

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
ggsave("Figure2d.png", plot = p, width = 10, height = 6)
