library(dplyr)

dfData <- read.csv('consolidated_maximum_epoch_time.tsv',header=FALSE,sep="\t")
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

gaussian <- c('GaussianNB', 13.671541213989258,13.538777589797974,13.58523440361023,14.103225708007812,14.17424750328064,13.806288480758667,13.623787879943848,13.520166873931885,13.784691095352173,14.217859506607056)

dfFiltered <- rbind(gaussian, dfFiltered)
dfFiltered[,1] <- c('GaussianNB', 'LeNet-5', 'MyResNet8','MyResNet10', 'ResNet18', 'ResNet34')

dfFiltered <- as.data.frame(dfFiltered)
dfFiltered$V1 <- factor(dfFiltered$V1, levels = c('GaussianNB', 'LeNet-5', 'MyResNet8','MyResNet10', 'ResNet18', 'ResNet34'))
dfFiltered[, 2:11] <- lapply(dfFiltered[, 2:11], as.numeric)

means <- rowMeans(dfFiltered[, 2:11])

plot_data <- data.frame(
  Model = dfFiltered$V1,
  Mean = means
)

color_palette <- c("GaussianNB" = "#1f78b4", 
                   "LeNet-5" = "#33a02c", 
                   "MyResNet8" = "#e31a1c", 
                   "MyResNet10" = "#ff7f00", 
                   "ResNet18" = "#6a3d9a", 
                   "ResNet34" = "#b15928")

p <- ggplot(plot_data, aes(x = Model, y = Mean, fill = Model)) +
  geom_bar(stat = "identity", width = 0.7, color="black") +
  scale_fill_manual(values = color_palette) +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        legend.position = "none") +
  xlab("") +
  ylab("Time duration(s)") +
  ggtitle("") +
  scale_y_continuous(limits = c(0, 250), expand=c(0,0))

print(p)
ggsave("Figure2b.png", plot = p, width = 10, height = 6)
