df <- read.csv('fig4b_out.csv')

library(ggplot2)

p <- ggplot(df, aes(x = t.SNE.1, y = t.SNE.2, shape = Pred, color = True)) +
  geom_point(size = 3) + 
  theme_minimal() +
  labs(x = "t-SNE 1", y = "t-SNE 2", title = "") +
  scale_color_brewer(palette = "Set1") +
  theme(legend.position = "right",
        axis.text.x = element_blank(),
        axis.text.y = element_blank())

print(p)
ggsave("Figure4b.png", plot = p, width = 10, height = 6)
