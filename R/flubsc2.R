# COMPARATIVO 2 EQUIPOS - BSC FLU

library(gridExtra)
library(ggplot2)
library(grid)

image1 <- png::readPNG("flu.png")
image2 <- png::readPNG("bsc.png")

data <- read.csv("/home/xut/Documents/udaviz/R/studio/udafutec/data/flubar.csv")


g1 <- ggplot(data,
             aes(Estadística, Fluminense)) +
  annotation_custom(rasterGrob(image1, width = 1, height = 1), ) +
  geom_bar(stat = "identity",
           width = .5,
           #fill = "#993333",
           fill = "#ffff33") + 
  geom_text(aes(label = Fluminense), 
            size = 6,
            hjust = -.5, 
            color = "darkred") + 
  #hjust = "right") +
  ggtitle("") +
  theme(axis.title.x = element_blank(), 
        axis.title.y = element_blank(), 
        axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(),
        panel.background = element_rect(fill = "transparent"),
        plot.margin = unit(c(1, -1, 1, 0), "mm")) +
  scale_y_reverse() + 
  coord_flip()

g2 <- ggplot(data,
             aes(Estadística, BarcelonaSC)) +
  annotation_custom(rasterGrob(image2, width = 1, height = 1)) +
  xlab(NULL) +
  geom_bar(stat = "identity",
           width = .5,
           #fill = "#ffff33",
           fill = "#990000") + 
  geom_text(aes(label = BarcelonaSC), 
            size = 6,
            hjust = 1.5, 
            color = "yellow") +
  #hjust = "left") +
  ggtitle("") +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), 
        axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        panel.background = element_rect(fill = "#fff8dc", colour = "#000000"),
        plot.margin = unit(c(1, 0, 1, -1), "mm")) +
  coord_flip()

g.mid <- ggplot(data,
                aes(1, Estadística)) +
  geom_text(aes(label = Estadística),
            size = 5) +
  ggtitle("") +
  ylab(NULL) +
  scale_x_continuous(expand = c(0, 0), limits=c(0.91, 1.1)) +
  theme(axis.title = element_blank(),
        panel.grid = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.background = element_blank(),
        #panel.background = element_rect(fill = "#fff8dc", colour = "#000000"),
        axis.text.x = element_text(color=NA),
        axis.ticks.x = element_line(color = NA),
        plot.margin = unit(c(1, -1, 1, -1), "mm"))


# GRAFICO

gg1 <- ggplot_gtable(ggplot_build(g1))
gg2 <- ggplot_gtable(ggplot_build(g2))
gg.mid <- ggplot_gtable(ggplot_build(g.mid))

grid.arrange(gg1,gg2,gg.mid,ncol=3,widths = c(5/9,5/9,1.2/9))


