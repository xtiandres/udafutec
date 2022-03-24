# GRAFICA UFC - COMPARATIVO LUCHADORES

library(gridExtra)
library(ggplot2)
library(grid)

data <- read.csv("/home/xut/Documents/udaviz/R/studio/udafutec/data/ufc.csv")

g1 <- ggplot(data,
             aes(Medidor, FrankieEdgar)) +
  geom_bar(stat = "identity",
           width = .5,
           fill = "#0000ff") + 
  geom_text(aes(label = FrankieEdgar), 
            size = 4,
            vjust = 0.5,
            color = "red", 
            hjust = "right") +
  ggtitle("") +
  theme(axis.title.x = element_blank(), 
        axis.title.y = element_blank(), 
        axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(),
        #panel.background = element_rect(fill = "#BFD5E3", colour = "#6D9EC1"),
        panel.background = element_rect(fill = "#c2dfff", colour = "#6D9EC1"),
        plot.margin = unit(c(1, -1, 1, 0), "mm")) +
  scale_y_reverse() +
  coord_flip()

g2 <- ggplot(data,
             aes(Medidor, ChitoVera)) +
  xlab(NULL) +
  geom_bar(stat = "identity",
           width = .5,
           fill = "#ffff00") + 
  geom_text(aes(label = ChitoVera), 
            size = 4,
            vjust = 0.5, 
            color = "red", 
            hjust = "left") +
  ggtitle("") +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), 
        axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        #panel.background = element_rect(fill = "#BFD5E3", colour = "#6D9EC1"),
        panel.background = element_rect(fill = "#93ffe8", colour = "#6D9EC1"),
        plot.margin = unit(c(1, 0, 1, -1), "mm")) +
  coord_flip()

g.mid <- ggplot(data,
                aes(1, Medidor)) +
  geom_text(aes(label = Medidor)) +
  #geom_segment(aes(x = 0.92, xend = 0.94, yend = Medidor)) +
  #geom_segment(aes(x = 1.06, xend = 1.08, yend = Medidor)) +
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

#svg("ufc268.svg", width = 14, height = 7)

gg1 <- ggplot_gtable(ggplot_build(g1))
gg2 <- ggplot_gtable(ggplot_build(g2))
gg.mid <- ggplot_gtable(ggplot_build(g.mid))

grid.arrange(gg1, 
             gg.mid, 
             gg2, 
             ncol=3,
             widths = c(4/9,1.3/9,4/9))

#dev.off()