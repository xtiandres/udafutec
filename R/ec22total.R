# GRÁFICO BUMP CHART - OK

# FUNCIÓN COLORES PARA GGPLOT

my_theme <- function() {
  # Colors
  color.background = "grey"
  color.text = "#22211d"
  # Begin construction of chart
  theme_bw(base_size=15) +
    # Format background colors
    theme(panel.background = element_rect(fill=color.background,
                                          color=color.background)) +
    theme(plot.background  = element_rect(fill=color.background,
                                          color=color.background)) +
    theme(panel.border     = element_rect(color=color.background)) +
    theme(strip.background = element_rect(fill=color.background,
                                          color=color.background)) +
    # Format the grid
    theme(panel.grid.major.y = element_blank()) +
    theme(panel.grid.minor.y = element_blank()) +
    theme(axis.ticks       = element_blank()) +
    # Format the legend
    theme(legend.position = "right") +
    # Format title and axis labels
    theme(plot.title       = element_text(color=color.text, size=20, face = "bold")) +
    theme(axis.title.x     = element_text(size=14, color="black", face = "bold")) +
    theme(axis.title.y     = element_text(size=14, color="black", face = "bold",
                                          vjust=1.25)) +
    theme(axis.text.x      = element_text(size=10, vjust=0.5, hjust=0.5,
                                          color = color.text)) +
    theme(axis.text.y      = element_text(size=10, color = color.text)) +
    theme(strip.text       = element_text(face = "bold")) +
    # Plot margins
    theme(plot.margin = unit(c(0.35, 0.2, 0.3, 0.35), "cm"))
}

# CONVERSIÓN A NÚMERO DE VARIABLE "value"

totx$value <- as.numeric(totx$value)

# GRÁFICO

ggplot(totx, aes(x = variable, y = value, group = EQUIPOS)) +
  geom_line(aes(color = EQUIPOS, alpha = 1), size = 2) +
  geom_point(aes(color = EQUIPOS, alpha = 1), size = 6) +
  scale_y_reverse(breaks = 1:nrow(totx)) +
#  scale_x_discrete(breaks = 1:3) +
#  theme(legend.position = "TRUE") +
#  theme_minimal() +
#  theme(panel.grid.major = element_blank(),
#        panel.grid.minor = element_blank()) + 
  labs(title = "CAMPEONATO ECUATORIANO DE FÚTBOL 2022",
       subtitle = "Comportamiento por Jornada de los Equipos en la tabla de posiciones",
       caption = "powered by Udaviz",
       x = "Jornadas de Campeonato",
       y = "Posiciones Tabla Acumulada") +
  my_theme() +
  scale_color_manual(values = c("#FFD86C", "#F3C911", "#663399", "#3257A8",
                                "#D83B01", "#12239E", "#95DABB", "#42F9F9",
                                "#EF008C", "#F3F3F3", "#B5DAFE", "#009292",
                                "#2F8AC3", "#026645", "#D82C20", "#70B0E0"))

