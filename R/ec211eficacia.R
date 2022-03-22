#Gráfico Tabla Acumulada Eficacia vs Equipos

t8 <- ggplot(tabla2e,
             aes(x = reorder(Equipos,Eficacia), 
                 y = Eficacia))
                 #size = Eficacia,
                 #shapes = PartidosJugados))

t8 + 
  geom_point(aes(shape=as.character(PartidosJugados)),
             size = 3) +
  geom_segment(aes(y = 10,
                   yend = Eficacia,
                   x = reorder(Equipos, Eficacia),
                   xend = reorder(Equipos, Eficacia)),
               color = "lightgrey") +
  geom_text(aes(label = formattable::percent(Eficacia/100)),
            vjust = -0.7) +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
        #legend.background = element_rect(fill = "darkgray",
        #                                 color = "yellow",
        #                                 linetype = "solid"),
        #legend.key = element_rect(fill = "yellow"),
        #legend.position = "none",
        ) +
  theme(axis.text.x = element_text(colour = "darkgreen",
                                   angle = 45,
                                   hjust = 1)) +
  labs(title = "Test8",
       subtitle = "Eficacia vs Equipos",
       shape = "P-Jugados",
       x = "",
       y = "Eficacia [%].")