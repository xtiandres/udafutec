library(forcats)
library(RColorBrewer)

tota = full_join(taba1, taba2, by = "EQUIPOS")
totb = full_join(tota, taba3, by = "EQUIPOS")
totc = full_join(totb, taba4, by = "EQUIPOS")
totx = select(totc, "EQUIPOS", "J1", "J2", "J3", "J4")
totx <- gather(totx,
               key = "variable",
               value = "value",
               J1:J4)

# Save totx data.frame as txt file
write.table(totx,"totx.txt",sep="\t",row.names=FALSE)

  
px <- ggplot(totx, aes(x = value, 
                        y = variable,
                        #group = EQUIPOS,
                        color = EQUIPOS)) 
px +
  geom_point(size = 6) +
  scale_color_manual(values = c("#FF994E", "#F2C80F", "#663399", "#0078D7",
                                "#D82C20", "#118DFF", "#00AFBB", "#8AD4EB",
                                "#F66DB6", "#F3F3F3", "#B5DAFE", "#107C10",
                                "#70B0E0", "#A43B76", "#F64F5C", "#5C0001")) +
  geom_line(size = .1, color = "#CCAA14") +
  aes(x = fct_inorder(value)) +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + 
  labs(title = "CAMPEONATO 2022 - Tabla de Posiciones",
       subtitle = "Histórico de posiciones por Equipo y Jornada",
       caption = "powered by Udaviz",
       x = "Posiciones Acumuladas",
       y = "Jornadas Campeonato")