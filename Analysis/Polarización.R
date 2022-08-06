library(dplyr)
library(ggplot2)

PY <- rio::import(here::here("Data", "Dataset_AL.xlsx"))


PYDalton <- PY[PY$Country=="PARAGUAY", c("Year","Dalton_Seats")]


G2 <- ggplot(PYDalton, aes(x=as.numeric(Year), y=as.numeric(Dalton_Seats)))+
        geom_line(color="#B22222") +
        labs(title = "") +
        ylim(0.,3)+
        xlab("")+
        ylab("Índice de Dalton")+
        labs(color = "") + 
        theme_minimal() +
        theme(plot.margin = margin(10,30,10,10),
              axis.text.x = element_text(angle = -0, vjust = 0, hjust = 0, size = 6),
              axis.text.y = element_text(size = 6),
              legend.position= "bottom",
              text=element_text(size=10, family="Cambria"))


jpeg(filename = "Figures/Polarización.jpg", 
     width = 1200, height = 1000, res = 300)
G2
dev.off()

