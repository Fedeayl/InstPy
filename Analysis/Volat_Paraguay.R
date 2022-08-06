library(dplyr)
library(ggplot2)
PY <- rio::import(here::here("Data", "Dataset_AL.xlsx"))


PYA <- PY[PY$Country=="PARAGUAY", c("Year","Volat_A_Cohen")]
PYA$Tipo <- c("Exógena")
names(PYA) <- c("Año", "Volatilidad", "Tipo")
PYB <- PY[PY$Country=="PARAGUAY", c("Year","Volat_B_Cohen")]
PYB$Volat <- c("Endógena")
names(PYB) <- c("Año", "Volatilidad", "Tipo")
PYT <- PY[PY$Country=="PARAGUAY", c("Year","Evolat_Cohen")]
PYT$Volat <- c("Total")
names(PYT) <- c("Año", "Volatilidad", "Tipo")

PYvolat <- full_join(PYA, PYB) |> full_join(PYT)
PYvolat <- PYvolat[!duplicated(PYvolat$Volatilidad), ]


G2 <- ggplot(PYvolat, aes(x=as.numeric(Año), y=as.numeric(Volatilidad), color=Tipo))+
        geom_line() +
        labs(title = "") +
        xlab("")+
        ylab("Índice de Pedersen")+
        labs(color = "") + 
        theme_minimal() +
        theme(plot.margin = margin(10,30,10,10),
              axis.text.x = element_text(angle = -0, vjust = 0, hjust = 0, size = 6),
              axis.text.y = element_text(size = 6),
              legend.position= "bottom",
              text=element_text(size=10, family="Cambria"))


jpeg(filename = "Figures/Volatilidad.jpg", 
     width = 1200, height = 1000, res = 300)
G2
dev.off()




