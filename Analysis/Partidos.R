library(ggplot2)
library(patchwork)

Data <- readxl::read_xlsx(here::here("Data", "Paraguay.xlsx"), sheet = "VariableLAPOP")

Simp <- Data[Data$Variable=="Simpatia",]
Reun <- Data[Data$Variable=="ReunionesPartidos",]


G1 <- ggplot(Simp, aes(x=as.numeric(Año), y=as.numeric(Valor), fill=as.factor(Concepto)))+
        geom_col(width = 1.40) +
        labs(title = "Simpatía",
             subtitle = "con partidos políticos") +
        scale_fill_manual(values=c("#1E90FF","#B22222"))+
        labs(fill="") +
        xlab("")+
        ylab("%")+
        labs(color = "") + 
        theme_minimal() +
        theme(plot.margin = margin(10,30,10,10),
              axis.text.x = element_text(angle = -0, vjust = 0, hjust = 0, size = 6),
              axis.text.y = element_text(size = 6),
              legend.position= "bottom",
              legend.title = element_text(size=8),
              legend.text = element_text(size=6),
              text=element_text(size=10, family="Cambria"))


G2 <- ggplot(Reun, aes(x=as.numeric(Año), y=as.numeric(Valor), fill=as.factor(Concepto)))+
        geom_col(width = 1.40) +
        labs(title = "Asistencia",
             subtitle = "a reuniones partidarias") +
        scale_fill_manual(values=c("#B22222","#1E90FF"))+
        labs(fill="") +
        xlab("")+
        ylab("%")+
        labs(color = "") + 
        theme_minimal() +
        theme(plot.margin = margin(10,30,10,10),
              axis.text.x = element_text(angle = -0, vjust = 0, hjust = 0, size = 6),
              axis.text.y = element_text(size = 6),
              legend.position= "bottom",
              legend.title = element_text(size=8),
              legend.text = element_text(size=6),
              text=element_text(size=10, family="Cambria"))




Data2 <- readxl::read_xlsx(here::here("Data", "Paraguay.xlsx"), sheet = "Afiliacion")



G3 <- ggplot(Data2, aes(x=as.factor(Concepto), y=as.numeric(Valor), fill=as.factor(Concepto)))+
        geom_col(width = 0.8) +
        labs(title = "Afiliados",
             subtitle = "a los principales partidos") +
        scale_fill_manual(values=c("#B22222","#1E90FF"))+
        facet_wrap("Año")+
        labs(fill="") +
        xlab("")+
        ylab("% padrón")+
        labs(color = "") + 
        theme_minimal() +
        theme(plot.margin = margin(10,30,10,10),
              axis.text.x = element_blank(),
              axis.text.y = element_text(size = 6),
              legend.position= "bottom",
              legend.title = element_text(size=8),
              legend.text = element_text(size=6),
              text=element_text(size=10, family="Cambria"))


jpeg(filename = "Figures/Partidos.jpg", 
     width = 2000, height = 1000, res = 300)

G1 + G2 + G3

dev.off()




