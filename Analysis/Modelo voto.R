
library(ggplot2)
Data <- haven::read_dta(here::here("Data", "PY_LAPOP_2019.dta"))

Data <- as.data.frame(Data)

Data$voto <- Data$vb3n
Data <- Data[Data$voto==1201 | Data$voto==1202,]
Data <- Data[!is.na(Data$idnum),]
Data$voto <- as.factor(ifelse(Data$voto ==1201, 1, 0))
Data$voto<-relevel(Data$voto, ref = 1)

Data$urbano <- as.factor(Data$ur)
Data$urbano <- as.factor(ifelse(Data$urbano ==1, 1, 0))

Data$sexo_mujer <- as.factor(ifelse(Data$q1 ==2, 1, 0))

Data$edad <- as.numeric(Data$q2)

Data$ingresos <- as.numeric(Data$q10new)
Data$ideologia <- as.numeric(Data$l1)
Data$democracia <- as.numeric(Data$pn4)

Data$clien4a <- as.numeric(Data$clien4a)
Data$clien4b <- as.numeric(Data$clien4b)
Data$clientelismo <- ifelse(is.na(Data$clien4a)==FALSE, Data$clien4a, Data$clien4b)

probit <- glm(data=Data, voto~ urbano + sexo_mujer +  edad + ingresos + ideologia + 
                      democracia +clientelismo, family=binomial(link="probit"))
summary(probit)

stargazer::stargazer(probit)

Data$voto <- as.character(Data$voto)
Data[Data$voto==1,]$voto <- "ANR"
Data[Data$voto==0,]$voto <- "PLRA"

Gi <- ggplot(data = Data, mapping = aes(x = voto, y = ideologia, color=voto)) +
                geom_boxplot(aes(color = voto)) +
                labs(title = "",
                     subtitle = "") +
                scale_color_manual(values=c("#B22222","#1E90FF"))+
                labs(fill="") + ylim(0,10) +
                xlab("Voto")+ ylab("Ideología")+ labs(color = "") + 
                theme_minimal() +
                theme(plot.margin = margin(10,30,10,10),
                        axis.text.x = element_text(angle = -0, vjust = 0, hjust = 0, size = 8),
                        axis.text.y = element_text(size = 8),
                        legend.position= "none",
                        legend.title = element_text(size=8),
                        legend.text = element_text(size=8),
                        text=element_text(size=10, family="Cambria"))

jpeg(filename = "Figures/Ideología.jpg", 
     width = 1200, height = 1000, res = 300)
Gi
dev.off()

