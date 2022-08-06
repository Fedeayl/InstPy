
library(ggplot2)
library(dplyr)
library(patchwork)

Data <- rio::import(here::here("Data", "resultados-1996-2018-municipales-y-generales.csv"))

names(Data) <- iconv(names(Data),"WINDOWS-1252","UTF-8")
Data$depdes <- iconv(Data$depdes,"WINDOWS-1252","UTF-8")


#### Fragmentación ####

PY <- rio::import(here::here("Data", "Dataset_AL.xlsx"))
PY <- PY[PY$Country=="PARAGUAY",]
max(PY$Enph_Schmidt)
GF <- ggplot(PY, aes(x=as.numeric(Year), y=as.numeric(Enph_Schmidt),  color=Enph_Schmidt))+
        geom_line(color= "#B22222") +
        labs(title = "") +
        ylim(1,4)+
        xlab("")+
        ylab("NEP (legislativo)")+
        xlim(1992,2020) +
        theme_minimal() +
        theme(plot.margin = margin(10,30,10,10),
              axis.text.x = element_text(angle = -0, vjust = 0, hjust = 0, size = 6),
              axis.text.y = element_text(size = 6),
              legend.position = "bottom",
              text=element_text(size=10, family="Cambria"))

#### Volatilidad #####

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


GV <- ggplot(PYvolat, aes(x=as.numeric(Año), y=as.numeric(Volatilidad), color=Tipo))+
        geom_line() +
        scale_color_manual(values=c("#1E90FF","#B22222", "#556B2F"))+
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



#### Nacionalización ####

Data <- rio::import(here::here("Data", "resultados-1996-2018-municipales-y-generales.csv"))


names(Data) <- iconv(names(Data),"WINDOWS-1252","UTF-8")
Data$depdes <- iconv(Data$depdes,"WINDOWS-1252","UTF-8")

unique(Data$año)
table(Data$año, Data$tipo_eleccion)

nac98 <- Data[Data$tipo_eleccion=="generales" & Data$cand_desc=="DIPUTADOS" & Data$año==1998,
              c("depdes","siglas_lista", "votos", "total_votos")]
nac98 <- doBy::summary_by(nac98, votos~ depdes + siglas_lista, FUN=sum, na.rm=T)
names(nac98) <- c("PROVINCE","PARTY", "VOTES")


nac03 <- Data[Data$tipo_eleccion=="generales" & Data$cand_desc=="DIPUTADOS" & Data$año==2003,
              c("depdes","siglas_lista", "votos", "total_votos")]
nac03 <- doBy::summary_by(nac03, votos~ depdes + siglas_lista, FUN=sum, na.rm=T)
names(nac03) <- c("PROVINCE","PARTY", "VOTES")


nac08 <- Data[Data$tipo_eleccion=="generales" & Data$cand_desc=="DIPUTADOS" & Data$año==2008,
              c("depdes","siglas_lista", "votos", "total_votos")]
nac08 <- doBy::summary_by(nac08, votos~ depdes + siglas_lista, FUN=sum, na.rm=T)
names(nac08) <- c("PROVINCE","PARTY", "VOTES")


nac13 <- Data[Data$tipo_eleccion=="generales" & Data$cand_desc=="DIPUTADOS" & Data$año==2013,
              c("depdes","siglas_lista", "votos", "total_votos")]
nac13 <- doBy::summary_by(nac13, votos~ depdes + siglas_lista, FUN=sum, na.rm=T)
names(nac13) <- c("PROVINCE","PARTY", "VOTES")


nac18 <- Data[Data$tipo_eleccion=="generales" & Data$cand_desc=="DIPUTADOS" & Data$año==2018,
              c("depdes","siglas_lista", "votos", "total_votos")]
nac18 <- doBy::summary_by(nac18, votos~ depdes + siglas_lista, FUN=sum, na.rm=T)
names(nac18) <- c("PROVINCE","PARTY", "VOTES")



# municipales: 1996, 2001, 2006, 2010, 2015


dep96 <- Data[Data$tipo_eleccion=="municipales" & Data$cand_desc=="INTENDENTE" & Data$año==1996,
              c("depdes","siglas_lista", "votos", "total_votos")]
dep96 <- doBy::summary_by(dep96, votos~ depdes + siglas_lista, FUN=sum, na.rm=T)
names(dep96) <- c("PROVINCE","PARTY", "VOTES")


dep01 <- Data[Data$tipo_eleccion=="municipales" & Data$cand_desc=="INTENDENTE" & Data$año==2001,
              c("depdes","siglas_lista", "votos", "total_votos")]
dep01 <- doBy::summary_by(dep01, votos~ depdes + siglas_lista, FUN=sum, na.rm=T)
names(dep01) <- c("PROVINCE","PARTY", "VOTES")


dep06 <- Data[Data$tipo_eleccion=="municipales" & Data$cand_desc=="INTENDENTE" & Data$año==2006,
              c("depdes","siglas_lista", "votos", "total_votos")]
dep06 <- doBy::summary_by(dep06, votos~ depdes + siglas_lista, FUN=sum, na.rm=T)
names(dep06) <- c("PROVINCE","PARTY", "VOTES")


dep10 <- Data[Data$tipo_eleccion=="municipales" & Data$cand_desc=="INTENDENTE" & Data$año==2010,
              c("depdes","siglas_lista", "votos", "total_votos")]
dep10 <- doBy::summary_by(dep10, votos~ depdes + siglas_lista, FUN=sum, na.rm=T)
names(dep10) <- c("PROVINCE","PARTY", "VOTES")


dep15 <- Data[Data$tipo_eleccion=="municipales" & Data$cand_desc=="INTENDENTE" & Data$año==2015,
              c("depdes","siglas_lista", "votos", "total_votos")]
dep15 <- doBy::summary_by(dep15, votos~ depdes + siglas_lista, FUN=sum, na.rm=T)
names(dep15) <- c("PROVINCE","PARTY", "VOTES")



INSnacional <- cbind.data.frame(c(1998, 2003, 2008, 2013, 2018),
                                c(electoral::psns(nac98),
                                  electoral::psns(nac03),
                                  electoral::psns(nac08),
                                  electoral::psns(nac13),
                                  electoral::psns(nac18)),
                                c(rep("Nacional", 5)))

names(INSnacional) <- c("Año", "INS", "Elección")


INSdeptal <- cbind.data.frame(c(1996, 2001, 2006, 2010, 2015),
                              c(electoral::psns(dep96),
                                electoral::psns(dep01),
                                electoral::psns(dep06),
                                electoral::psns(dep10),
                                electoral::psns(dep15)),
                              c(rep("Sub-nacional", 5)))

names(INSdeptal) <- c("Año", "INS", "Elección")

INS <- dplyr::full_join(INSnacional, INSdeptal)



GN <- ggplot(INS, aes(x=as.numeric(Año), y=as.numeric(INS), color=Elección))+
        geom_line() +
        scale_color_manual(values=c("#1E90FF","#B22222", "#556B2F"))+
        labs(title = "") +
        ylim(0.7,1)+
        xlab("")+
        ylab("INS")+
        xlim(1996,2018) +
        theme_minimal() +
        theme(plot.margin = margin(10,30,10,10),
              axis.text.x = element_text(angle = -0, vjust = 0, hjust = 0, size = 6),
              axis.text.y = element_text(size = 6),
              legend.title = element_blank(),
              legend.position = "bottom",
              text=element_text(size=10, family="Cambria"))



#### Exportación #####


jpeg(filename = "Figures/Sistema.jpg", 
     width = 2000, height = 1000, res = 300)
GF + GV

dev.off()

jpeg(filename = "Figures/Nacionalización.jpg", 
     width = 1200, height = 1000, res = 300)
GN

dev.off()






