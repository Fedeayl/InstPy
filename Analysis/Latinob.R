
LAPOP <- rio::import(here::here("Data", "LAPOPtodo.sav"))

PY <- LAPOP[LAPOP$pais ==12, ]

# Importación de bases
ImportLatinobarometro <- function (Año) {rio::import(paste0("/Users/Fede/Desktop/Bases de datos/Latinobarometro/", 
                          paste(Año, "Latinobarometro.sav", sep="-")))}

B2020 <- ImportLatinobarometro(2020)
B2018 <- ImportLatinobarometro(2018)
B2017 <- ImportLatinobarometro(2017)
B2016 <- ImportLatinobarometro(2016)
B2015 <- ImportLatinobarometro(2015)
B2013 <- ImportLatinobarometro(2013)
B2011 <- ImportLatinobarometro(2011)
B2010 <- ImportLatinobarometro(2010)
B2009 <- ImportLatinobarometro(2009)
B2008 <- ImportLatinobarometro(2008)
B2007 <- ImportLatinobarometro(2007)
B2006 <- ImportLatinobarometro(2006)
B2005 <- ImportLatinobarometro(2005)
B2004 <- ImportLatinobarometro(2004)
B2003 <- ImportLatinobarometro(2003)
B2002 <- ImportLatinobarometro(2002)
B2001 <- ImportLatinobarometro(2001)
B2000 <- ImportLatinobarometro(2000)
B1998 <- ImportLatinobarometro(1998)
B1997 <- ImportLatinobarometro(1997)
B1996 <- ImportLatinobarometro(1996)
B1995 <- ImportLatinobarometro(1995)

# Paraguay = "600"
pais <- 600

Py1995 <- B1995[B1995$pais==600,]
table(Py1995$p27j)

Py1996 <- B1996[B1996$pais==600,]
table(Py1996$p33j)

Py1997 <- B1997[B1997$idenpa==600,]
table(Py1997$sp63g)

Py1998 <- B1998[B1998$idenpa==600,]
table(Py1998$sp38g)

Py2000 <- B2000[B2000$IDENPA==600,]
table(Py2000$P35ST.G)

Py2001 <- B2001[B2001$idenpa==600,]
table(Py2001$p61stg)

Py2002 <- B2002[B2002$idenpa==600,]
table(Py2002$p34stf)

Py2003 <- B2003[B2003$idenpa==600,]
table(Py2003$p21std)

Py2004 <- B2004[B2004$idenpa==600,]
table(Py2004$p34std)

Py2005 <- B2005[B2005$idenpa==600,]
table(Py2005$p47stb)

Py2006 <- B2006[B2006$idenpa==600,]
table(Py2005$p47stb)
