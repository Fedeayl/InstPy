LAPOP <- rio::import(here::here("Data", "LAPOPtodo.sav"))

PY <- LAPOP[LAPOP$pais ==12, ]

doBy::summary_by(PY, vb10~ year,na.rm=T)

prop.table(table(PY$year, PY$vb10), margin=1)
