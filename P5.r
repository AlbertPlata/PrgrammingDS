library(RMySQL)

puntoUno <- function(df){
  s <<- sapply(df, sd, na.rm = TRUE)
  mediana <<- sapply(df, median, na.rm = TRUE)
  rango <<- sapply(df, range, na.rm = TRUE)
  quantiles <<- quantile( x = unlist( df), 
                          c(.2, .5, .8, .9),
                          na.rm = TRUE )
}

data <- read.table(file='C:\\Users\\HP\\Downloads\\datos.txt', header = FALSE, sep = "", dec = ".", fill = TRUE)
print(data)

names(data) <- c("Archivo de muestras de temperaturas del mundo en los meses de mayo a septiembre")
puntoUno(data)
print(s)
print(mediana)
print(rango)
print(quantiles)
#-----------------------------------------------------------------------------
con <- dbConnect(MySQL(), 
                 user = 'root',
                 password = '',
                 host = 'localhost',
                 dbname = 'nba')
message("Ingrese el limite de asistencias")
query = sprintf("SELECT Asistencias_por_partido FROM nba.estadisticas WHERE Asistencias_por_partido > 2 and Asistencias_por_partido < %s ;", readline("Ingrese el limite de asistencias: \n"))
registros <- dbGetQuery(con, query)

df <- data.frame(Asistencias = registros)
names(df) <- c("Asistencias")
puntoUno(df)
print(s)
print(mediana)
print(rango)
print(quantiles)

killDbConnections <- function () {
  
  all_cons <- dbListConnections(MySQL())
  
  print(all_cons)
  
  for(con in all_cons)
    +  dbDisconnect(con)
  
  print(paste(length(all_cons), " connections killed."))
  
}
killDbConnections()
