library(httr)
library(jsonlite)
library(tidyverse)



url <- "https://wxs.ign.fr/essentiels/geoportail/geocodage/rest/0.1/search?q=73 Avenue de Paris Saint-Mandé"

res <- httr::content(
  httr::GET("https://wxs.ign.fr/essentiels/geoportail/geocodage/rest/0.1/search?q=37%rue%d'%Ivry%Lyon"),             # url correspond à l'url à interroger
  as = "text",                # type de la sortie renvoyée
  httr::content_type_json(),  # type de la réponse de l'url
  encoding = "UTF-8"          # encodage de la réponse de l'url
)



url <- 
"https://data.statistiques.developpement-durable.gouv.fr:443/dido/api/v1/datafiles/8b35affb-55fc-4c1f-915b-7750f974446a/csv?millesime=2024-01&withColumnName=true&withColumnDescription=false&withColumnUnit=false&DEP_CODE=contains%3A69"


res <- httr::content(
  httr::GET(url),             # url correspond à l'url à interroger
  as = "raw",                # type de la sortie renvoyée
  type = httr::content_type("text/csv"),  # type de la réponse de l'url
  encoding = "UTF-8"          # encodage de la réponse de l'url
)
writeBin(res, "data.csv")

dat = read_csv2("data.csv")
