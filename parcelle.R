# Package names
packages <- c("httr2", "tidyverse", "jsonlite", "sf", "geojsonsf", "mapsf")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Packages loading
invisible(lapply(packages, library, character.only = TRUE))




# API Dido pour récupérer les données de sitadel
# Formation de l'URL
# Base
base <- "https://data.statistiques.developpement-durable.gouv.fr:443/dido/api/v1/datafiles/8b35affb-55fc-4c1f-915b-7750f974446a/csv"


# Construction de l'url
url <- 
  base |>
  url_parse() |>
  list_merge(
    query =
      list(
        # Paramètres obligatoires
        withColumnName = "true",
        withColumnDescription = "false",
        withColumnUnit = "false",
        # Département du rhône
        COMM = "contains:69123"
      )
  ) |>
  url_build()

# Collecte des données
# Création d'une requête
req <- request(url)
# Lancement de la requête
resp <- req_perform(req)
# Lecture du contenu et création du data.frame
conslyon <- resp |> 
  resp_body_string() |>
  read_csv2()

# Uniquement les promoteurs
conslyon <-
  conslyon %>%
  filter(str_starts(CAT_DEM, "3") &
           NB_LGT_COL_HORS_RES > 4)

# Identifiants
conslyon <-
  conslyon %>%
  rowid_to_column(var = "id")

#### DVF ####
# Base de l'url
dvf_open <- "https://apidf-preprod.cerema.fr/dvf_opendata/geomutations/"

# Construction de l'url
# Partant de la base
url <- dvf_open |> 
  # Décomposée sous un format liste
  url_parse() |>
  # À laquelle on rajoute les éléments supplémentaires
  list_merge(
    # Les requêtes
    query = 
      list(
        code_insee = paste0(69381:69389, collapse = ","),
        page_size = "1000",
        # Les appartements uniquement
        codtypbien = "121,101"
      ),
  ) |>
  # Compilation des éléments de l'url
  url_build()

# Collecte des données
# Création d'une première requête des 500 entrées de la première page
req <- request(url)
resp1 <- req_perform(req) |>
  resp_body_json()

# Création d'une liste d'url pour toutes les pages
urls <- paste0(url,
               "&page=",
               seq(
                 # De la première page
                 1,
                 # À la dernière (nb entités/entités par page + 1)
                 (resp1$count/500)+1))

# Créations des requêtes
reqs <- 
  map(.x = urls,
      .f = ~ request(.))
# Téléchargement des données
resps <- 
  req_perform_parallel(reqs,
                       pool = 10)

reqs_c <- split(reqs, seq(1, 162, 20))
resps <- 
  map(.x = reqs_c,
      .f = ~ req_perform_parallel(.))
resps <- unlist(resps, recursive = F)


dvflyon <- 
  resps |>
  map_chr(resp_body_string) |>
  str_c() |>
  geojson_sf() 

dvflyon <-
  dvflyon %>%
  mutate(parc = str_remove_all(l_idpar, "[^a-zA-Z0-9]"))

#### Conslyon ####
conslyon <- 
  conslyon %>%
  mutate(
    base = 
      paste0(
        DEP_CODE, 
        str_sub(NUM_DAU, 4, 6),
        "000"),
    sufcad1 =
      paste0(
        SEC_CADASTRE1,
        str_pad(NUM_CADASTRE1, width = 4, pad = "0")
      ),
    sufcad2 =
      paste0(
        SEC_CADASTRE2,
        str_pad(NUM_CADASTRE2, width = 4, pad = "0")
      ),
    sufcad3 =
      paste0(
        SEC_CADASTRE3,
        str_pad(NUM_CADASTRE3, width = 4, pad = "0")
      ),
    across(
      .cols = sufcad1:sufcad3,
      .f = ~ paste0(base, .),
      .names = "parc_{.col}"
    ))


## Présence de l'identifiant
conslyon <-
  conslyon %>%
  mutate(
    pres_parc = 
      parc_sufcad1 %in% dvflyon$parc |
      parc_sufcad2 %in% dvflyon$parc |
      parc_sufcad3 %in% dvflyon$parc
  )


### Adresse ####
# Information sur l'adresse
# Première transformation
conslyon <-
  conslyon %>%
  mutate(
    adresse = 
      if_else(
        # Si le numéro est dans le libellé
        str_detect(ADR_LIBVOIE_TER, "\\d"),
        # Alors garde seulement le libellé
        ADR_LIBVOIE_TER,
        # Sinon cherche le numéro dans la colonne
        paste0(ADR_NUM_TER, " ", ADR_LIBVOIE_TER)
      ),
    # On ne cherche pas les constructions sans numéro de rue
    adresse = if_else(str_detect(adresse, "NA"), NA, adresse)
  )

# Construction de l'url
# Partant de la base
# Base de l'url
conslyon <-
  conslyon %>%
  mutate(
    base_url_adr = "https://wxs.ign.fr/essentiels/geoportail/geocodage/rest/0.1/search?q=",
    url_parc_adr = 
      paste0(
        base_url_adr,
        adresse,
        "&index=address",
        "&limit=1",
        "&citycode=",
        COMM,
        "&type=housenumber"
      )
  )


# Création des requêtes
reqs <- 
  map(conslyon$url_parc_adr,
      url_parse) |>
  map(url_build) |>
  map(request)

# Téléchargement des données
resps <- req_perform_parallel(reqs,
                              on_error = "continue")
# Récupération des adresses

httr2class <- 
  function(resp) {
    class(resp)[[1]] != "httr2_response"
  }


resps <- 
  resps |>
  map_if(
    .p = httr2class,
    .f = pluck("resp")) |>
  map(resp_body_string) %>%
  tibble() |>
  rowid_to_column() |>
  filter(!str_detect(., '"code":400') &
           !str_detect(., "\\[\\]"))

adresses <- resps$. |>
  str_c() |>
  geojson_sf()

adresses$id <- resps$rowid


# Coordonnées
lonlat <-
  st_coordinates(adresses)

### Parcelles
urls <- paste0("https://data.geopf.fr/geocodage/reverse",
               "?index=parcel",
               "&lon=",
               lonlat[,1],
               "&lat=",
               lonlat[,2],
               "&limit=3")

reqs <- 
  map(.x = urls,
      .f = ~ request(.))
resps <- req_perform_parallel(reqs)

parcelles <-
  resps |>
  map(resp_body_string) %>%
  map(geojson_sf) |>
  list_rbind(names_to = "id") %>%
  mutate(parc_adr = 
           paste0(
             departmentcode,
             districtcode,
             oldmunicipalitycode,
             section,
             number
           ))

parcelles <- parcelles |>
  select(id, parc_adr) |>
  mutate(rank = row_number(),
         .by = id) |>
  pivot_wider(values_from = parc_adr,
              names_from = rank,
              names_prefix = "parc_adr")


adresses <- 
  adresses |>
  tibble() |>
  select(id) |>
  cbind(
    select(
      parcelles,
      parc_adr1:parc_adr3
    )
  )


conslyon <-
  conslyon |>
  left_join(
    adresses
  )

head(conslyon |>
       filter(!pres_parc) |>
       select(parc_adr1:parc_adr3, parc_sufcad1:parc_sufcad3),
     n = 50)

t <-
  conslyon %>%
  filter(parc_sufcad1 == "69381000AK0072")




#### Cadastre ####



## Jointure
conslyonj <-
  conslyon %>%
  filter(pres_parc) %>%
  select(parc_sufcad1:parc_sufcad3, DENOM_DEM, NB_LGT_COL_HORS_RES) 

dvflyonj <- 
  dvflyon %>%
  select(parc, valeurfonc, vefa, libtypbien)

imm <- 
  rbind(
    left_join(
      conslyonj,
      dvflyonj,
      by = c("parc_sufcad1" = "parc"),
      keep = T
    ),
    left_join(
      conslyonj,
      dvflyonj,
      by = c("parc_sufcad2" = "parc"),
      keep = T
    ),
    left_join(
      conslyonj,
      dvflyonj,
      by = c("parc_sufcad3" = "parc"),
      keep = T
    )
  )


imm <-
  imm %>%
  mutate(ndvf = n(),
         .by = parc
  )


t <- filter(conslyon, !pres_parc)





