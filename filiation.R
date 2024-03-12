conslyon

dfi <- read_csv("dfi_lyon.csv") |>
  mutate(meres = str_remove_all(meres, "[^a-zA-Z0-9]")) |>
  filter(as.numeric(validation) > 20100000)

conslyon2 <-
  conslyon |>
  select(parc_sufcad1:parc_sufcad3, pres_parc)

conslyon2 <- 
  conslyon2 |>
  left_join(
    select(dfi,
           meres,
           filles),
    by = c("parc_sufcad1" = "meres")
  ) |>
  rename("filles1" = "filles") |>
  left_join(
    select(dfi,
           meres,
           filles),
    by = c("parc_sufcad2" = "meres")
  ) |>
  rename("filles2" = "filles") |>
  left_join(
    select(dfi,
           meres,
           filles),
    by = c("parc_sufcad3" = "meres")
  ) |>
  rename("filles3" = "filles") 



conslyon2 <-
  conslyon2 |>
  mutate(
    across(.cols = filles1:filles3,
           .fns = ~ str_remove(.x, "\\{")),
    across(.cols = filles1:filles3,
           .fns = ~ str_remove(.x, "\\}")),
    across(.cols = filles1:filles3,
           .fns = ~ str_split(.x, ","))
  )
    
    
    ,
         filles1 = str_remove(filles1, "\\}"),
         filles1 = str_split(filles1, ","))


conslyon2$filles1[[1]] %in% dvflyon$parc
         

