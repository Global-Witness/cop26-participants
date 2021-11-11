library(tidyverse)
library(googlesheets4)
library(fuzzyjoin)
library(rvest)
library(glue)

gs4_auth()

participants <- read_csv(Sys.getenv("CSV_PATH"))

classifications_ca100plus <-
  tibble(
    affiliation_ca100plus =
      map(seq(1, 7), function(page) {
        read_html(str_c("https://www.climateaction100.org/whos-involved/companies/page/", page)) |>
          html_nodes("h3.card-header-title") |>
          html_text() |>
          str_trim()
      }) |>
      unlist(),
    ff_ca100plus = "Yes")

classifications_ngo <-
  read_sheet(Sys.getenv("NGO_SHEET_ID"), sheet = 1) |>
  select(
    delegation_ngo = `Official Name`,
    ff_delegation = `Count as FF lobbyists`,
    ff_director = `Org with fossil fuel member(s) / FF rep on board of directors`,
    ff_employees = `Count direct employees as FF lobbyists`) |>
  filter(!is.na(delegation_ngo))

classifications_cop25 <-
  read_sheet(Sys.getenv("COP25_SHEET_ID"), sheet = 1, skip = 1) |>
  select(
    affiliation_cop25 = `Affiliation original`,
    ff_affiliation = `FF affiliation`) |>
  filter(
    !is.na(affiliation_cop25),
    affiliation_cop25 != "NA")

matches_ca100plus <-
  stringdist_inner_join(
    x = participants |>
      distinct(id, affiliation) |>
      filter(!is.na(affiliation)),
    y = classifications_ca100plus,
    by = c("affiliation" = "affiliation_ca100plus"),
    distance_col = "string_dist") |>
  select(-affiliation) |>
  group_by(id) |>
  mutate(n_matches_ca100plus = n()) |>
  ungroup() |>
  filter(
    (n_matches_ca100plus <= as.integer(Sys.getenv("MAX_MATCH_N")) &
    nchar(affiliation_ca100plus) >= as.integer(Sys.getenv("MIN_MATCH_CHAR"))) |
    string_dist == 0
  ) |> 
  select(-string_dist)

matches_ngo <-
  stringdist_inner_join(
    x = participants |>
      distinct(id, delegation) |>
      filter(!is.na(delegation)),
    y = classifications_ngo,
    by = c("delegation" = "delegation_ngo"),
    distance_col = "string_dist") |>
  select(-delegation) |>
  group_by(id) |>
  mutate(n_matches_ngo = n()) |>
  ungroup() |>
  filter(
    (n_matches_ngo <= as.integer(Sys.getenv("MAX_MATCH_N")) &
     nchar(delegation_ngo) >= as.integer(Sys.getenv("MIN_MATCH_CHAR"))) |
    string_dist == 0
  ) |> 
  select(-string_dist)

matches_cop25 <-
  stringdist_inner_join(
    x = participants |>
      distinct(id, affiliation) |>
      filter(!is.na(affiliation)),
    y = classifications_cop25,
    by = c("affiliation" = "affiliation_cop25"),
    distance_col = "string_dist") |>
  select(-affiliation) |>
  group_by(id) |>
  mutate(n_matches_cop25 = n()) |>
  ungroup() |>
  filter(
    (n_matches_cop25 <= as.integer(Sys.getenv("MAX_MATCH_N")) &
     nchar(affiliation_cop25) >= as.integer(Sys.getenv("MIN_MATCH_CHAR"))) |
    string_dist == 0
  ) |> 
  select(-string_dist)

participants_matched <- participants |>
  left_join(matches_cop25, by = "id") |>
  left_join(matches_ngo, by = "id") |>
  left_join(matches_ca100plus, by = "id") |>
  rowwise() |>
  mutate(ff_any = if_else(
    "Yes" %in% c(ff_affiliation, ff_delegation, ff_director, ff_employees, ff_ca100plus),
    "Yes",
    "No")) |>
  ungroup()

# pipe separate duplicate values
participants_pipe_separated <- participants_matched |> 
  group_by(id) |> 
  summarise_all(function(x) {
    if (length(unique(x)) == 1) return(first(x))
    return(str_c(x, collapse = " | "))
  })

# subset and rename columns
participants_renamed <- participants_pipe_separated |> 
  select(
    ID = id,
    Delegation = delegation,
    Salutation = salutation,
    Name = name,
    Title = title,
    Affiliation = affiliation,
    `FF any` = ff_any,
    `Affiliation (COP25)` = affiliation_cop25,
    `FF affiliation` = ff_affiliation,
    `COP25 matches (n)` = n_matches_cop25,
    `Delegation (NGO)` = delegation_ngo,
    `FF delegation` = ff_delegation,
    `FF director` = ff_director,
    `FF employees` = ff_employees,
    `NGO matches (n)` = n_matches_ngo,
    `Affiliation (CA100+)` = affiliation_ca100plus,
    `FF CA100+` = ff_ca100plus,
    `CA100+ matches (n)` = n_matches_ca100plus
  ) |>
  rename_with(
    ~ str_c("Matched ", .x), 
    .cols = `FF any`:`CA100+ matches (n)`
  )

# write to google sheets
write_sheet(participants_renamed, Sys.getenv("RESULTS_SHEET_ID"), sheet = Sys.getenv("RESULTS_SHEET_NAME"))
