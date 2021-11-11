library(tidyverse)
library(pdftools)

# List of titles used to detect individual names
titles <-
  c("Mr", "Ms", "Miss", "Mrs", "Sr", "Sra", "Mme", "M", "H.E. Mr", "H.E. Ms",
    "S.E. M", "S.E. Sr", "S.E. Sra", "S.E. Mme", "Ind", "Dr")

new_data <-
  pdf_data(Sys.getenv("PDF_PATH")) |>
  map_dfr(bind_rows, .id = "page") |>
  mutate(
    page = as.integer(page),
    offset = y - lag(y)) |>
  filter(page > 1, !text %in% c("COP26.PLOP", "Parties"), !str_detect(text, "^[:digit:]{1,}$")) |>
  group_by(page) |>
  mutate(row = row_number()) |>
  ungroup() |>
  mutate(
    new_participant_flag = if_else(height == 8 & offset >= 18, 1, 0),
    new_participant_flag = if_else(row == 1 & !str_detect(text, str_c("^(", str_c(titles, collapse = "|"), ")(\\.| )")), 0, new_participant_flag),
    id = cumsum(new_participant_flag)) |>
  group_by(id, height, y) |>
  summarise(text = str_c(text, collapse = " ")) |>
  mutate(
    delegation = if_else(height == 10, text, NA_character_),
    name = first(text),
    affiliation = last(text)) |>
  ungroup() |>
  fill(delegation) |>
  filter(name != delegation) |>
  select(id, delegation, name, affiliation) |>
  distinct()

data <-
  pdf_text(Sys.getenv("PDF_PATH")) |>
  str_split("\n") |>
  map_dfr(tibble, .id = "page") |>
  rename(text = `<chr>`) |>
  mutate(text = str_trim(text)) |>
  filter(
    page != "1",
    !str_detect(text, "COP\\d{2}.PLOP"),
    !str_detect(text, "^[:digit:]{1,}$"),
    text != "Parties") |>
  mutate(
    salutation = str_extract(text, str_c("^(", str_c(titles, collapse = "|"), ")\\.?(?= )"))) |>
  mutate(
    category = case_when(
      !is.na(salutation) ~ "name",
      lag(text) == "" & (lead(text) == "" | lead(text, 2) == "") & str_detect(text, "^[A-Z].*")~ "delegation",
      TRUE ~ NA_character_)) |>
  mutate(
    delegation = case_when(
      category == "delegation" & lead(text) == "" ~ text,
      category == "delegation" & lead(text) != "" ~ str_c(text, lead(text), sep = ""),
      TRUE ~ NA_character_),
    new_participant_flag = case_when(
      category == "name" ~ 1L,
      TRUE ~ 0L),
    id = cumsum(new_participant_flag)) |>
  fill(delegation) |>
  filter(
    text != "",
    is.na(category) | category != "delegation") |>
  group_by(id) |>
  mutate(
    affiliation = if_else(category == "name", last(text), NA_character_),
    affiliation = if_else(text == affiliation, NA_character_, affiliation)) |>
  filter(text == first(text)) |>
  ungroup() |>
  select(id, delegation, name = text, affiliation)

write_csv(data, "participants.csv")