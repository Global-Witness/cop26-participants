library(tidyverse)
library(pdftools)
library(glue)
library(jsonlite)

# Extract text from PDF in provisional format
extract_provisional <- function(pdf_path, csv_path, txt_path, titles) {
  data <- pdf_text(pdf_path) |>
    str_split("\n") |>
    map_dfr(tibble, .id = "page") |>
    rename(text = `<chr>`) |>
    mutate(text = str_trim(text)) |>
    # Remove first page, page headers (COP25.PLOP) and page numbers
    filter(page != "1") |>
    filter(text != "COP25.PLOP") |>
    filter(text != "Parties") |> 
    filter(str_detect(text, "^[:digit:]{1,}$", negate = TRUE)) |>
    # Create a new column showing the category of information included in each
    # line and make a first attempt at identifying participant names and
    # affiliations, based on what precedes and follows them. Also create a
    # column for each individual's salutation.
    mutate(
      category = case_when(
        text != "" & lag(text) == "" ~ "name",
        text != "" & lead(text) == "" ~ "affiliation",
        TRUE ~ NA_character_),
      salutation = str_extract(text, str_c("^(", str_c(titles, collapse = "|"), ")\\.?(?= )"))) |>
    # Do the same for delegation names (countries and other bodies)
    mutate(
      category = case_when(
        text != "" & lag(text) %in% c("", "Parties") & lead(text) == "" & is.na(salutation) ~ "delegation",
        is.na(category) &
          ((lag(category) == "name" & lead(category) == "affiliation") |
           (lag(category) == "name" & lead(category, 2) == "affiliation") |
           (lag(category, 2) == "name" & lead(category) == "affiliation")) ~ "title",
        TRUE ~ category),
      delegation = if_else(category == "delegation", text, NA_character_)) |>
    # 'Fill' the `delegation` column downwards so that it applies to all participants
    # in a given delegation
    fill(delegation) |>
    # Assign an ID to each participant by adding a 1 where a new participant is
    # detected, then doing a cumulative sum on the column
    mutate(
      new_participant_flag = case_when(
        category == "name" ~ 1L,
        TRUE ~ 0L),
      id = cumsum(new_participant_flag)) |>
    # Fill the `salutation` column and create a `title` column for each
    # participant, where applicable
    group_by(id) |>
    fill(salutation) |>
    mutate(
      title = na_if(
        str_c(na.omit(text[category == "title"]), collapse = ", "),
        "")) |>
    ungroup() |>
    # Filter the data to rows where a category has been identified, excluding a
    # few stray rows that are miscategorised (titles, page numbers, &c.)
    filter(
      !is.na(delegation),
      category %in% c("name", "affiliation"),
      str_detect(text, "[A-Z][a-z]")
      ) |>
    # Transform the data into 'wide' format, with a column for each category
    pivot_wider(
      id_cols = c(id, delegation, salutation, title),
      names_from = category,
      values_from = text) |>
    # 'Reset' the participant ID column to get rid of any gaps caused by the last
    # filter
    mutate(id = row_number()) |>
    # Reorder the columns
    select(id, delegation, salutation, name, title, affiliation)
  
  # Save a copy of the final data as a CSV
  write_csv(data, csv_path)
  
  # Save a copy of the data as a text file, with one row per participant
  data |>
    mutate(
      text = case_when(
        !is.na(affiliation) & !is.na(title) ~ str_c(name, title, affiliation, sep = " "),
        !is.na(affiliation) & is.na(title) ~ str_c(name, affiliation, sep = " "),
        is.na(affiliation) & !is.na(title) ~ str_c(name, title, sep = " "),
        TRUE ~ name)) |>
      pull(text) |>
      write_lines(txt_path)
}

# Extract text from PDF in final format
extract_final <- function(pdf_path, csv_path, txt_path, titles) {
  raw_data <- pdf_data(pdf_path) |>
    map_dfr(bind_rows, .id = "page") |>
    mutate(page = as.integer(page)) |>
    # Assign column numbers using text's x position
    mutate(
      column = case_when(
        x < 220 ~ 1,
        between(x, 220, 390) ~ 2,
        x > 390 ~ 3)) |>
    # Remove extraneous content
    filter(
      page >= 2,
      between(y, 60, 800)) |>
    # Combine text into lines
    group_by(page, column, height, y) |>
    summarise(text = str_c(text, collapse = " "), .groups = "drop") |>
    arrange(page, column, y) |>
    # Identify parties using line height and titles
    mutate(
      delegation =
        case_when(
          lag(height) == 10 & height == 10 ~ str_c(lag(text), text, sep = " "),
          height == 10 ~ text,
          TRUE ~ NA_character_) |>
        str_remove(" \\(continued\\)"),
      salutation = str_extract(text, str_c("^(", str_c(titles, collapse = "|"), ")\\.?(?= )"))) |>
    fill(delegation) |>
    filter(!is.na(delegation), height == 8) |>
    # Assign an ID number to each participant
    mutate(
      new_participant_flag = case_when(
        !is.na(salutation) ~ 1L,
        TRUE ~ 0L),
      id = cumsum(new_participant_flag))
  
  # Save a copy of the data as a text file, with one row per participant
  raw_data |>
    group_by(id) |>
    summarise(text = str_c(text, collapse = " "), .groups = "drop") |>
    pull(text) |>
    write_lines(txt_path)
  
  # Try to detect affiliations using a very rough heuristic
  raw_data |>
    group_by(id, height) |>
    mutate(
      name = text,
      title = case_when(
        !is.na(salutation) &
          is.na(lead(salutation)) &
          str_detect(lead(text), "( (for|and|of)|,)$") ~ str_c(lead(text), lead(text, 2), sep = " "),
        !is.na(salutation) &
          is.na(lead(salutation)) ~ lead(text),
        TRUE ~ NA_character_),
      affiliation = str_remove(str_c(text, collapse = " "), fixed(str_c(name, title, sep = " "))) |>
        str_trim() |>
        na_if("")) |>
    ungroup() |>
    filter(!is.na(salutation)) |>
    # Order data and save as a CSV
    select(id, delegation, salutation, name, title, affiliation) |>
    write_csv(csv_path)
}

recognize_entities <- function(csv_path, txt_path, s3_path, entity_recognizer_arn, data_access_role_arn, titles) {
  # Copy text file to S3
  system(glue("aws s3 cp {txt_path} {s3_path}{txt_path}"))
  
  # Start custom entity recognition job on text file
  system(glue("
    aws comprehend start-entities-detection-job \\
      --entity-recognizer-arn \"{entity_recognizer_arn}\" \\
      --job-name COPDelegates-$(date '+%s') \\
      --data-access-role-arn \"{data_access_role_arn}\" \\
      --language-code en \\
      --input-data-config \"S3Uri={s3_path}{txt_path}\" \\
      --output-data-config \"S3Uri={s3_path}\" \\
      --region eu-west-1
  "))
  
  # Check in on the job every few seconds until it's complete
  job <- list(JobStatus = "SUBMITTED")
  
  while(job$JobStatus %in% c("SUBMITTED", "IN_PROGRESS")) {
    jobs <- fromJSON(
      system("aws comprehend list-entities-detection-jobs", intern = TRUE),
      simplifyVector = FALSE)
    
    job <- tail(jobs$EntitiesDetectionJobPropertiesList, 1)[[1]]
    
    Sys.sleep(10)
  }
  
  if(job$JobStatus == "COMPLETED") {
    # Copy the results back from S3
    system(glue("aws s3 cp {job$OutputDataConfig$S3Uri} ."))
    
    # Extract and parse the results
    results <-
      system("tar -xOvf output.tar.gz output", intern = TRUE) |>
      str_split("\n") |>
      map(fromJSON, simplifyVector = FALSE) |>
      map_dfr(function(result) {
        tibble(
          id = result$Line + 1,
          affiliation = tail(result$Entities, 1))
      }) |>
      rowwise() |>
      mutate(affiliation = affiliation$Text) |>
      ungroup() |>
      # Remove salutations
      filter(!str_detect(affiliation, str_c("^(", str_c(titles, collapse = "|"), ")\\.$")))
    
    # Patch the existing CSV file with the new affiliations
    read_csv(csv_path) |>
      select(-affiliation) |>
      left_join(results, by = "id") |>
      write_csv(csv_path)
    
  } else {
    stop(glue("{job$JobStatus} {job$Message}"))
  }
}

# List of titles used to detect individual names
titles <-
  c("Mr", "Ms", "Miss", "Mrs", "Sr", "Sra", "Mme", "M", "H.E. Mr", "H.E. Ms",
    "S.E. M", "S.E. Sr", "S.E. Sra", "S.E. Mme")

# Run the functions above
if(Sys.getenv("LIST_TYPE") == "Provisional") {
  
  message(glue("Extracting data from PDF in provisional format and saving to {Sys.getenv(\"CSV_PATH\")} and {Sys.getenv(\"TXT_PATH\")}..."))
  extract_provisional(
    pdf_path = Sys.getenv("PDF_PATH"),
    csv_path = Sys.getenv("CSV_PATH"),
    txt_path = Sys.getenv("TXT_PATH"),
    titles = titles)
 
} else if(Sys.getenv("LIST_TYPE") == "Final") {
  
  message(glue("Extracting data from PDF in final format and saving to {Sys.getenv(\"CSV_PATH\")} and {Sys.getenv(\"TXT_PATH\")}..."))
  extract_final(
    pdf_path = Sys.getenv("PDF_PATH"),
    csv_path = Sys.getenv("CSV_PATH"),
    txt_path = Sys.getenv("TXT_PATH"),
    titles = titles)
  
  message(glue("Patching CSV file with affiliations generated using AWS Comprehend..."))
  recognize_entities(
    csv_path = Sys.getenv("CSV_PATH"),
    txt_path = Sys.getenv("TXT_PATH"),
    s3_path  = Sys.getenv("S3_PATH"),
    entity_recognizer_arn  = Sys.getenv("ENTITY_RECOGNIZER_ARN"),
    data_access_role_arn = Sys.getenv("DATA_ACCESS_ROLE_ARN"),
    titles = titles)
  
} else {
  stop("Please set the `LIST_TYPE` environment variable to either \"Provisional\" or \"Final\".")
}
