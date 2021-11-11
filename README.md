# COP26 participants analysis

This repository is for scripts related to a planned analysis of the list of participants at 26th UNFCCC Conference of the Parties (COP26). For the [actual analysis](https://www.bbc.co.uk/news/science-environment-59199484), automated matching was used as a guide by the researchers who manually checked the lists. 

## Background

UNFCCC will release a list of participants at some point before COP26, likely via [this page](https://unfccc.int/documents?search2=&search3=participants). The list will be in one of two formats: provisional (appears to be an export from Microsoft Word—[example](https://unfccc.int/sites/default/files/resource/SB2021%28May-June%29.pdf)) or final (professionally typeset in columns—[example](https://unfccc.int/sites/default/files/resource/sb2021_inf01.pdf)).

The provisional format can be parsed quite easily into participant delegations (e.g. United Kingdom), names (e.g. Louis Goddard), titles (e.g. Senior Data Investigations Adviser) and affiliations (e.g. Global Witness) using line breaks. The final format isn't possible to parse fully as the narrow columns cause a lot of wrapped lines, which can be ambiguous (e.g. "Louis Goddard\\nSenior Data\\nInvestigations Adviser\\nGlobal Witness"). To get around this, we use an entity recognition model trained on data from COP25 to extract affiliations from the full line of text.

Once the data has been extracted from the PDFs, a further script attempts to match the participants against lists of previous participants linked to the fossil fuel industry, producing a list of matches as the ultimate output.

## Requirements

AWS CLI; R and the packages `tidyverse`, `pdftools`, `glue`, `jsonlite`, `googlesheets4`, `rvest`, `fuzzyjoin`.

## Usage

Firstly, set all the environment variables in `.Renviron` as described below.

| Variable | Value |
|----------|-------|
| `LIST_TYPE` | "Provisional" or "Final" |
| `PDF_PATH` | Path to source PDF file, including filename |
| `TXT_PATH` | Path for text output, including filename |
| `CSV_PATH` | Path for CSV output, including filename |
| `S3_PATH` | Path for input and output on S3—must be a directory, including a trailing slash |
| `ENTITY_RECOGNIZER_ARN` | ARN for the relevant AWS Comprehend custom entity recognizer |
| `DATA_ACCESS_ROLE_ARN` | ARN for a role with relevant permissions for running the recognizer and accessing the S3 bucket given above |
| `MIN_MATCH_CHAR` | Minimum length of string to consider for fuzzy matches (used to exclude very short strings like "BP") |
| `MAX_MATCH_N` | Maximum number of matches to consider across the whole data set (used to exclude very common strings like "Ministry of Finance") |
| `NGO_SHEET_ID` | Google Sheets ID for the admitted NGOs classification sheet |
| `COP25_SHEET_ID` | Google Sheets ID for the COP25 affiliations classification sheet |
| `RESULTS_SHEET_ID` | Google Sheets ID for the output produced by these scripts |
| `RESULTS_SHEET_NAME` | Name of the sheet (i.e. tab) to use for output |

Now run the numbered R scripts in this repository sequentially. This can be done manually or by executing `./run.R`. A description of each script is given below. Note that step 2 can be run independently of step 1, for example if UNFCCC suddenly issues the list in a new format, as long as a CSV file of participants exists at the location specified in `CSV_PATH` in the correct format.

### `01-extract-participants.R`

Extracts participant list from the PDF given as `PDF_PATH` and saves the results to `TXT_PATH` and `CSV_PATH`. In the case of lists in the final format, also runs an entity recognition job on the text file via AWS Comprehend and patches the results into the CSV file.

### `02-match-to-classifications.R`

Matches the participant data in the CSV file to data from three sources: [Climate Action 100+](https://www.climateaction100.org/whos-involved/companies/)'s list of fossil fuel companies; a list of participants at COP25 in which fossil fuel links have been identified; a list of admitted NGOs in which fossil fuel links have been identified. Also combines the resulting matches and writes them to a Google Sheet.
