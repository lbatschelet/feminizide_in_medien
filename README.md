# Media reporting on feminicide in Swiss news

This project analyzes discursive change in Swiss media reporting on feminicide. It uses the Swissdox@LiRI API to retrieve German-language articles and applies descriptive statistics and diachronic word2vec models to study shifts from euphemistic framing to more explicit naming.

A rendered version of the script is available at https://lbatschelet.github.io/feminizide_in_medien/.

## Access to Swissdox@LiRI

Swissdox@LiRI access is available to members of most Swiss universities. You can log in and manage access via the Swissdox portal:  
https://swissdox.linguistik.uzh.ch/

## Setup

1) Install [**Quarto**](https://quarto.org/) (required to render the main notebook).  
2) Create a local environment file with your API credentials.  
3) Ensure `quanteda` and `word2vec` are running with multithreading enabled (see note below).

### Local environment file

Copy `local.env.example` to `local.env` and add your Swissdox credentials:

```
cp local.env.example local.env
```

### Data directory note

The `/data` directory is intentionally empty in the repository. The raw Swissdox exports are extremely large (multi‑GB) and exceed GitHub limits. With API access, you can reproduce the calls locally and rebuild the datasets.


> [!IMPORTANT]
> **Performance note**
>
> We strongly recommend enabling multithreading for both `quanteda` and `word2vec`. On some systems, `quanteda` requires additional configuration to allow parallel execution; otherwise tokenization and DFM construction can take hours. The notebook includes a short check and a link to the official setup guide.

## Main analysis

The main analysis is in `Notebook.qmd` (Quarto). Render with:

```
quarto render Notebook.qmd
```

## Files

- `Notebook.qmd`: Main analysis notebook (Quarto).
- `swissdox_api.R`: Swissdox API wrapper helper.
- `load_raw_tsv.R`: Helper to reload TSV results when caching failed (not used in the final notebook, but helpful during data recovery).
- `german_stopwords_full.txt`: Curated German stopword list from the solariz repository.  
  Source: https://github.com/solariz/german_stopwords/tree/master
- `local.env.example`: Template for API credentials (copy to `local.env`).
- `library.bib`: References used in the notebook.