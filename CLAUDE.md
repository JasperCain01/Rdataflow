# Rdataflow

## Purpose

Rdataflow is an R package that, given an R object, reverse-engineers the data imports and transformations that were applied to produce it. The output is a flow diagram visualizing all steps — from source data through each transformation — that led to the object.

## Design guidance

- Use tidyverse packages and syntax wherever possible (e.g. `dplyr`, `tidyr`, `purrr`, `readr`, `tibble`, `stringr`, `magrittr`/native pipe).
- Follow the style and principles laid out in *R for Data Science* (https://r4ds.hadley.nz/). When in doubt on API design, naming, or idiom, defer to that book.
- all code chunks should be commented explaining what they do and aim to achieve

## Git workflow

- Work on branches prefixed with `claude_` (e.g. `claude_parse-script`, `claude_flowchart-output`).
- Make **small, frequent commits** as work progresses — do not batch large changes into one commit.
- Individual commits do **not** require user approval.
- **Always ask** before merging a `claude_` branch into `main`.
