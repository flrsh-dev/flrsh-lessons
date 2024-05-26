library(dplyr)
library(lightparser)

# in_file <- "duckdb-deep-dive/chapter2.qmd"

# parse_chapter(in_file)

# Each exercise is a single string
# The first one contains the yaml front matter. It can be ommitted.
# The exercises is a single string
#
# Rendering process:
# lightparser -> rmarkdown -> pandoc -> downlit
parse_exercise <- function(exercise) {
  stopifnot(
    "Must be length 1" = length(exercise) == 1,
    "Must be a character" = is.character(exercise)
  )

  # create a temp Rmd file to render
  tmp <- tempfile(fileext = ".Rmd")

  # write to the temp file
  brio::write_file(exercise, tmp)

  # create a temporary markdown file to store the output
  out_md <- tempfile(fileext = ".md")

  # set the output character
  knitr::opts_chunk$set(comment = "#>")

  # render the section alone
  rmarkdown::render(
    tmp,
    rmarkdown::github_document(),
    out_md
  )

  # create a temporary HTML file to convert to with pandoc
  tmp_out <- tempfile(fileext = ".html")

  # convert to html
  pandoc::pandoc_convert(
    out_md,
    to = "html",
    output = tmp_out,
    standalone = FALSE
  )

  # apply auto-linking to the rendered html
  downlit::downlit_html_path(
    tmp_out,
    tmp_out,
    downlit::classes_pandoc()
  )

  # remove inserted tags from downlit
  lines <- brio::read_lines(tmp_out)
  paste(lines[3:(length(lines) - 1)], collapse = "\n")
}

# Processes a whole qmd file and creates a list with one element `content`
# which we will need to apply to more than one chapter at a time
parse_chapter <- function(in_file) {
  front <- rmarkdown::yaml_front_matter(in_file)

  chapter <- split_to_tbl(in_file)

  exercise_titles <- chapter |>
    # all exercises
    filter(heading_level == 1L) |>
    pull(heading) |>
    # remove exercise class indicator
    stringr::str_remove_all("\\{.exercise\\}|\\{.reading\\}") |>
    stringr::str_squish()

  exercises <- chapter |>
    # this filters our data to start at the frist instance of a heading
    # NOTE this assumes that the first heading is a level one—which it should be!!
    slice(-(1:(which(type != "heading")[1] + 1))) |>
    mutate(
      is_exercise_heading = coalesce(heading_level == 1, !is.na(heading_level)),
      section = cumsum(is_exercise_heading)
    ) |>
    group_by(section) |>
    group_split() |>
    lapply(combine_tbl_to_file) |>
    vapply(parse_exercise, character(1))

  exercises <- Map(
    \(.title, .slug) {
      list(title = .title, slug = .slug)
    },
    exercise_titles,
    heck::to_kebab_case(exercise_titles)
  ) |> unname()

  list(
    chapter_title = front$title,
    exercises = exercises
  )
}

parse_course <- function(chapters) {
  chapts <- lapply(chapters, parse_chapter)
  chapts
}

r <- parse_course(
  c(
    "duckdb-deep-dive/chapter1.qmd",
    "duckdb-deep-dive/chapter2.qmd",
  )
)

# create json, this can be copied into the Rust source as a const
# we can think about automating to
list(chapters = r) |>
  jsonify::to_json(unbox = T)
