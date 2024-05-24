#' Functions to process a single chapter of a course
#' A chapter is stored in a rendered QMD file

#' Given an xml nodeset for a code-exercise, extract the content into
#' a list of 5 elements. This can be serialized to JSON for ingestion
#' into the webr-course code exercise structure.
#'
#' Since this frees memory, it is prone to corruption and crashing.
#' The element that is processed _must_ be its own object, not an item
#' in a list otherwise it will crash. That's fine. We can use intermediate
#' objects in that case.
#'
#' Imports:
#'  - brio
#'  - selectr
#'  - rvest
#'  - xml2
#'  - jsonify
#'
#' TODO:
#' automatic syntax highlighting using downlit https://github.com/r-lib/downlit/blob/main/R/downlit-md.R
#' this will help boost SEO i believe
parse_exercise <- function(x) {
  # pre-allocate list to fill
  res <- list(
    content = character(1),
    instructions = character(1),
    pre_exercise = NA_character_,
    script_pane = NA_character_,
    answer = NA_character_
  )

  # fetch code related elements and script pane elements
  preex_code <- rvest::html_elements(
    x,
    xpath = selectr::css_to_xpath(".pre-ex")
  )

  script_pane_code <- rvest::html_elements(
    x,
    xpath = selectr::css_to_xpath(".script-pane")
  )

  answer_code <- rvest::html_elements(
    x,
    xpath = selectr::css_to_xpath(".answer")
  )

  # store contents
  res[["pre_exercise"]] <- rvest::html_text2(preex_code)
  res[["script_pane"]] <- rvest::html_text2(script_pane_code)
  res[["answer"]] <- rvest::html_text2(answer_code)

  # remove code from the DOM
  xml2::xml_remove(preex_code, free = TRUE)
  xml2::xml_remove(script_pane_code, free = TRUE)
  xml2::xml_remove(answer_code, free = TRUE)

  # remove the removed DOM elements
  x <- x[nzchar(rvest::html_name(x))]

  # find where instructions start
  # we assume that the instructions are the last part of the
  # quarto doc. This should be documented.
  instruction_start <- which(grepl("instructions", rvest::html_attr(x, "class")))

  # get the exercise contents
  res[["content"]] <- as.character(x[1:(instruction_start - 1)])
  # get instructions
  res[["instructions"]] <- as.character(x[instruction_start:length(x)])

  res
}

parse_chapter <- function(fp) {
  # read the raw html
  html <- rvest::read_html(fp)
  # remove all script elements and the header element
  xml2::xml_remove(rvest::html_elements(html, "script"))
  xml2::xml_remove(rvest::html_elements(html, "header"))

  # extract all elements in the body
  all_elements <- rvest::html_elements(html, "body") |>
    rvest::html_children()

  # count how many total elements
  n <- length(all_elements)

  # now we need to iterate through each of these based on each h1 level heading
  # each level 1 heading is a lesson
  lesson_starts <- which(rvest::html_name(all_elements) == "h1")

  # identify the lesson types
  lesson_types <- rvest::html_attr(all_elements, "class")[lesson_starts]

  # count how many elements are in each lesson
  lesson_n_els <- diff(c(lesson_starts, length(all_elements)))

  # determine the last element position per exercise
  lesson_ends <- (lesson_starts + lesson_n_els) - 1

  # last element should be n
  lesson_ends[length(lesson_ends)] <- n

  # create a list of exercise html nodes
  lessons <- Map(
    function(start, end, type) {
      x <- all_elements[start:end]

      if (type == "exercise") {
        structure(parse_exercise(x), lesson = "execise")
      } else {
        structure(as.character(x), lesson = "reading")
      }
    },
    lesson_starts,
    lesson_ends,
    lesson_types
  )
  lessons
}

process_chapter <- function(fp) {
  # get the chapter file name
  chapt_name <- tools::file_path_sans_ext(fp)

  # output file path name
  out_dir <- file.path("output", chapt_name)

  if (!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE)
  }

  # process the file
  res <- parse_chapter(fp)

  # create output file names
  ind <- seq_along(res)
  fnames <- file.path(
    out_dir,
    paste("exercise-", ind, ".json", sep = "")
  )

  for (i in ind) {
    brio::write_file(
      jsonify::to_json(res[[i]], unbox = TRUE),
      fnames[i]
    )
  }

  # delete the html file
  file.remove(fp)
}


# Apply processing --------------------------------------------------------

files <- Sys.getenv("QUARTO_PROJECT_OUTPUT_FILES")

# extract file names
in_fps <- strsplit(files, "\n")[[1]]

for (fp in in_fps) {
  process_chapter(fp)
}


# Course metadata --------------------------------------------------------

# for testing
# in_fp <- sample(c("chapter-1.html", "chapter-2.html", "chapter-3.html", "chapter-4.html"))

# extract chapter names
chaps <- tools::file_path_sans_ext(in_fps)

# paste qmd to get qmd paths
qmds <- paste0(chaps, ".qmd")

# read the front matter from the quarto docs
chapters_meta <- lapply(qmds, rmarkdown::yaml_front_matter)

# extract order because we need to store the json in order
chapter_number <- vapply(
  strsplit(chaps, "-"),
  \(.x) as.integer(.x[2]),
  integer(1)
)

# reorder it
chapters <- chapters_meta[order(chapter_number)]

# read in projct file
quarto_file <- yaml::read_yaml("_quarto.yml")

# discard metadata related to rendering
quarto_cleaned <- purrr::discard_at(
  quarto_file,
  c("highlight-style", "eval", "embed-resources", "project", "theme")
)

# make json and write it
list(course = quarto_cleaned, chapters = chapters) |>
  jsonify::pretty_json(unbox = TRUE) |>
  brio::write_file("output/course.json")
