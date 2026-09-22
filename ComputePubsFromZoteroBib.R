# Install required package if needed
if (!requireNamespace("bibtex", quietly = TRUE)) {
  install.packages("bibtex")
}
library(bibtex)

# ---- Load your BibTeX file ----
bib <- read.bib("~/Downloads/zotero.bib")

# ---- SET YOUR NAME (IMPORTANT) ----
my_family <- "Knoche"
my_given  <- "Hendrik"

# =========================
# AUTHOR POSITION ANALYSIS
# (TOTAL / FIRST / LAST / NOT AUTHOR)
# =========================
my_name <- tolower(trimws(paste(my_family, my_given)))

# =========================
# AUTHOR EXTRACTION
# =========================
get_authors <- function(entry) {
  
  authors <- entry$author
  if (is.null(authors) || length(authors) == 0) return(NULL)
  
  sapply(authors, function(a) {
    family <- a$family
    given  <- paste(a$given, collapse = " ")
    trimws(paste(family, given))
  })
}

normalize <- function(x) {
  x <- tolower(x)
  x <- gsub("\\.", "", x)
  x <- gsub(",", "", x)
  trimws(x)
}

# small helper: pull a field out of a bib entry as a single lowercase string,
# regardless of whether it's NULL, length 0, or missing.
# NOTE: bibentry objects override `[[` for indexing entries by position, not
# by field name -- field access has to go through `$`, so we use do.call to
# do a variable-name "$" lookup.
field_txt <- function(entry, name) {
  val <- tryCatch(do.call("$", list(entry, name)), error = function(e) NULL)
  if (is.null(val) || length(val) == 0) return("")
  tolower(paste(val, collapse = " "))
}

# =========================
# CATEGORY CLASSIFICATION
# conference / journal / book (incl. anthologies) / openaccess / other
# =========================
get_category <- function(entry) {
  
  bibtype       <- if (!is.null(entry$bibtype)) tolower(entry$bibtype) else "unknown"
  journal_txt   <- field_txt(entry, "journal")
  archive_txt   <- field_txt(entry, "archiveprefix")
  eprinttype    <- field_txt(entry, "eprinttype")
  eprint_txt    <- field_txt(entry, "eprint")
  doi_txt       <- field_txt(entry, "doi")
  publisher_txt <- field_txt(entry, "publisher")
  note_txt      <- field_txt(entry, "note")
  
  combined <- paste(journal_txt, archive_txt, eprinttype, eprint_txt, doi_txt, note_txt)
  
  # ---- Open access preprints (arXiv / medRxiv) take priority over bibtype,
  #      since Zotero often exports these as "article" or "unpublished" ----
  if (grepl("arxiv", combined) ||
      grepl("medrxiv", combined) ||
      grepl("10\\.1101", doi_txt)) {
    return("openaccess")
  }
  
  # ---- Conferences ----
  if (bibtype %in% c("inproceedings", "conference", "proceedings")) {
    return("conference")
  }
  
  # ---- Journals ----
  if (bibtype %in% c("article", "journalarticle")) {
    return("journal")
  }
  
  # ---- Books / anthologies (edited volumes, book chapters, whole books) ----
  if (bibtype %in% c("book", "incollection", "inbook", "collection")) {
    return("book")
  }
  
  return("other")
}

# =========================
# STATS STORAGE
# =========================
categories <- c("conference", "journal", "book", "openaccess", "other")

stats <- list()
for (cat in categories) {
  stats[[paste0(cat, "_total")]] <- 0
  stats[[paste0(cat, "_first")]] <- 0
  stats[[paste0(cat, "_last")]]  <- 0
}
stats$not_author <- 0

not_author_titles <- c()

# =========================
# MAIN LOOP
# =========================
for (i in seq_along(bib)) {
  
  entry <- bib[[i]]
  authors <- get_authors(entry)
  
  title <- if (!is.null(entry$title)) entry$title else NA
  
  if (is.null(authors) || length(authors) == 0) next
  
  authors_n <- normalize(authors)
  cat_type  <- get_category(entry)
  
  is_first <- authors_n[1] == my_name
  is_last  <- tail(authors_n, 1) == my_name
  is_me    <- any(authors_n == my_name)
  
  # ---- NOT AUTHOR ----
  if (!is_me) {
    stats$not_author <- stats$not_author + 1
    not_author_titles <- c(not_author_titles, title)
    next
  }
  
  # ---- TALLY BY CATEGORY ----
  stats[[paste0(cat_type, "_total")]] <- stats[[paste0(cat_type, "_total")]] + 1
  if (is_first) stats[[paste0(cat_type, "_first")]] <- stats[[paste0(cat_type, "_first")]] + 1
  if (is_last)  stats[[paste0(cat_type, "_last")]]  <- stats[[paste0(cat_type, "_last")]] + 1
}

# =========================
# OUTPUT SUMMARY
# =========================

# display order and labels, as requested
display_order <- c("journal", "conference", "book", "openaccess", "other")

cat_label <- c(
  journal    = "Journals",
  conference = "Conferences",
  book       = "Books/Anthologies",
  openaccess = "Open access (arXiv, medRxiv)",
  other      = "Other"
)

grand_total <- 0
grand_first <- 0
grand_last  <- 0

cat("\n")
for (ct in display_order) {
  tot <- stats[[paste0(ct, "_total")]]
  fst <- stats[[paste0(ct, "_first")]]
  lst <- stats[[paste0(ct, "_last")]]
  
  grand_total <- grand_total + tot
  grand_first <- grand_first + fst
  grand_last  <- grand_last  + lst
  
  cat(sprintf("%s (%d, %d, %d)\n",
              cat_label[[ct]], tot, fst, lst))
}

cat(sprintf("Total (%d, %d, %d)\n",
            grand_total, grand_first, grand_last))

cat("\nNot an author on:", stats$not_author, "entries\n")
if (length(not_author_titles) > 0) {
  cat("Titles:\n")
  for (t in not_author_titles) cat(" -", t, "\n")
}