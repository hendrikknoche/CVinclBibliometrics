# ==============================================================================
# publication_tally.R
#
# Tally one author's publications from a BibTeX export by publication category,
# with counts of: total, first-author, last-author and middle-author records.
#
#   Input : data/zotero.bib
#   Output: table on the console, plus two CSV files in output/
#             - publication_tally.csv        (the summary table)
#             - publications_classified.csv  (one row per record, for auditing)
#   Needs : base R only (no packages)
#
# Authorship rules
#   first  : the author is listed first. A sole-author record is counted here
#            only, i.e. it is never also counted as "last".
#   last   : the author is listed last AND the record has 2+ authors.
#   middle : everything else.
#   Edited volumes have no author field, so the editor list is used instead.
#
# Category rules: see classify() below. Export types are unreliable (e.g. Pure/
# Zotero often use @inbook for conference abstracts), so a short, explicit
# override table (by bib key) handles the cases the metadata cannot tell apart.
# ==============================================================================

# ---- 1. Settings -------------------------------------------------------------
BIB_FILE        <- file.path("data", "zotero.bib")
AUTHOR_PATTERN  <- "Knoche"   # regex; matched against each author/editor name,
# so it also catches "Knoche, {Hendrik Ole}" and
# "H. Knoche"
OUT_DIR         <- "output"
DROP_DUPLICATES <- FALSE      # TRUE = exclude the records listed in `duplicates`

CATEGORIES <- c(
  "Journal articles",
  "Conference papers (proceedings)",
  "Conference abstracts, talks & workshop contributions",
  "Book chapters",
  "Edited books / proceedings",
  "Editorials, prefaces & front matter",
  "Preprints / working papers",
  "Other (magazine / misc)"
)

# Manual classification for records the metadata cannot resolve (bib key -> category).
overrides <- data.frame(
  key = c(
    "b15299c4a7f64545bfb157fec6a46147",  # 2022 "Of apples and oranges" - ESO conference abstract in a journal supplement
    "cbdec3a42c9149b48a9d7e43a4a4440f",  # 2025 "Whack away" - WCVR 2024 abstract
    "4912db5b15494306ae277eccc29b7919",  # 2022 "Virtual Mirror Therapy" - ESO abstract in a journal supplement
    "4b3ce0a99a564a7491397d7f26426815",  # 2022 "Virtual Motor Spaces" - ICVR 2022 abstract
    "262434ae10854ddf9742f0f04bd333ea",  # 2022 "VR Baking Tray Test" - WCNR 2022 abstract
    "579ca89fa8a643e2b6c4b062b53b4d9b",  # 2018 "VR@SN" - Neuroscience Day abstract (also exported as @conference)
    "11e5d8a015034054bf6f5b9918e4a09d",  # 2019 special-issue preface (IxD&A)
    "dde2bb844a5e45c4b21a24077862df60",  # 2018 special-issue preface (IxD&A)
    "6239bb3db04e46b4b219b413956dbec6",  # 2014 "Thinking beyond the box" - special-issue editorial (BIT)
    "6435714df6234ba184f5656d914af40f",  # 2013 special-issue editorial (Multimedia Systems)
    "1ed81849087e4d759b1478a7cead3f1b",  # 2012 "Chairs' welcome" - proceedings front matter
    "fbe263fc34f9402b90ceecbfacc9958f",  # 2014 "Peer pressure" - magazine piece (D+C)
    "0e542d3c6b614955932534dfc1b8f0b1",  # 2004 WWRF12 - conference paper exported as @article
    "6dbccd9127824ad4bab8ef5bcd710b67",  # 2024 EDULEARN - conference paper exported as @inbook
    "8a17162869274548a1ea10a803632cb8"   # 2024 "Aiming, Pointing, Steering" - PACM HCI (CHI PLAY) = journal
  ),
  category = c(
    rep("Conference abstracts, talks & workshop contributions", 6),
    rep("Editorials, prefaces & front matter", 5),
    "Other (magazine / misc)",
    rep("Conference papers (proceedings)", 2),
    "Journal articles"
  ),
  stringsAsFactors = FALSE
)

# Records that repeat another record (preprint of a published paper, two
# versions of one arXiv ID, or the same abstract exported twice).
# Only the *second* copy is listed; the first is kept.
duplicates <- c(
  "c920be96089a4d028ee3fd5846cecc9e",  # esports screening paper: 2nd arXiv entry
  "a8e6d0551e78424c979a364d52127143",  # "Game Changers": arXiv entry duplicating the other arXiv entry
  "14aa7fa6d8c74db7a51572d403a58802",  # medRxiv preprint of the European Stroke Journal paper
  "579ca89fa8a643e2b6c4b062b53b4d9b"   # VR@SN abstract exported twice
)

# ---- 2. Minimal BibTeX parser (handles nested braces, \{ \} escapes, "{"}") ----
NAME_CHARS <- c(letters, LETTERS, 0:9, "_", "-", ":", ".")
WS_CHARS   <- c(" ", "\t", "\n", "\r")

# Read one field value starting at position i of the character vector `ch`.
# Handles "..." , {...} and bare tokens (numbers, month macros such as `mar`).
read_value <- function(ch, i) {
  n <- length(ch)
  open <- ch[i]
  if (open == "{" || open == "\"") {
    depth <- if (open == "{") 1L else 0L
    start <- i + 1L
    i <- start
    while (i <= n) {
      cur <- ch[i]
      if (cur == "\\") { i <- i + 2L; next }             # skip escaped char (\{ \} \" ...)
      if (cur == "{") {
        depth <- depth + 1L
      } else if (cur == "}") {
        if (open == "{" && depth == 1L) break
        depth <- depth - 1L
      } else if (cur == "\"" && open == "\"" && depth == 0L) {
        break                                             # a quote only closes at brace depth 0
      }
      i <- i + 1L
    }
    end <- min(i - 1L, n)
    val <- if (end >= start) paste(ch[start:end], collapse = "") else ""
    return(list(value = val, next_i = i + 1L))
  }
  start <- i
  while (i <= n && !(ch[i] %in% c(",", "}", WS_CHARS))) i <- i + 1L
  list(value = if (i > start) paste(ch[start:(i - 1L)], collapse = "") else "",
       next_i = i)
}

parse_entry <- function(chunk) {
  hdr <- regmatches(chunk, regexec("^\\s*@([A-Za-z]+)\\s*\\{\\s*([^,]+),", chunk, perl = TRUE))[[1]]
  ch  <- strsplit(substring(chunk, nchar(hdr[1]) + 1L), "")[[1]]
  n   <- length(ch); i <- 1L
  fields <- list()
  while (i <= n) {
    if (ch[i] %in% c(WS_CHARS, ",")) { i <- i + 1L; next }
    if (ch[i] == "}") break                               # end of entry
    s <- i
    while (i <= n && ch[i] %in% NAME_CHARS) i <- i + 1L
    if (i == s) { i <- i + 1L; next }                     # unexpected character: skip
    name <- tolower(paste(ch[s:(i - 1L)], collapse = ""))
    while (i <= n && ch[i] %in% WS_CHARS) i <- i + 1L
    if (i > n || ch[i] != "=") next
    i <- i + 1L
    while (i <= n && ch[i] %in% WS_CHARS) i <- i + 1L
    if (i > n) break
    v <- read_value(ch, i)
    if (is.null(fields[[name]])) fields[[name]] <- v$value  # keep first occurrence
    i <- v$next_i
  }
  list(key = trimws(hdr[3]), bibtype = tolower(hdr[2]), fields = fields)
}

read_bib <- function(path) {
  if (!file.exists(path)) stop("Bib file not found: ", normalizePath(path, mustWork = FALSE))
  raw    <- paste(readLines(path, encoding = "UTF-8", warn = FALSE), collapse = "\n")
  chunks <- strsplit(raw, "\n(?=@[A-Za-z]+\\s*\\{)", perl = TRUE)[[1]]
  chunks <- chunks[grepl("^\\s*@[A-Za-z]+\\s*\\{", chunks)]
  recs   <- lapply(chunks, parse_entry)
  recs   <- recs[!vapply(recs, function(r) r$bibtype %in% c("comment", "string", "preamble"), logical(1))]
  fld <- function(nm) vapply(recs, function(r) {
    v <- r$fields[[nm]]
    if (is.null(v)) "" else v
  }, character(1))
  data.frame(
    key         = vapply(recs, `[[`, character(1), "key"),
    bibtype     = vapply(recs, `[[`, character(1), "bibtype"),
    year        = fld("year"),
    title       = fld("title"),
    author      = fld("author"),
    editor      = fld("editor"),
    journal     = fld("journal"),
    booktitle   = fld("booktitle"),
    publisher   = fld("publisher"),
    institution = fld("institution"),
    type_field  = fld("type"),      # e.g. "WorkingPaper" for preprints
    stringsAsFactors = FALSE
  )
}

strip_tex <- function(x) {
  x <- gsub("\\\\text[A-Za-z]+", "", x)   # \textquoteright etc.
  gsub("[{}\\\\]", "", x)
}

# ---- 3. Authorship: first / last / middle ------------------------------------
add_authorship <- function(pubs) {
  people  <- ifelse(nzchar(pubs$author), pubs$author, pubs$editor)   # editors for edited volumes
  lst     <- lapply(people, function(x) {
    if (!nzchar(x)) character(0) else trimws(strsplit(x, "\\s+and\\s+", perl = TRUE)[[1]])
  })
  pubs$n_authors <- vapply(lst, length, integer(1))
  pubs$position  <- vapply(lst, function(a) {
    hit <- grep(AUTHOR_PATTERN, a, ignore.case = TRUE)
    if (length(hit)) hit[1] else NA_integer_
  }, integer(1))
  pubs$role <- ifelse(is.na(pubs$position), "not found",
                      ifelse(pubs$position == 1L, "first",                        # incl. sole author
                             ifelse(pubs$position == pubs$n_authors, "last", "middle")))  # last needs n >= 2 (n == 1 caught above)
  pubs
}

# ---- 4. Category rules (first match wins) ------------------------------------
classify <- function(pubs) {
  venue <- paste(pubs$journal, pubs$booktitle, pubs$publisher, pubs$institution)
  cat   <- rep(NA_character_, nrow(pubs))
  set   <- function(cond, label) { idx <- is.na(cat) & cond; cat[idx] <<- label }
  
  # 0. explicit overrides
  cat <- overrides$category[match(pubs$key, overrides$key)]
  
  # 1. preprints / working papers (arXiv, medRxiv)
  set(pubs$type_field == "WorkingPaper" | grepl("arxiv|medrxiv", venue, ignore.case = TRUE),
      "Preprints / working papers")
  # 2. edited books / proceedings volumes
  set(pubs$bibtype == "book", "Edited books / proceedings")
  # 3. @conference = conference contribution (talk, abstract, workshop paper)
  set(pubs$bibtype == "conference", "Conference abstracts, talks & workshop contributions")
  # 4. remaining @misc
  set(pubs$bibtype == "misc", "Other (magazine / misc)")
  # 5. PACM HCI is a journal even when exported as @inproceedings
  set(grepl("Proceedings of the ACM on Human-Computer", venue, fixed = TRUE), "Journal articles")
  # 6. journal articles
  set(pubs$bibtype == "article", "Journal articles")
  # 7. @inbook: proceedings papers vs. book chapters
  set(pubs$bibtype == "inbook" & grepl("proceedings", pubs$booktitle, ignore.case = TRUE),
      "Conference papers (proceedings)")
  set(pubs$bibtype == "inbook", "Book chapters")
  # 8. @inproceedings
  set(pubs$bibtype == "inproceedings", "Conference papers (proceedings)")
  # 9. anything unexpected
  set(TRUE, "Other (magazine / misc)")
  cat
}

# ---- 5. Run ------------------------------------------------------------------
pubs <- read_bib(BIB_FILE)
pubs <- add_authorship(pubs)
pubs$category  <- classify(pubs)
pubs$duplicate <- pubs$key %in% duplicates
pubs$title     <- strip_tex(pubs$title)

if (any(pubs$role == "not found"))
  warning(sum(pubs$role == "not found"), " record(s) without a match for AUTHOR_PATTERN: ",
          paste(pubs$key[pubs$role == "not found"], collapse = ", "))

n_all <- nrow(pubs)
if (DROP_DUPLICATES) pubs <- pubs[!pubs$duplicate, ]

tally <- do.call(rbind, lapply(CATEGORIES, function(k) {
  d <- pubs[pubs$category == k, ]
  data.frame(Category = k, Total = nrow(d),
             First  = sum(d$role == "first"),
             Last   = sum(d$role == "last"),
             Middle = sum(d$role == "middle"),
             stringsAsFactors = FALSE)
}))
tally <- rbind(tally, data.frame(Category = "TOTAL",
                                 Total = sum(tally$Total), First = sum(tally$First),
                                 Last = sum(tally$Last),   Middle = sum(tally$Middle),
                                 stringsAsFactors = FALSE))
stopifnot(tally$Total[nrow(tally)] == nrow(pubs))   # every record landed in exactly one category

cat("\nSource:", BIB_FILE, "-", n_all, "records read\n")
if (!DROP_DUPLICATES)
  cat("Note:", sum(pubs$duplicate), "records are flagged as duplicates; set DROP_DUPLICATES <- TRUE to exclude them.\n\n")
print(tally, row.names = FALSE)

# ---- 6. Save -----------------------------------------------------------------
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)
write.csv(tally, file.path(OUT_DIR, "publication_tally.csv"), row.names = FALSE)
write.csv(pubs[order(pubs$category, pubs$year),
               c("key", "year", "category", "bibtype", "n_authors", "position", "role", "duplicate", "title")],
          file.path(OUT_DIR, "publications_classified.csv"), row.names = FALSE)
cat("\nWrote", file.path(OUT_DIR, c("publication_tally.csv", "publications_classified.csv")), "\n")