as_cast_fn <- function(f) {
  function(column, unit) f
}

pad_cutpoints <- function(cuts) {
  levs <- levels(cuts)

  txt <- as.character(levs)
  txtsplit <- strsplit(levs, ",")

  padded <- sapply(txtsplit, function(x) {
    if (length(x) == 1) {
      return(txt)
    }

    if (nchar(x[1]) == 2) {
      a <- substring(x[1], 1, 1)
      b <- substring(x[1], 2, 2)
      x[1] <- paste0(a, "0", b)
    }

    if (nchar(x[2]) == 2) {
      a <- substring(x[2], 1, 1)
      b <- substring(x[2], 2, 2)
      x[2] <- paste0("0", a, b)
    }

    paste0(x, collapse = ",")
  })

  factor(cuts, levels = levs, labels = padded)
}

file_type_from_name <- function(filename) {
  short <- grepl("short", filename)
  long <- grepl("long", filename)

  stopifnot(sum(c(short, long)) == 1)

  ifelse(short, "short", "long")
}

get_bare_indicator_name <- function(abbr_name) {
  gsub("_adj|_unadj", "", abbr_name)
}


# git 303
geometric_mean <- function(x, remove_na = TRUE) {
  exp(mean(log(x), na.rm = remove_na))
}

path_ext <- function(path) {
  if (length(path) == 0) {
    return(character())
  }

  base <- basename(path)
  reg_match <- regmatches(base, regexec("[.]([^.]+)\\s*$", base))[[1]]

  if (length(reg_match) == 0) {
    return(character())
  }

  reg_match[2]
}

get_age_cats <- function(dat) {
  age_groups_with_values <- vapply(
    names(total_age_group_names),
    function(x) {
      any(total_age_group_functions[[x]](dat[["age"]]))
    },
    logical(1)
  )

  vec_c(
    sort(
      set_names(names(total_age_group_names), total_age_group_names)[
        age_groups_with_values
      ]
    )
  )
}
