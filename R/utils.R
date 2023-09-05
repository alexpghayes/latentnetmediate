utils::globalVariables(
  c(
    "aligned_true_coef",
    "conf.low",
    "conf.high",
    "color",
    "estimate",
    "estimand",
    "id",
    "i",
    "j",
    "n",
    "trt",
    "x",
    "name",
    "nde",
    "nie",
    "effect",
    "term",
    "value"
  )
)

left_padded_sequence <- function(x) {

  original <- withr::with_options(
    c(scipen = 999),
    as.character(x)
  )

  max_digits <- max(vapply(original, nchar, integer(1)))
  formatC(x, width = max_digits, format = "d", flag = "0")
}
