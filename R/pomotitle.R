#' Generate a Postmodernist Title
#'
#' Generates a character vector of postmodernist titles.
#'
#' Why? Because we must situate erasure as a form of ontological pluralistic
#' framework.
#'
#' @param n number of titles to generate.
#'
#' @return a character vector of titles.
#' @export
#'
#' @examples
#' generate_pomotitle()
#'
#' generate_pomotitle(10)
generate_pomotitle <- function(n = 1) {
  if (!is.numeric(n)) {
    stop("I can't handle anything but numbers as input arguments.")
  } else if (length(n) > 1) {
    stop("I don't know how to handle anything but a single number.")
  } else  if (n == 0) {
    return(character())
  } else if (n < 0) {
    stop("Even though I tried, I cannot generate a negative number of titles.")
  }
  parts <- get_title_parts()
  sapply(seq.int(n), function(x) {
    build_title(parts[[1]], parts[[2]], parts[[3]])
    })
}

get_title_parts <- function() {
  a <- c("Situating",
         "Reinterpreting",
         "Critiquing",
         "A Reading of",
         "Activating",
         "The Politics of",
         "Representations of",
         "Interrogating",
         "Erasing",
         "Redefining",
         "Identifying",
         "Reimagining",
         "Performing",
         "The Legability of",
         "Democratizing",
         "De-Centering",
         "Gender and",
         "Debating",
         "Signaling",
         "Embodying",
         "Building",
         "The Role of",
         "Historicizing",
         "Repositioning",
         "Destabilization",
         "Mapping")

  b <- c("Spaces",
         "Bodies",
         "Identity",
         "Narratives",
         "Politics and Power",
         "Aesthetics",
         "Representation",
         "Historical Categories",
         "Pluralities",
         "Gender",
         "The Gaze",
         "Forms of Oppression",
         "Silences",
         "Power Structures",
         "Dissent",
         "Normativity",
         "Progress",
         "Erasure",
         "The Self",
         "Queerness",
         "Modes of Being",
         "Ontology",
         "Agency",
         "Epistemologies",
         "Intertextuality",
         "Fields of Belonging")

  c <- c("As a Site of Resistance",
         "As Performance",
         "As Coded Queerness",
         "As Cultural Mediators",
         "As Tranformative Justice",
         "As Violence",
         "In an Intersectional Framework",
         "In New Media",
         "As a Form of Erasure",
         "As a Site of Political Contestation",
         "In Crisis",
         "Through a Critical Lens")
  list(a, b, c)
}

build_title <- function (a, b, c) {
  paste(sample(a, 1), sample(b, 1), sample(c, 1))
}
