library(methods)

########## animals #############################################################
setClass("animal",
  slots = c(
    name = "character",
    weight = "numeric",
    female = "logical"
  ),
  prototype = list(
    name = NA_character_,
    weight = NA_real_,
    female = NA
  )
)

setValidity("animal", function(object) {
  checkmate::assert_character(object@name, min.chars = 1, len = 1)
  # 0 does not really make sense though
  checkmate::assert_number(object@weight, lower = 0, finite = TRUE)
  checkmate::assert_flag(object@female)
  TRUE
})

# separate method to reuse in subclasses
show_animal <- function(object) {
  f <- "(m)"
  if (object@female) f <- "(f)"

  cat(is(object)[[1]], " '", object@name, "' ", f, "\n",
    "  weight: ", object@weight, "\n",
    sep = ""
  )
}

setMethod("show", "animal", show_animal)

# animal interface
animal <- function(name, weight, female) {
  new("animal", name = name, weight = weight, female = female)
}

########## preys ###############################################################
setClass("prey",
  contains = "animal",
  slots = c(
    hide = "numeric"
  ),
  prototype = list(hide = 0)
)

setValidity("prey", function(object) {
  checkmate::assert_number(object@hide, lower = 0, upper = 1)
  TRUE
})

setMethod("show", "prey", function(object) {
  cat(show_animal(object),
    "  hide: ", object@hide, "\n",
    sep = ""
  )
})

# prey interface
prey <- function(name, weight, female, hide) {
  new("prey", name = name, weight = weight, female = female, hide = hide)
}

setClass("mouse",
  contains = "prey"
)

setClass("rabbit",
  contains = "prey"
)

setClass("deer",
  contains = "prey"
)

mouse <- function(name = NULL, weight = NULL, female = NULL, hide = NULL) {
  animal_factory("mouse", name, weight, female, hide)
}

rabbit <- function(name = NULL, weight = NULL, female = NULL, hide = NULL) {
  animal_factory("rabbit", name, weight, female, hide)
}

deer <- function(name = NULL, weight = NULL, female = NULL, hide = NULL) {
  animal_factory("deer", name, weight, female, hide)
}

########## predators ###########################################################
setClass("predator",
  contains = "animal",
  slots = c(
    seek = "numeric"
  ),
  prototype = list(seek = 0)
)

setValidity("predator", function(object) {
  checkmate::assert_number(object@seek, lower = 0, upper = 1)
  TRUE
})

show_predator <- function(object) {
  cat(show_animal(object),
    "  seek: ", object@seek, "\n",
    sep = ""
  )
}

setMethod("show", "predator", show_predator)

# predator interface
predator <- function(name, weight, female, seek) {
  new("predator", name = name, weight = weight, female = female, seek = seek)
}

setClass("hawk",
  contains = "predator"
)

setClass("lynx",
  contains = "predator"
)

hawk <- function(name = NULL, weight = NULL, female = NULL, seek = NULL) {
  animal_factory("hawk", name, weight, female, seek)
}

lynx <- function(name = NULL, weight = NULL, female = NULL, seek = NULL) {
  animal_factory("lynx", name, weight, female, seek)
}


# I know this was not the idea...
# but I cannot cope with redundant code ¯\_(o_O)_/¯
#' Animal factory.
#' Encodes restrictions on specific species attributes, eg weight, hide, seek.
#' Takes care of missing arguments and validation.
#'
#' @param species
#' @param name
#' @param weight
#' @param female
#' @param hide_or_seek
#'
#' @return species
#' @export
#'
#' @examples
animal_factory <- function(species, name, weight, female, hide_or_seek) {
  
  # lookup dataframe
  attribute_lookup <- data.frame(
    id = c(1:5),
    species = c("mouse", "rabbit", "deer", "hawk", "lynx"),
    species_type = c("prey", "prey", "prey", "predator", "predator"),
    min_weight = c(0.5, 1, 15, 3, 20),
    max_weight = c(1, 5, 30, 8, 60),
    min_hs = c(0.5, 0.3, 0.2, 0.6, 0.5),
    max_hs = c(1, 0.8, 0.7, 1.0, 0.9),
    stringsAsFactors = FALSE
  )
  
  # specific row
  attribute <- attribute_lookup[attribute_lookup["species"] == species, ]
  
  min_w <- attribute[["min_weight"]]
  max_w <- attribute[["max_weight"]]
  min_hs <- attribute[["min_hs"]]
  max_hs <- attribute[["max_hs"]]
  species_type <- attribute[["species_type"]]
  
  # instantiate random values for each null
  if (is.null(name)) name <- make_name()
  if (is.null(female)) female <- sample(c(TRUE, FALSE), 1)
  if (is.null(weight)) weight <- runif(1, min_w, max_w)
  if (is.null(hide_or_seek)) hide_or_seek <- runif(1, min_hs, max_hs)
  
  # make sure input is valid
  checkmate::assert_number(weight, lower = min_w, upper = max_w)
  checkmate::assert_number(hide_or_seek, lower = min_hs, upper = max_hs)
  
  # currently either prey or predator => upgrade to switch if more complex
  if (species_type == "prey") {
    new(species, name = name, weight = weight, female = female, hide = hide_or_seek)
  } else {
    new(species, name = name, weight = weight, female = female, seek = hide_or_seek)
  }
}

########## meet generic ########################################################
# define meet
setGeneric("meet", function(animal1, animal2) standardGeneric("meet"))

# implement meet for both preys
setMethod("meet", signature("prey", "prey"), function(animal1, animal2) {
  same_species <- class(animal1) == class(animal2)
  same_sex <- animal1@female == animal2@female

  if (same_species && !same_sex) {
    outcome <- sample(c("ignore", "sniff", "love"), 1, prob = c(0.25, 0.25, 0.5))
  } else {
    outcome <- sample(c("ignore", "sniff"), 1)
  }

  meet_str(animal1, animal2, outcome = outcome)
})

# implement meet for both predators
setMethod("meet", signature("predator", "predator"), function(animal1, animal2) {
  same_species <- class(animal1) == class(animal2)
  same_sex <- animal1@female == animal2@female

  if (same_species && !same_sex) {
    outcome <- sample(c("fight", "love"), 1)
  } else {
    outcome <- sample(c("ignore", "sniff", "fight"), 1)
  }

  meet_str(animal1, animal2, outcome = outcome)
})

# implement meet for predator vs prey
setMethod("meet", signature("predator", "prey"), function(animal1, animal2) {
  predator <- animal1
  prey <- animal2

  smaller_prey <- prey@weight >= 0.05 * predator@weight && prey@weight <= 0.7 * predator@weight

  if (smaller_prey) {
    kill_prob <- min(1, max(0, 0.6 + predator@seek - prey@hide))
    outcome <- sample(c("kill", "escape"), 1, prob = c(kill_prob, 1 - kill_prob))
  } else {
    outcome <- sample(c("ignore", "sniff"), 1)
  }

  meet_str(predator, prey, outcome = outcome)
})

# simply call predator vs prey in reverse order
setMethod("meet", signature("prey", "predator"), function(animal1, animal2) {
  callGeneric(animal2, animal1)
})


#' String outcome of animal encounters.
#'
#' @param animal1 predator if animals are foes, otherwise same as animal2
#' @param animal2 prey if animals are foes, otherwise same as animal1
#' @param outcome some encoding of an outcome, eg string
#'
#' @return
#' @export
#'
#' @examples
meet_str <- function(animal1, animal2, outcome) {
  animal1_str <- paste(class(animal1), " '", animal1@name, "'", sep = "")
  animal2_str <- paste(class(animal2), " '", animal2@name, "'", sep = "")

  switch(outcome,
    "ignore" = return(paste(animal1_str, "&", animal2_str, "ignore each other", "\n", sep = " ")),
    "love" = return(paste(animal1_str, "&", animal2_str, "make sweet, sweet love", "\n", sep = " ")),
    "sniff" = return(paste(animal1_str, "&", animal2_str, "sniff each others' butts", "\n", sep = " ")),
    "fight" = return(paste(animal1_str, "&", animal2_str, "fight for territory", "\n", sep = " ")),
    "kill" = return(paste(animal1_str, "kills and eats", animal2_str, "\n", sep = " ")),
    "escape" = return(paste(animal2_str, "escapes from", animal1_str, "\n", sep = " "))
  )

  return(paste(animal1_str, "gazes at her reflection in a puddle", "\n", sep = " "))
}

# random pronouncable strings with length <length>
make_name <- function(length = 7) {
  vowels <- c("a", "e", "i", "o", "u")
  consonants <- setdiff(letters, vowels)
  name <- character(length)
  name[1] <- sample(toupper(consonants), 1)
  name[seq(3, length, by = 2)] <-
    sample(consonants, size = ceiling(length / 2) - 1, replace = TRUE)
  name[seq(2, length, by = 2)] <-
    sample(vowels, size = floor(length / 2), replace = TRUE)
  paste(name, collapse = "")
}

# example code for animal class: (results may vary.)
set.seed(20191121)
animals <- list(
  mouse(female = TRUE),
  rabbit(),
  hawk(female = TRUE),
  deer(),
  lynx(female = TRUE),
  lynx(female = FALSE),
  deer(),
  mouse(female = FALSE),
  deer(female = TRUE)
)

for (animal1 in animals[1:5]) {
  for (animal2 in animals[9:5]) {
    cat(meet(animal1, animal2))
  }
}