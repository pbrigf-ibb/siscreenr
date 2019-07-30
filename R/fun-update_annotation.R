#' update library annotation
#'
#' Check all geneIDs in the original library annotation supplied by Dharmacon
#' against GeneBank and get up-to-date information.
#'
#' Since information in data bases can change it is prudent
#' to refresh the library annotation from time to time.
#' This function takes every geneID in the original annotation supplied by Dharmacon
#' and checks its current status in GeneBank: whether is was withdrawn or replcaed
#' (and if so, by what new geneID), and whether the locus is a pseudogene or not.
#' Once the geneIDs are updated, a new query is sent to GeneBank to retrieve current
#' gene symbol, gene description, map location, chromosome number, and aliases
#' of all geneIDs.
#'
#' The outdated information is NOT dropped, it is stored in separate columns,
#' e.g. the original geneIDs end up in column \code{old_geneid}.
#'
#' @section Dependencies:
#' GeneBank queries are handled with the package \code{reutils}.
#' Data is loaded with package \code{data.table}.
#' Data processing is done with \code{tidyverse}.
#' Four functions are called internally here, see \code{Functions}.
#'
#' @section Processing time:
#' \code{check_geneid_status}
#' queries GeneBank one geneID at a time, which may create an appearance
#' of a DDOS attack if there are too many geneIDs to check.
#' Hence, a 1 second pause is introduced before every query.
#'
#' @section Subsets:
#' The library consists of three subsets, described in the annotation file as
#' 'Human Genome', 'Human Drug Targets' and 'Human Druggable Subset',
#' that consist of 38, 18, and 10 plates, respectively. Plates are numbered
#' independently within subsets and are renumbered 1 through 66 during the update.
#'
#' The update can be limited to a one of those subset or to a specific plate
#' to save time.
#'
#' @param infile file containing the original annotation;
#'               must be compatible with \code{[data.table]{fread}};
#'               deafults to internally sotred Dharmacon annotation from 16th May 2015
#' @param path (optional) path to directory where to save the updated annotation
#' @param verbose print function progeress as messages or not
#' @param plates optional numeric vector to limit the update to a set of plates, see \code{Subsets}
#' @param part optional library subset to limit the update to, see \code{Subsets}
#'
#' @return The function either invisibly returns the updated annotation
#'         or saves to a specified path and returns nothing.
#'
#' @export
#'

update_annotation <- function(infile, path, verbose = FALSE, plates,
    part = c('Human Genome', 'Human Drug Targets', 'Human Druggable Subset')) {

  if (!missing(infile) && !file.exists(infile)) stop('"infile" not found')
  # for subsetting
  if (!missing(part)) part <- match.arg('part', several.ok = TRUE)
  if (!missing(plates) && any(plates > 38)) stop('invalid plate selection')
  if (!missing(plates) && !is.numeric(plates)) stop('"plates" must be a numeric vector')
  if (missing(plates)) plates <- 1:38
  plates <- paste('Plate', plates)

  # load original annotation
  if (verbose) message('loading annotation')
  if (missing(infile)) infile <- 'extdata/ANNOTATION.LIBRARY.GENOMIC_20150416.original.txt'
  annotation_original <- data.table::fread(file = infile)

  # change column names to lower case
  nms <- tolower(names(annotation_original))

  ### provisions for making this function work for any annotation format
  # define a function
  replacer <- function(x, string, replacement, error, error2) {
    ind <- grep(string, x)
    if (length(ind) == 1) x[ind] <- replacement else
      if (length(ind) == 0) stop(error, call. = FALSE) else
        stop(error2, call. = FALSE)
  }
  # create vectors of regexs, replacements, and error messages
  strings <- c('wells?', 'gene[-,_,\\., ]symbols?', 'gene[-,_,\\., ]ids?', 'subsets?')
  replacements <- c('position', 'genesymbol', 'geneid', 'subset')
  errors <- paste('"data" contains no apparent specification of',
                  c('position', 'gene symbols', 'gene ids', 'library subsets'))
  errors2 <- paste('"data" contains unambiguous specification of',
                   c('position', 'gene symbols', 'gene ids', 'library subsets'))
  # run the function across the column names
  nms <-
    as.vector(mapply(FUN = replacer,
                     string = strings, replacement = replacements, error = errors, error2 = errors2,
                     MoreArgs = list(x = nms), SIMPLIFY = TRUE))
  # replace column names
  names(annotation_original) <- nms
  ### required column names are now present

  # filter subsets
  ind <- is.element(annotation_original$plate, plates) & is.element(annotation_original$subset, part)
  annotation_original <- annotation_original[ind, , drop = FALSE]
  if (nrow(annotation_original) == 0) stop('no plates in the subset')

  annotation_original$geneid <- as.character(annotation_original$geneid)
  # extract original geneIDs
  suppressWarnings({
    geneids <- as.numeric(unique(annotation_original$geneid))
    geneids <- geneids[!is.na(geneids)]
  })
  # check geneID status
  if (verbose) message('checking geneID status')
  if (verbose) double_check <- check_geneids(geneids) else
    suppressMessages(double_check <- check_geneids(geneids))
  # add geneID status to annotation and amend geneIDs
  annotation_checked <-
    #dplyr::full_join(annotation_original, double_check, by = c('GENEID' = 'geneid'))
    data.table::merge(annotation_original, double_check, by = 'geneid', all = TRUE)
  annotation_checked$old_geneid <- annotation_checked$geneid
  annotation_checked$geneid <- ifelse(is.na(annotation_checked$new_geneid),
                                      annotation_checked$old_geneid,
                                      annotation_checked$new_geneid)
  # extract new geneIDs
  suppressWarnings({
    geneids <- as.numeric(unique(annotation_checked$geneid))
    geneids <- geneids[!is.na(geneids)]
  })
  # get gene fields for all new geneIDs
  if (verbose) message('getting locus info...')
  fields <- get_gene_fields_batch(geneids)
  # append fields to amended annotation
  if (verbose) message('appending to annotation')
  #annotations_joined <- dplyr::full_join(annotation_checked, fields, by = 'geneid')
  annotations_merged <- data.table::merge(annotation_checked, fields, by = 'geneid')
  # update annotation
  if (verbose) message('updating annotation')
  annotation_updated <- #annotations_joined %>%
    annotations_merged %>%
    stats::setNames(tolower(names(.))) %>%
    dplyr::rename('old_gene_symbol' = 'genesymbol',
                  'duplex_catalog_number' = 'duplexcatalognumber',
                  'pool_catalog_number' = 'poolcatalognumber',
                  'gene_accession' = 'geneaccession') %>%
    dplyr::mutate(plate = rep(1, each = 280 * 4), position = toupper(.$position)) %>%
    dplyr::group_by('plate', 'position') %>%
    dplyr::mutate(sequences = paste(.$sequence, collapse = ', '),
                  duplex_catalog_numbers = paste(.$duplex_catalog_number, collapse = ', ')) %>%
    dplyr::ungroup() %>%
    dplyr::select(dplyr::one_of(
      c('plate', 'position', 'geneid', 'ginumber', 'gene_accession', 'gene_symbol',
        'aliases', 'description', 'map_location', 'chromosome',
        'sequences', 'duplex_catalog_numbers', 'pool_catalog_number',
        'withdrawn', 'replaced', 'old_geneid', 'old_gene_symbol'))) %>%
    dplyr::distinct() %>%
    dplyr::arrange(.$plate, .$position)
  annotation_updated$gene_symbol <-
    ifelse(annotation_updated$geneid == 'none', 'none', annotation_updated$gene_symbol)
  annotation_updated$gene_symbol <-
    ifelse(annotation_updated$geneid == '???', '???', annotation_updated$gene_symbol)
  annotation_updated$description <-
    ifelse(annotation_updated$geneid == 'none', 'none', annotation_updated$description)
  # save or return result
  if (missing(path)) {
    if (verbose) message('finished')
    invisible(annotation_updated)
  } else {
    if (verbose) message('saving file')
    filename <- paste0(path, '/', 'ANNOTATION.LIBRARY.GENOMIC_', gsub('-', '', Sys.Date()),'.txt')
    data.table::fwrite(annotation_updated, file = filename, sep = '\t')
    if (verbose) message('done!')
  }
}

#' @describeIn update_annotation
#' checks a single geneID and returns its pseudogene status (TRUE/FALSE),
#' withdrawn status (TRUE/FALSE), and a new geneID it his one has been replcaed;
#' returns a character vector
#'
# #' @param geneID a geneID number, given as a number, character string or factor
#'
#' @keywords internal
#'
check_geneid_status <- function(geneID) {
  if (length(geneID) != 1L) stop('"geneID" must be of length 1')
  efetch_object <- reutils::efetch(geneID, db = 'gene', 'gene_table')
  efetch_object_as_text <- reutils::content(efetch_object, as = 'text')
  result <-
    c(geneid = as.character(geneID),
      pseudogene = as.character(any(grepl('pseudogene', efetch_object_as_text))),
      withdrawn = as.character(any(grepl('withdrawn', efetch_object_as_text))),
      replaced =
        if (any(grepl('replaced', efetch_object_as_text))) {
          grep('replaced', unlist(strsplit(efetch_object_as_text, split = '\n')), value = TRUE)
        } else NA_character_)
  return(result)
}

#' @describeIn update_annotation
#' runs \code{check_geneid_status} for all geneIDs and returns a \code{data.frame};
#' pauses for 1 second before each request to avoid suspicion of DDOS attack
#'
# #' @param geneIDs vector of geneIDs, numeric, character or factor
#'
#' @keywords internal
#'
check_geneids <- function(geneIDs) {
  how_to_count <- function() {
    X <- length(geneIDs)
    iteration <- 1
    function() {
      message('\t geneID ', iteration, ' of ', X, '...')
      iteration <<- iteration + 1
    }
  }
  count <- how_to_count()
  check_geneid_status_with_pause <- function(x) {
    count()
    Sys.sleep(1)
    check_geneid_status(x)
  }
  a <- vapply(geneIDs, check_geneid_status_with_pause, character(4)) %>%
    t %>% data.frame(stringsAsFactors = FALSE) %>%
    dplyr::mutate_at(2:3, as.logical) %>%
    tidyr::separate('replaced', c('replaced', 'new_geneid'), sep = 'ID: ')
  a$replaced <- ifelse(is.na(a$replaced), FALSE, TRUE)
  a$new_geneid <- as.numeric(a$new_geneid)
  return(a)
}

#' @describeIn update_annotation
#' queries the gene data base and retrieves file fields:
#' gene symbol, gene description, map location, chromosome number, and aliases
#' (other geneIDs associated with the geneID)
#'
# #' @param geneIDs vector of geneIDs, numeric, character or factor
#'
#' @keywords internal
#'
get_gene_fields <- function(geneIDs) {
  e_object <- reutils::efetch(geneIDs, db = 'gene', rettype = 'docsum', retmode = 'text')
  e_object_as_text <- reutils::content(e_object, as = 'text')
  split_text <- strsplit(e_object_as_text, split='<[/]?Name>')[[1]]
  gene_symbol <- split_text[seq(from = 2, to = length(split_text), by = 2)]
  split_text <- strsplit(e_object_as_text, split='<[/]?Description>')[[1]]
  description <- split_text[seq(from = 2, to = length(split_text), by = 2)]
  split_text <- strsplit(e_object_as_text, split='<[/]?MapLocation>')[[1]]
  map_location <- split_text[seq(from = 2, to = length(split_text), by = 2)]
  split_text <- strsplit(e_object_as_text, split='<[/]?Chromosome>')[[1]]
  chromosome <- split_text[seq(from = 2, to = length(split_text), by = 2)]
  split_text <- strsplit(e_object_as_text, split='<[/]?OtherAliases>')[[1]]
  aliases <- split_text[seq(from = 2, to = length(split_text), by = 2)]
  return(
    data.frame(
      geneid = as.character(geneIDs), gene_symbol, description, map_location, chromosome, aliases,
      stringsAsFactors = F))
}

#' @describeIn update_annotation
#' run \code{get_gene_fields} in batches of 499 and less; this is necessary as
#' the results of \code{reutils::efetch} are unworkable for larger sets
#'
# #' @param geneIDs vector of geneIDs, numeric, character or factor
#'
#' @keywords internal
#'
get_gene_fields_batch <- function(geneIDs) {
  # we shall be calling efetch, which can only be done for less than 500 geneIDs at aa time
  # test how many items there are
  howmany <- length(geneIDs)
  # if there area less that 500 items, a single call suffices
  if (howmany < 500) {
    #if (verbose) message('... in one batch') # scoping not working...
    return(get_gene_fields(geneIDs))
  } else {
    # if there are 500 or more, we shall do it in 499-item steps
    # how many steps will there be?
    steps <- ceiling(howmany / 499)
    #if (verbose) message('... in ', steps, ' batches') # scoping not working...
    # create funcion factory that will select a 499-long intervals from a long vector
    stepper <- function(step) {
      # return function that returns the i-th 499-element section of x
      function(x) {
        X <- x[1:499 + 499 * (step -1)]
        X[!is.na(X)]
      }
    }
    # run the stepper function factory:
    # generate a list of functions that each returns a section of a vector
    steppers <- lapply(1:steps, stepper)
    # separate geneIDs into list of intervals
    ranges <- lapply(steppers, function(f) f(geneIDs))
    # do the deed over the list
    parts <- lapply(ranges, get_gene_fields)
    # wrap into single data frame
    return(do.call(rbind, parts))
  }
}

