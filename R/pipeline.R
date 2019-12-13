
#
# Zur Zeit kann man settings nur ueber ein file setzen wenn
# man docker startet.
#


#
# coref.algorithm = c("clustering", "statistical", "neural", "hybrid")
#
nlp_client_control <- function(coref.algorithm = "neural") {
    "TODO"
}

## 
#' @title Stanford \code{CoreNLP} annotator pipeline
#' @description Create a Stanford \code{CoreNLP} annotator pipeline.
#' @param annotators
#'   a character string specifying the annotators to be used in addition
#'   to \sQuote{ssplit} (sentence token annotation) and \sQuote{tokenize}
#'   (word token annotations), with elements
#'   \code{"pos"} (POS tagging),
#'   \code{"lemma"} (lemmatizing),
#'   \code{"ner"} (named entity recognition),
#'   \code{"regexner"} (rule-based named entity recognition over token
#'   sequences using Java regular expressions),
#'   \code{"parse"} (constituency parsing),
#'   \code{"depparse"} (dependency parsing),
#'   \code{"sentiment"} (sentiment analysis),
#'   \code{"coref"} (coference resolution),
#'   \code{"dcoref"} (deterministic coference resolution),
#'   \code{"cleanxml"} (clean XML tags),
#'   or
#'   \code{"relation"} (relation extraction),
#'   or unique abbreviations thereof.
#'   Ignored for languages other than English.
#' @param language a character string giving the ISO-639 code of the
#'   language being processed by the annotator pipeline.
#' @param control a named or empty (default) list vector with annotator control
#'   options, with the names giving the option names.  See
#'   \url{https://stanfordnlp.github.io/CoreNLP/annotators.html} for
#'   available control options.
#' @param port an integer giving the port (default is \code{9000L}).
#' @param host a character string giving the hostname of the server.
#' @note
#'   See \url{https://stanfordnlp.github.io/CoreNLP/#citing-stanford-corenlp-in-papers}
#'   for information on citing Stanford CoreNLP.
#' 
#'   Using the parse annotator requires considerable amounts of (Java)
#'   memory.  The Stanford CoreNLP documentation suggests starting the JVM
#'   with at least 3GB of memory on 64-bit systems (and in fact, not using
#'   32-bit systems), and hence have the JVM started with \option{-Xmx3g}
#'   unless option \code{java.parameters} is set to something non-empty
#'   (hence, this option should be set appropriately to accommodate
#'   different memory requirements or constraints).
#' 
#'   Using the coreference annotators nowadays requires even more (Java)
#'   memory.  The Stanford CoreNLP documentation suggests starting the JVM
#'   with at least 5GB of memory; we find 4GB sufficient.  Hence, to use
#'   these annotators one needs to set option \code{java.parameters} as
#'   appropriate before starting the JVM.
#' @seealso
#'   \url{https://stanfordnlp.github.io/CoreNLP/} for more 
#'   information about the Stanford CoreNLP tools.
#' @return 
#' An \code{\link[NLP]{Annotator}} object providing the annotator pipeline.
#' @examples
#' require("NLP")
#' s <- as.String(paste("Stanford University is located in California.",
#'                      "It is a great university."))
#' s
#' 
#' ## Annotators: ssplit, tokenize:
#' if ( ping_nlp_client() == "pong" ) {
#' p <- StanfordCoreNLP_Pipeline(NULL)
#' a <- p(s)
#' a
#' 
#' ## Annotators: ssplit, tokenize, pos, lemma (default):
#' p <- StanfordCoreNLP_Pipeline()
#' a <- p(s)
#' a
#' 
#' ## Equivalently:
#' annotate(s, p)
#' 
#' ## Annotators: ssplit, tokenize, parse:
#' p <- StanfordCoreNLP_Pipeline("parse")
#' a <- p(s)
#' a
#' 
#' ## Respective formatted parse trees using Penn Treebank notation
#' ## (see <https://catalog.ldc.upenn.edu/docs/LDC95T7/cl93.html>):
#' ptexts <- sapply(subset(a, type == "sentence")$features, `[[`, "parse")
#' ptexts
#' 
#' ## Read into NLP Tree objects.
#' ptrees <- lapply(ptexts, Tree_parse)
#' ptrees
#' 
#' ## Basic dependencies:
#' depends <- lapply(subset(a, type == "sentence")$features, `[[`,
#'                   "basic-dependencies")
#' depends
#' ## Note that the non-zero ids (gid for governor and did for dependent)
#' ## refer to word token positions within the respective sentences, and
#' ## not the ids of these token in the annotation: these can easily be
#' ## matched using the sentence constituents features:
#' lapply(subset(a, type == "sentence")$features, `[[`, "constituents")
#' 
#' ## (Similarly for sentence ids used in dcoref document features.)
#' 
#' ## Note also that the dependencies are returned as a data frame 
#' ## inheriting from class "Stanford_typed_dependencies" which has print
#' ## and format methods for obtaining the usual formatting.
#' depends[[1L]]
#' ## Use as.data.frame() to obtain strip this class:
#' as.data.frame(depends[[1L]])
#' }
#' @export
StanfordCoreNLP_Pipeline <- function(annotators = c("pos", "lemma"), language = "en", 
    control = list(), port = 9000L, host = "localhost") {

    if(language == "en") {
         table <- c("tokenize", "cleanxml", "ssplit",
                   "pos", "lemma", "gender", "ner", "regexner",
                   "truecase",
                   "parse", "depparse",
                   "sentiment",
                   "coref", "dcoref",
                   "relation")

        depends <-
            list(cleanxml =
                     c("tokenize"),
                 pos =
                     c("tokenize", "ssplit"),
                 lemma =
                     c("tokenize", "ssplit", "pos"),
                 ner =
                     c("tokenize", "ssplit", "pos", "lemma"),
                 regexner =
                     c("tokenize", "ssplit", "pos"),
                 sentiment =
                     c("pos", "parse"),
                 parse =
                     c("tokenize", "ssplit", "pos"),
                 depparse =
                     c("tokenize", "ssplit", "pos"),
                 dcoref =
                     c("tokenize", "ssplit", "pos", "lemma", "ner", "parse"),
                 relation =
                     c("tokenize", "ssplit", "pos", "lemma", "ner", "parse"),
                 natlog =
                     c("tokenize", "ssplit", "pos", "lemma", "parse"),
                 quote =
                     character(),
                 ## Not on the HTML page ...
                 coref =
                     c("tokenize", "ssplit", "pos", "lemma", "ner", "parse"),
                 gender = c("tokenize", "ssplit", "pos"),
                 truecase = c("tokenize", "ssplit", "pos", "lemma")
                 )
        pos <- pmatch(tolower(annotators), table)
        if(any(ind <- is.na(pos)))
            stop(sprintf("No unique match for annotator(s) %s",
                         paste(sQuote(annotators[ind]), collapse = " ")))
        
        annotators <-
            intersect(table,
                      c("tokenize", "ssplit",
                        table[pos], unlist(depends[table[pos]])))

        if ( isTRUE(control$coref.algorithm %in% c("neural", "statistical")) )
          annotators <- union(setdiff(annotators, "dcoref"), c("mention", "coref"))
 
    } 

    nlpCore <- new_nlp_client(annotators, port = port, host = host, control = control)
    
    f <- function(s, a = NULL) {       
        s <- as.String(s)

        out <- nlpCore$annotate(s)
        doc <- read_xml(out)

        tags <- c("POS", "lemma", "NER", "NormalizedNER")
        
        fun <- function(si, pi, offset) {
            start <-
                as.integer(vapply(si, `[[`, "", "CharacterOffsetBegin")) + 1L
            end <-
                as.integer(vapply(si, `[[`, "", "CharacterOffsetEnd"))
            len <- length(end)
            id <- seq.int(offset, offset + len)
            Annotation(id,
                       c("sentence", rep.int("word", len)),
                       c(min(start), start),
                       c(max(end), end),
                       c(list(c(list(constituents = tail(id, -1L)),
                                pi)),
                         lapply(si,
                                function(e) e[intersect(tags, names(e))])))
        }

        y <- xpath_apply(doc, "//sentences/sentence", info_for_sentence)
        z <- if(!is.na(match("parse", annotators))) {
            Map(c,
                xpath_apply(doc, "//sentences/sentence/parse",
                           function(e) list(parse = xml_text(e))),
                xpath_apply(doc, "//sentences/sentence",
                           sentence_dependencies))
        } else if(!is.na(match("depparse", annotators))) {
            xpath_apply(doc, "//sentences/sentence",
                        sentence_dependencies)
        } else {
            vector("list", length(y))
        }
        if(!is.na(match("sentiment", annotators)))
            z <- Map(c, z,
                     xpath_apply(doc, "//sentences/sentence",
                                function(e)
                                as.list(xml_attrs(e)[c("sentimentValue",
                                                       "sentiment")])))
        offsets <- cumsum(1L + c(0, head(lengths(y), -1L)))
        y <- do.call(c, Map(fun, y, z, offsets))

        if((language == "en") && !is.na(match("pos", annotators))) {
            meta(y, "POS_tagset") <- "en-ptb"
            meta(y, "POS_tagset_URL") <-
                "https://catalog.ldc.upenn.edu/docs/LDC95T7/cl93.htm"
        }

        if( any(c("dcoref", "coref") %in% annotators) ) {
            coreferences <- 
                xpath_apply(doc, "//coreference/coreference",
                            info_for_coreferences)

            y <- merge(y,
                       Annotation(next_id(y$id),
                                  "document",
                                  1L,
                                  nchar(s),
                                  list(list(coreferences =
                                            coreferences))))
        }
                              
        y
    }


    description <-
        sprintf("Computes annotations using a Stanford CoreNLP annotator pipeline with the following annotators: %s.",
                paste(annotators, collapse = ", "))

    Annotator(f, list(description = description))
}

info_for_token <-
function(x)
{
    kids <- xml_children(x)
    y <- xml_text(kids)
    names(y) <- xml_name(kids)
    c(xml_attr(x, "id"), y)
}  

info_for_sentence <-
function(x)
{
    xpath_apply(x, "./tokens/token", info_for_token)
}

sentence_dependencies <-
function(x) {
    y <- xpath_apply(x, "./dependencies", info_for_dependencies)
    names(y) <- xml_attr(xml_find_all(x, "./dependencies"), "type")
    y
}

info_for_dependencies <-
function(x)
{
    y <- data.frame(type =
                        xml_attr(xml_find_all(x, "./dep"), "type"),
                    gid =
                        as.integer(xml_attr(xml_find_all(x, "./dep/governor"),
                                            "idx")),
                    governor =
                        xml_text(xml_find_all(x, "./dep/governor")),
                    did = 
                        as.integer(xml_attr(xml_find_all(x, "./dep/dependent"),
                                            "idx")),
                    dependent =
                        xml_text(xml_find_all(x, "./dep/dependent")),
                    stringsAsFactors = FALSE)
    class(y) <- c("Stanford_typed_dependencies", class(y))
    y
}

#' @noRd
#' @export
format.Stanford_typed_dependencies <-
function(x, ...)
    sprintf("%s(%s-%s, %s-%s)",
            x$type, x$governor, x$gid, x$dependent, x$did)

#' @noRd
#' @export
print.Stanford_typed_dependencies <-
function(x, ...)
{
    writeLines(format(x))
    invisible(x)
}

info_for_coreferences <-
function(x)
{
    f <- function(x, p) as.integer(xml_text(xml_find_all(x, p)))
    ## There is also xml2::xml_integer() ...
    y <- data.frame(sentence = f(x, "./mention/sentence"),
                    start = f(x, "./mention/start"),
                    end = f(x, "./mention/end") - 1L,
                    head = f(x, "./mention/head"),
                    text = xml_text(xml_find_all(x, "./mention/text")),
                    stringsAsFactors = FALSE)
    a <- xml_attr(xml_find_all(x, "./mention"), "representative")
    cbind(representative = !vapply(a, is.na, NA, USE.NAMES = FALSE), y)
}

read_dot_properties <-
function(file)
{
    ## A simple reader for the .properties files used by Stanford
    ## CoreNLP.
    ## See <https://en.wikipedia.org/wiki/.properties>.
    lines <- readLines(file, warn = FALSE)
    Encoding(lines) <- "UTF-8"
    lines <- lines[!grepl("^[[:space:]]*$", lines)]
    keys <- trimws(sub("(^[^=]*)=.*", "\\1", lines))
    values <- trimws(sub("^[^=]*= *", "", lines))
    names(values) <- keys
    values
}

xpath_apply <-
function(x, p, FUN, ...)
    lapply(xml_find_all(x, p), FUN, ...)
