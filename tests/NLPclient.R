
use <- function(pkgs) {
    .use <- function(pkg) 
        (require(pkg, quietly = TRUE, character.only = TRUE) || 
         stop("package '", pkg, "' not found"))
    sapply(pkgs, .use)
}

use(c("NLP", "NLPclient"))

test_annotation <- function() {
    s <- as.String(paste("Stanford University is located in California.",
                         "It is a great university."))

    p <- StanfordCoreNLP_Pipeline(NULL)
    a <- p(s)
    stopifnot(inherits(a, "Annotation"))
}


if ( ping_nlp_client() == "pong" ) {
    test_annotation()
} else {
    print("Startup your CoreNLP server! More information can be found in the README!")
}

