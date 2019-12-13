#' @importFrom "NLP" "Annotation" "Annotator" "as.String" "meta<-" "next_id"
#' @importFrom "utils" "head" "tail" "URLencode"
#' @importFrom "xml2" "read_xml" "xml_attr" "xml_attrs" "xml_children" "xml_find_all" "xml_name" "xml_text"
#' @importFrom "curl" "curl_fetch_memory" "new_handle" "handle_setopt" "handle_setheaders"


## Input example from <https://stanfordnlp.github.io/CoreNLP/corenlp-server.html>.
## "inputFormat": "serialized", "inputSerializer", "edu.stanford.nlp.pipeline.ProtobufAnnotationSerializer", "annotators": "tokenize,ssplit,pos,lemma,ner", "outputFormat": "serialized", "serializer", "edu.stanford.nlp.pipeline.ProtobufAnnotationSerializer"
build_properties <- function(annotators, output_format, control) {
    x <- c(list(annotators = annotators, outputFormat = output_format), control)
    join <- function(key, val) {
        type <- "cmd"
        sprintf('%s: %s', shQuote(key, type = type), paste(shQuote(val, type = type), collapse = ', '))
    }
    paste(mapply(join, names(x), x, USE.NAMES = FALSE), collapse = ', ')
}

# Example:
# cmd <- build_properties(annotators = "tokenize,ssplit,pos,lemma,ner", output_format = "xml", 
#     control = list(inputFormat = c("serialized", "inputSerializer", "edu.stanford.nlp.pipeline.ProtobufAnnotationSerializer")))
# cat(cmd, "\n")


# sprintf('http://%s:%i/?properties={"annotators": "%s", "outputFormat": "%s"}', host, port, annotators, output_format)
build_request_url <- function(host, port, annotators, output_format = "xml", control = list()) {
    props <- build_properties(annotators, output_format, control)
    sprintf('http://%s:%i/?properties={%s}', host, port, props)
}
# Example:
# sprintf('http://%s:%i/?properties={"annotators": "%s", "outputFormat": "%s"}', "localhost", 9000L, "tokenize,ssplit,pos", "xml")
# build_request_url("localhost", 9000L, "tokenize,ssplit,pos")

## 
#' @title Ping Stanford CoreNLP Server
#' @description Ping Stanford CoreNLP server.
#' @param port an integer giving the port (default is \code{9000L}).
#' @param host a character string giving the hostname of the server.
#' @param user_agent a character string giving the client name.
#' @return Returns \code{"pong"} if the server is online \code{""} otherwise.
#' @examples
#' ping_nlp_client()
#' @export
ping_nlp_client <- function(port = 9000L, host = "localhost", user_agent = "NLPclient") {
    handle <- new_handle()
    handle_setopt(handle, port = port)
    handle_setopt(handle, customrequest = "POST")
    handle_setopt(handle, followlocation = TRUE)
    handle_setheaders(handle,
        "Content-Type" = "application/json",
        "User-Agent" = user_agent)
    request_ping <- sprintf('http://%s:%i/ping', host, port)
    response <- tryCatch(curl_fetch_memory(request_ping, handle), error = identity)
    if ( inherits(response, "error") ) "" else gsub("\\s", "", rawToChar(response$content))
}


new_nlp_client <- function(annotators, port = 9000L, host = "localhost", user_agent = "NLPclient", control = list()) {
    env <- new.env(parent = emptyenv())
    env$port <- as.integer(port)
    env$host <- host
    env$handle <- NULL
    env$new_handle <- function() {
        self <- parent.env(environment())$env
        handle <- new_handle()
        handle_setopt(handle, port = port)
        handle_setopt(handle, customrequest = "POST")
        handle_setopt(handle, followlocation = TRUE)
        handle_setheaders(handle,
            "Content-Type" = "application/json",
            "User-Agent" = user_agent)
        self$handle <- handle
    }
    env$annotators <- annotators
    env$request_url <- URLencode(build_request_url(host, port, paste(annotators, collapse = ","), control = control))
    env$annotate <- function(s) {
        self <- parent.env(environment())$env
        handle_setopt(self$handle, postfields = s)
        response <- tryCatch(curl_fetch_memory(self$request_url, self$handle), error = identity)
        if ( inherits(response, "error") ) {
            stop(response$message, call. = FALSE, domain = "NLPclient")
        }
        rawToChar(response$content)
    }
    env$new_handle()
    class(env) <- "nlp_client"
    env
}


parse_kwargs <- function(kwargs) {
    .to_character <- function(key, value) {
        if ( is.logical(value) ) {
            val <- if ( value ) "true" else "false"
        } else if ( is.integer(value) ) {
            val <- sprintf("%i", value)
        } else if ( is.double(value) ) {
            if ( !is.finite(value) ) return("")
            val <- sprintf("%i", as.integer(value))
        } else if ( is.character(value) ) {
            if ( nchar(value) == 0L ) return("")
            val <- value
        } else {
            stop("type error")
        }
        sprintf('-%s %s', key, val)
    }
    s <- mapply(.to_character, names(kwargs), kwargs, USE.NAMES = FALSE)
    paste(s[nchar(s) > 0] , collapse = " ")
}


## 
#' @title Create the Docker Run Command
#' @description This function helps to create the docker run command 
#'   which can be copied into the terminal (Unix) or docker toolbox (Windows).
#' @param name a character string giving the name of the docker container.
#' @param docker a character string giving the image name.
#' @param memory a character string giving the java memory options.
#' @param threads an integer giving the number of threads to be used. The default is \code{NA} in which case \code{CoreNLP} will choose the number of threads to be used.
#' @param port an integer giving the server port.
#' @param status_port an integer giving the port to run the liveness and readiness server on.
#' @param timeout an integer giving the maximum amount of time, in milliseconds, to wait for an annotation to finish before cancelling it.
#' @param strict a logical controlling whether server strictly follows the \code{HTTP} standards.
#' @param quiet a logical controlling whether the incoming requests are logged to \code{STDOUT}.
#' @param ssl a logical controlling whether an \code{SSL} should be run.
#' @param key a character string giving the classpath or filepath to the \code{*.jks} key to use for creating an SSL connection.
#' @param username a character string giving the username of the server.
#' @param password a character string giving the password of the server.
#' @param annotators a character string giving the default annotators (e.g. \code{"tokenize,ssplit,pos"}).
#' @param preload a character string giving the set of annotators to warm up in the cache when the server boots up.
#' @param server_properties a character giving the path to the default properties.
#' @param default_properties a character string giving the server properties, to be written into the file \code{"default.properties"}.
#' @param cleanup a logical giving if docker should automatically clean up the container and remove the file system when the container exits.
#' @return A character string which can be copied into the terminal (or docker toolbox) to start the \code{coreNLP} server.
#' @examples
#' nlp_server_docker_run(memory = "-mx6g")
#' @export
nlp_server_docker_run <- function(name = "coreNLP", docker = "schwe/corenlp", 
    memory = "-mx8g", threads = NA, port = 9000L, status_port = 9000L, timeout = 15000L, 
    strict = FALSE, quiet = FALSE, ssl = FALSE, key = "edu/stanford/nlp/pipeline/corenlp.jks", 
    username = "", password = "", annotators = "all", preload = "", server_properties = "",
    default_properties = "", cleanup = TRUE) {

    kwargs <- as.list(match.call())[-1]
    names(kwargs) <- gsub("server_properties", "serverProperties", names(kwargs), fixed = TRUE)
    kwargs <- kwargs[!names(kwargs) %in% c("name", "docker", "memory", "cleanup", "default_properties")]
    if ( "threads" %in% names(kwargs) ) {
        kwargs$threads <- as.integer(threads)
    }
    if ( nchar(default_properties) > 0 ) {
        if ( isTRUE(server_properties == "") ) {
            kwargs$serverProperties <- "default.properties"
        }
        props_args <- sprintf(" --env SERVER_PROPERTIES=%s", shQuote(default_properties))
    } else {
        props_args <- ""
    }
    pre_args <- sprintf(" --env PREARGS=%s", shQuote(memory))
    sub_args <- if ( length(kwargs) ) sprintf(" --env SUBARGS=%s", shQuote(parse_kwargs(kwargs))) else ""
    pre_cmd <- sprintf("docker run -p %i:%i --name %s", port, port, name)
    sub_cmd <- sprintf("%s -i -t %s", if ( cleanup ) " --rm" else "", docker)

    paste(pre_cmd, pre_args, sub_args, props_args, sub_cmd, sep = "")
}

