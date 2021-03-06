% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/client.R
\name{nlp_server_docker_run}
\alias{nlp_server_docker_run}
\title{Create the Docker Run Command}
\usage{
nlp_server_docker_run(name = "coreNLP", docker = "schwe/corenlp",
  memory = "-mx8g", threads = NA, port = 9000L,
  status_port = 9000L, timeout = 15000L, strict = FALSE,
  quiet = FALSE, ssl = FALSE,
  key = "edu/stanford/nlp/pipeline/corenlp.jks", username = "",
  password = "", annotators = "all", preload = "",
  server_properties = "", default_properties = "", cleanup = TRUE)
}
\arguments{
\item{name}{a character string giving the name of the docker container.}

\item{docker}{a character string giving the image name.}

\item{memory}{a character string giving the java memory options.}

\item{threads}{an integer giving the number of threads to be used. The default is \code{NA} in which case \code{CoreNLP} will choose the number of threads to be used.}

\item{port}{an integer giving the server port.}

\item{status_port}{an integer giving the port to run the liveness and readiness server on.}

\item{timeout}{an integer giving the maximum amount of time, in milliseconds, to wait for an annotation to finish before cancelling it.}

\item{strict}{a logical controlling whether server strictly follows the \code{HTTP} standards.}

\item{quiet}{a logical controlling whether the incoming requests are logged to \code{STDOUT}.}

\item{ssl}{a logical controlling whether an \code{SSL} should be run.}

\item{key}{a character string giving the classpath or filepath to the \code{*.jks} key to use for creating an SSL connection.}

\item{username}{a character string giving the username of the server.}

\item{password}{a character string giving the password of the server.}

\item{annotators}{a character string giving the default annotators (e.g. \code{"tokenize,ssplit,pos"}).}

\item{preload}{a character string giving the set of annotators to warm up in the cache when the server boots up.}

\item{server_properties}{a character giving the path to the default properties.}

\item{default_properties}{a character string giving the server properties, to be written into the file \code{"default.properties"}.}

\item{cleanup}{a logical giving if docker should automatically clean up the container and remove the file system when the container exits.}
}
\value{
A character string which can be copied into the terminal (or docker toolbox) to start the \code{coreNLP} server.
}
\description{
This function helps to create the docker run command 
  which can be copied into the terminal (Unix) or docker toolbox (Windows).
}
\examples{
nlp_server_docker_run(memory = "-mx6g")
}
