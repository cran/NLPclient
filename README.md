# NLPclient

## Stanford CoreNLP
[Stanford CoreNLP](https://stanfordnlp.github.io/CoreNLP/index.html) is a 
`Java` suite for Natural Language Processing (NLP). 
In its new version it includes a web API server. The **NLPclient** package
provides client access to the *CoreNLP server*.
In the following we provide information on how to set up and to use **NLPclient**,
for more information about the [CoreNLP Server](https://stanfordnlp.github.io/CoreNLP/corenlp-server.html)
we refer to `https://stanfordnlp.github.io/CoreNLP/corenlp-server.html`.

## Install NLPclient
To install **NLPclient** from CRAN, execute:
```
install.packages("NLPclient")
```
For **NLPclient** to do anything useful *CoreNLP server* has to be installed.
*CoreNLP server* can either be run by starting the server with `Java` directly 
or by running the server under docker. For running *CoreNLP server* with `Java`
we refer to the [CoreNLP Server](https://stanfordnlp.github.io/CoreNLP/corenlp-server.html)
documentation.

## Install docker
The **RSelenium** documentation provides a nice introduction on the usage of docker
which also gives guidance on the [docker installation](https://cran.r-project.org/package=RSelenium/vignettes/docker.html).

## Run CoreNLP server with docker
We recommend to use the docker image `schwe/corenlp` from `https://hub.docker.com/r/schwe/corenlp`
it is derived from `frnkenstien/corenlp` but allows to insert arguments into server startup
and to specify default properties. The image can be obtained by
```{bash}
docker pull schwe/corenlp
```

To start the *CoreNLP server* a command similar to 
```{bash}
docker run -p 9000:9000 --name coreNLP --rm -i -t schwe/corenlp
```
can be used, this will also download the docker image if necessary.
On Linux it might by necessary to run docker with `sudo` or to
[setup docker to be manageable a non-root user](https://docs.docker.com/install/linux/linux-postinstall/).

The function `nlp_server_docker_run` can be used to create the run command from within `R`.
```
nlp_server_docker_run(memory = "-mx4g", threads = 2L)
#R> "docker run -p 9000:9000 --name coreNLP --env PREARGS='-mx4g' --env SUBARGS='-threads 2' --rm -i -t schwe/corenlp"
```
`nlp_server_docker_run` returns the run command as a string, to run the server the command has 
to be copied into the terminal (on Unix) or the docker toolbox (on Windows).
