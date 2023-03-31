# benchlingr
### Unofficial R Interface to the "Benchling Cloud R&D" Laboratory Information Management System (LIMS).  

![Ubuntu R package check, build, and test](https://github.com/hemoshear/benchlingr/actions/workflows/check-full.yml/badge.svg)
![Mac OS R package check, build, and test](https://github.com/hemoshear/benchlingr/actions/workflows/check-full-macos.yml/badge.svg)
![Windows R package check, build, and test](https://github.com/hemoshear/benchlingr/actions/workflows/check-full-windows.yml/badge.svg)


[Benchling](https://www.benchling.com/) is a laboratory information management system (LIMS) with a [developer platform](https://www.benchling.com/developer-platform) that is comprised of a Postgres database, an application programming interface (API), an "Events" system that is built on top of the AWS EventBridge service, as well as a Python software development kit (SDK).

`benchlingr` is an unofficial R package that streamlines common data science and engineering operations by providing functions to extract and join tables in the Benchling data warehouse, download file attachments, extract tables from notebook entries, and upload assay results to the Benchling platform. 

### Installation

Install the R package directly from GitHub using the `install_github` function in the remotes package. Use `install.packages('remotes')` to install the `remotes` package if you do not already have it and see the documentation with `?remotes::install_github` for more information.

```
remotes::install_github("hemoshear/benchlingr")
```

### Documentation

Read the [documentation on GitHub](https://benchling-r.info/)

### Getting help

Please use GitHub issues for questions, feature requests, and bug reports. If you are submitting a bug report, please include the output of `sessioninfo::session_info()`, as well as your code to help us understand the problem. 


### Benchling credentials

The `benchlingr` package uses the RESTFUL API and Postgres database components of the Benchling developer platform. In order to access the API and database, one must acquire API keys and database credentials through the Benchling interface. This section will focus on how to make these credentials available to the `benchlingr` package. 

*Note: If you are unable to generate an API key and/or database credentials, you may need to ask your Benchling administrator for developer access*

#### API

In order to access the Benchling API service on one's Benchling tenant, [one must obtain an API key through the Benchling interface.](https://help.benchling.com/en/articles/2353570-access-the-benchling-developer-platform-enterprise). After obtaining an API key, it is recommended that you define a variable in `.Renviron` called `BENCHLING_API_KEY` to make this key accessible to the `benchlingr` package, since functions in the package will look for `BENCHLING_API_KEY` by default.

```
# ~/.Renviron

# Benchling API key
BENCHLING_API_KEY=xxxxxxxxxxx
```

#### Data warehouse

[Follow the instructions in the official documentation](https://docs.benchling.com/docs/getting-started#obtaining-credentials) to obtain a username and password for the data warehouse for your Benchling tenant. After obtaining a username and password, it is recommended that one define a variable in `.Renviron` called `BENCHLING_WAREHOUSE_KEY` and `BENCHLING_WAREHOUSE_PASSWORD` to make the credentials available to `benchlingr` with the default settings. 

```
# ~/.Renviron

# Benchling API key
BENCHLING_API_KEY=xxxxxxxxxxx

# Benchling warehouse credentials
BENCHLING_WAREHOUSE_USERNAME=yyyyyyyyyyy
BENCHLING_WAREHOUSE_PASSWORD=xxxxxxxxxxx
```


### Contributing

See the [contribution guidelines](https://github.com/hemoshear/benchlingr/wiki/Contribution-guidelines) for more information.

