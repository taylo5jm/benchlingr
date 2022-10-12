# benchlingr
### Unofficial R Interface to the "Benchling Cloud R&D" Laboratory Information Management System (LIMS).  

![R package check, build, and test](https://github.com/hemoshear/benchlingr/actions/workflows/check-full.yaml/badge.svg)

Benchling is a laboratory information management system with a developer platform that is comprised of a Postgres database, an application programming interface (API), an "Events" system that is built on top of the AWS EventBridge service, as well as a Python software development kit (SDK). `benchlingr` is a R package that aims to make the developer platform accessible to data scientists.

### Installation

Install the R package directly from GitHub using the `install_github` function in the remotes package. Use `install.packages('remotes')` to install the `remotes` package if you do not already have it and see the documentation with `?remotes::install_github` for more information.

```
remotes::install_github("hemoshear/benchlingr")
```

### Documentation

- [GitHub Pages](upgraded-succotash-8df2b926.pages.github.io/)


### Configuration

The `benchlingr` package primarily utilizes the RESTFUL API service and Postgres database services of the Benchling developer platform. In order to use these components of the Benchling platform, one must acquire API keys and database credentials through the Benchling interface. This section will focus on how to make these credentials available to the `benchlingr` package. 

#### API

In order to access the Benchling API service on one's Benchling tenant, one must obtain an API key through the Benchling interface. [Follow the instructions in the official documentation](https://help.benchling.com/en/articles/2353570-access-the-benchling-developer-platform-enterprise) to obtain an API key. After obtaining an API key, it is recommended that you define a variable in `.Renviron` called `BENCHLING_API_KEY` to make this key accessible to the `benchlingr` package with the default function arguments. 

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

##### Development environment

We use the `renv` package to track R dependencies for this project. The list of R package versions is stored in the `renv.lock` file. The `.Rprofile` and `renv/activate.R` files are auto-loaders that `renv` uses to bootstrap itself. After creating a new RStudio project for development on this package, `renv` should install itself and one will be able to use `renv::restore` to install the set of packages defined in the `renv` file.

```
install.packages('renv')
renv::restore()
```

