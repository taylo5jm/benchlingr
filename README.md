# benchlingr
### Unofficial R Interface to the "Benchling Cloud R&D" Laboratory Information Management System (LIMS).

Benchling is a laboratory information management system with a developer platform that is comprised of a Postgres database, an application programming interface (API), an "Events" system that is built on top of the AWS EventBridge service, as well as a Python software development kit (SDK). `benchlingr` is a R package that aims to make the developer platform a

#### Installation

Install the R package directly from GitHub using the `install_github` function in the remotes package. Use `install.packages('remotes')` to install the `remotes` package if you do not already have it. 

```
remotes::install_github("hemoshear/benchlingr")
```

##### Development environment

We use the `renv` package to track R dependencies for this project. The list of R package versions is stored in the `renv.lock` file. The `.Rprofile` and `renv/activate.R` files are auto-loaders that `renv` uses to bootstrap itself. After creating a new RStudio project for development on this package, `renv` should install itself and one will be able to use `renv::restore` to install the set of packages defined in the `renv` file.

```
install.packages('renv')
renv::restore()
```
