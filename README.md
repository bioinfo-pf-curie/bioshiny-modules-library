1 Description
=============

This library aims to provide R shiny apps developers of the Institut
Curie with different modules witch can be used as bricks to build bigger
shiny apps.

2 Installation
==============

-   Download and unzip the source code
-   Create an empty app project directory and copy **renv.lock** into it
-   Then run the following command :

``` r
  renv::init("path_to_your_app_folder/").
```

This should install the library dependancies.

-   Finally install the library

``` r
install.packages("path_to_bioshiny_module_library_folder/", type = "source", repos = NULL)
```

3 Call a module
===============

To do so you have to call two functions, one in the server and the
second in the UI part.

3.1 Server Part
---------------

In the CallModule function you stipulate the library module that you
want to call (typically called **ModuleName**Server), and an id for this
specific call.

``` r
  server <- function(input, output, session) {
    observe({
    PCA <- callModule(module = DrawPCAServer2,id = "PCA1",
                     matrix = counts ,
                      metadata = metadata)
    })
}
```

*NB :* Some of the modules accept arguments of type reactiveValues.
Description of the arguments are provided in the ModuleNameServer
functions’ help sections. This wad different modules can communicates
and share data objects.

3.2 UI part
-----------

In the UI call the corresponding **ModuleName** UI function, with the
matching id.

``` r
ui <- dashboardPage(
  dashboardHeader(title = "PCA Module Test"),
  dashboardSidebar(),
  dashboardBody(
    fluidPage(
    fluidRow(
      DrawPCAUI2("PCA1")
    ))
  )
)
```

4 Modules List
==============

The current available modules can be test executing the whole code in
the corresponding **run\_dev\_ModuleNameModule.R** file in the </br>

- [inst/Tests/](inst/Tests/)
directory

5 Contact
=========

If you have any question, suggestions or bug reports please contact
<pierre.gestraud@curie.fr> and <clement.benoit@curie.fr>
