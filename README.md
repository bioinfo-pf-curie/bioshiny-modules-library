\[\]!(images/curie.base64.txt) \[\]!(images/bioshiny.base64.txt)

1 Description
=============

This library aims to provide R shiny apps developper of the Institut
Curie with different modules witch can be used as bricks to build bigger
shiny apps.

2 Installation
==============

-   Download and unzip the source code
-   run the following command :
    renv::init(“path\_to\_bioshiny\_module\_library\_folder/”). This
    should install the library dependancies.
-   Finally install the library
    install.packages(“path\_to\_bioshiny\_module\_library\_folder/”,
    type = “source”, repos = NULL)

3 Call a module
===============

To do so you have to call two functions, one in the server and the
second in the UI part.

3.1 Server Part
---------------

In the CallModule function you stipulate the library module that you
want to call (tipically called ModuleNameServer), and an id for this
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

In the UI call the corresponding ModuleNameUI function, with the
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

4 Contact
=========

If you have any question or suggestions please contact
<pierre.gestraud@curie.fr> and <clement.benoit@curie.fr>
