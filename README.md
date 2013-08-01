OpenCPU App: Gitstats
---------------------

Simple OpenCPU Application. To install in R:

    library(devtools)
    install_github("opencpu", "jeroenooms")
    install_github("gitstats", "opencpu")

    library(opencpu)
    opencpu$browse("library/gitstats/www")

Use the same function locally:

    library(gitstats)
    gitstats()
    ?gitstats

For more information about OpenCPU apps, see [opencpu.js](https://github.com/jeroenooms/opencpu.js#readme)
