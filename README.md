# Buy Vs Rent Thingamajig

The Buy Vs Rent Thingamajig is a web-based application that helps
users understand whether it would be better long term renting or buying a
house with a mortgage. The application is built with
R Shiny and deployed using shinylive. Check out the live application at
<https://shiny.thecoatlessprofessor.com/buy-vs-rent/>

With the shiny app, you can:

- Explore the financial implications of renting vs buying a home
- Visualize costs over time with interactive plots
- Compare different scenarios based on user inputs

## Deployment

This application is deployed using shinylive, allowing it to run directly in
the browser without requiring an R server. Shinylive converts the R code to 
WebAssembly, making it possible to run R applications entirely client-side.

## Local Development Environment

1. Clone the repository:

```bash
git clone https://github.com/coatless-shiny/buy-vs-rent.git
```

2. Open the `buy-vs-rent.Rproj`

3. Install required R packages:

```r
install.packages(c("shiny", "bslib", "dplyr", "DT", "plotly"))
```

4. Run the application:

```r
shiny::runApp()
```

5. Check if the application can be converted to `{shinylive}`:

```r
shinylive::export(".", "_site")
```

## Acknowledgments

- Built using the R Shiny framework
- Uses the bslib package for Bootstrap 5 theming
- Deployed using shinylive for browser-based execution
