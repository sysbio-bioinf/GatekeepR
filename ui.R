source("graph_functions.R")

useShinyjs() #Has to be called in UI at some point in order for all other shinyjs functions to work

ui <- fluidPage(
    title = "GatekeepR",
    # Application title + Scaled down dimension of original banner png (2845x273)
    titlePanel(
      title=div(img(src=base64enc::dataURI(file="Institut-Uni_Banner.png", mime="image/png"), width="711px", height="68px"), align="center", 
                h1(HTML('<p>GatekeepR</p> <font size="+1">Boolean network screening for possible sparsely connected intervention targets</font>'), align="center")
                )
    ),
    
    HTML("<p align=\"center\">The publication on which this method is based is available at <a href='https://academic.oup.com/bioinformatics/article/37/20/3530/6275260' target='_blank'>Weidner et al., Bioinformatics, 2021</a>. <br> If you find this application useful, please cite the article.</p>"),

    # Sidebar
    h3("Input"),
    fluidRow(style='margin-left: 2%; margin-right: 2%',
      column(6, fileInput("networkTXTfile", "Please upload a .txt or .sbml file with network rules:",
                multiple = FALSE, #buttonLabel = "Select a network file", 
                #placeholder = "No network selected yet",
                accept = c("text/txt",
                           "text/plain",
                           ".txt",
                           ".sbml"))),
      column(6, actionButton("exampleButton","Load Example with random network")),
      column(2,textOutput("confirmedFile")),
      column(2,textOutput("netsize")),
      column(2,textOutput("nredges"))),
    hr(),
    h3("Configuration"),
    fluidRow(style='margin-left: 2%; margin-right: 2%',
             column(5,sliderInput("threshold",
                                  "Selection threshold T:",
                                  min = 1,
                                  max = 100,
                                  value = 73,
                                  ticks=TRUE
             )),
             column(6,
      prettySwitch("shownodelabels", label="Show node labels in plot", value=TRUE, fill=FALSE),
      selectInput("LayoutAlgo", "Graph layout algorithm:",
                  c("Automatic choice" = "nicely",
                    "GEM" = "GEM",
                    "Graphopt" = "graphopt",
                    "Large Graph Layout" = "LGL",
                    "Circle" = "circle",
                    "Tree" = "tree",
                    "Grid" = "grid",
                    "Fruchterman-Reingold" = "fr",
                    "Kamada-Kawai" = "kk",
                    "Davidson-Harel" = "dh"
                  )))),
    
    hr(),
    conditionalPanel(condition= "output.fileUploaded",
    h3("Results"),
    fluidRow(style='margin-left: 2%; margin-right: 2%',
             plotOutput("intGraph"),
             hr(),
             column(3,htmlOutput("LegendGK")),
             column(3,htmlOutput("LegendHub")),
             column(3,htmlOutput("LegendGKHub"))),
    hr(),
             #column(4, screenshotButton(label="Take a screenshot")),
    downloadButton(outputId="saveplot", label="Save interaction graph as pdf"),
    h4("Runtime:"),
    textOutput("runtime"),
    h4("Suggested intervention targets, sorted by mismatch:"),
    tableOutput("GKsWithMismatch"),
    h4("Node classification:"),
    verbatimTextOutput("PMindices"),
    verbatimTextOutput("NMindices"),
    verbatimTextOutput("NSindices"),
    
    h4("Network measures:"),
    #h5(HTML("<u>Genes sorted as in txt file:</u>")),
    DT::dataTableOutput("table"),
    #h5(HTML("<u>Genes sorted by ranking on measures:</u>")),
    #tableOutput("sortedVBvec"),
    #tableOutput("sortedDPvec"),
    #tableOutput("sortedZvec"),
    
    h4("Network rules: "),
    verbatimTextOutput("rules"),#Print network rules
    
    tags$details(
      tags$summary(HTML("<u><b>Click here to show short summary of method</b></u>")),
      HTML("
      The method used here presents a ranked list of possible intervention targets in the provided Boolean network.
      These target nodes are determined in a two-step process.
      First, only nodes scoring high on both the measures of vertex betweenness (VB) and determinative power (DP) are retained. 
      The strictness of this selection can be tuned via the slider for the threshold T above. 
      The default value provided here has yielded an optimal balance of sensitivity and specificity in a binary classification
      of nodes into categories of high and low dynamic impact.
      In the second step, the retained nodes are further classified by comparing their ranking on the measures of VB and DP
      with their connectivity ranking. The recommended targets are sparsely connected nodes with a high mismatch of large impact
      on VB and DP despite relatively low connectivity. These nodes were found to preferentially exchange mutual information with 
      highly connected hub nodes and have thus been named gatekeepers.
      Note that the impact of perturbations was determined as the maximal change in the network's attractor landscape. 
      Therefore, this does not give information about whether such changes can be achieved using overexpressions or knockouts 
      of the network component, or whether the change leads the system to any particular desired behaviour.
      The presented recommended nodes may therefore be analysed in more detail, e.g. using perturbation screening tools
      such as <a href='https://sysbio.uni-ulm.de/?Software:ViSiBooL' target='_blank'>ViSiBooL</a>, 
      see <a href='https://academic.oup.com/bioinformatics/article/33/4/601/2593908' target='_blank'>Schwab et al. (2017)</a>.
      ")
    )),
    HTML("<p>The R code for this application is available on <a href='https://github.com/sysbio-bioinf/GatekeepR' target='_blank'>github</a>.</p>")
) #END of fluidPage
