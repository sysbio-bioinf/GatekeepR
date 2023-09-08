source("graph_functions.R")

function(input, output, session) {
  
  output$LegendGK <- renderText("<font color='red'>Red</font> = Gatekeeper, higher mismatch has higher opacity")
  output$LegendHub <- renderText("<font color='blue'>Blue</font> = Hub node, connectivity z-score > 2.5")
  output$LegendGKHub <- renderText("<font color='purple'>Purple</font> = Hub that is also classified as gatekeeper")
  
    observe({
      print("Separate threshold observer triggered")
      #print(input$threshold)
    })
    
  
    #OBSERVE IF A NETWORK FILE HAS BEEN UPLOADED
    observeEvent(input$networkTXTfile, #Alternative, works as well, but does not react to changing input$threshold
    if (is.null(input$networkTXTfile)){
      output$confirmedFile <- renderText({"No network loaded yet!"}) #typeof(input$networkTXTfile) gives NULL
    } else {
      print("Large observer triggered, recalculating measures...")
      #Print error if wrong file format is uploaded
      ending <- tools::file_ext(input$networkTXTfile$name)
      if (ending != "txt"){
        shinyalert("Wrong file format", "Input file needs to be in .txt format", type = "error")
      }
      
      network <- BoolNet::loadNetwork(input$networkTXTfile$datapath)
      netsize <- length(network$genes)
      adjmat <- conv2adjmat(network)
      gr <- igraph::graph_from_adjacency_matrix(adjmat)
      nredges <- sum(adjmat)
      
      output$confirmedFile <- renderText({"Network loaded successfully!"}) #typeof(input$networkTXTfile) gives list
      output$netsize <- renderText({paste0("Network size: ", netsize)})
      output$nredges <- renderText({paste0("Number of interactions: ", nredges)})
      
      #Measure runtime of calculating static measures
      starttime <- Sys.time()
      #Print 'in progress'-message while measures are being calculated
      showModal(modalDialog("Calculating network measures...", footer=NULL))
      staticmeasures <- calculateMeasures(network)
      removeModal()
      endtime <- Sys.time()
      runtime <- endtime - starttime
      VB <- staticmeasures$VB
      DP <- staticmeasures$DP
      Z <- staticmeasures$Z
      output$runtime <- renderText({paste0("Calculation of static measures finished after ", round(runtime,2), " seconds.")})
      
      VBrounded <- round(VB,3)
      DProunded <- round(DP,3)
      Zrounded <- round(Z,3)
      
      measuresmatrix <- matrix(NA, nrow=3, ncol=netsize)
      measuresmatrix[1,] <- VBrounded
      measuresmatrix[2,] <- DProunded
      measuresmatrix[3,] <- Zrounded
      rownames(measuresmatrix) <- c("VB", "DP", "Z")
      colnames(measuresmatrix) <- network$genes
      
      sortedVB <- as.data.frame(t(sort(VBrounded, decreasing = TRUE)))
      rownames(sortedVB) <- "VB"
      sortedDP <- as.data.frame(t(sort(DProunded, decreasing = TRUE)))
      rownames(sortedDP) <- "DP"
      sortedZ <- as.data.frame(t(sort(Zrounded, decreasing = TRUE)))
      rownames(sortedZ) <- "Z"
      
      #INNER OBSERVER for changing input$threshold slider
      #classificationResult <- classify(network, VB, DP, Z, threshold=input$threshold)
      observeEvent(input$threshold,
      {
      classificationResult <- classify(network, VB, DP, Z, threshold=input$threshold)
      
      PMindices <- classificationResult$PMindices
      NMindices <- classificationResult$NMindices
      NSindices <- classificationResult$NSindices
      hubindices <- classificationResult$hubindices

      #Display popup warning message using shinyalert if some static measure does not vary (i.e. connectvity in very small networks)
      if (sd(Z) == 0){
        shinyalert("No variation", "Connectivity has only one unique value for this network.", type = "warning")
      }
      if (sd(VB) == 0){
        shinyalert("No variation", "Vertex Betweenness has only one unique value for this network.", type = "warning")
      }
      if (sd(DP) == 0){
        shinyalert("No variation", "Determinative Power has only one unique value for this network.", type = "warning")
      }
      
      #Print priority-sorted list of GKs to UI:
      if (length(PMindices)>0){
        output$GKsWithMismatch <- renderTable(classificationResult$GKswithMismatch)
      } else {
        output$GKsWithMismatch <- renderText({"No gatekeeper nodes found, try a higher threshold."})
      }
  
      #Output node classification:    
      output$PMindices <- renderText({
        paste("Gatekeeper:", paste(network$genes[classificationResult$PMindices], collapse=", "))
      })
      output$NMindices <- renderText({
        paste("Selected, but negative mismatch:", paste(network$genes[classificationResult$NMindices], collapse=", "))
      })
      output$NSindices <- renderText({
        paste("Low impact, non-selected nodes:", paste(network$genes[classificationResult$NSindices], collapse=", "))
      })
      
      #List network rules at the bottom
      output$rules <- renderText({
        paste(readLines(input$networkTXTfile$datapath), collapse = "\n")
      })
      
      #PLOT OF INTERACTION GRAPH
      output$intGraph <- renderPlot({
        nodelabels <- network$genes
        keeplabel <- union(PMindices, hubindices)
        nodelabels[setdiff(seq(1,length(network$genes)), keeplabel)] <- "" #no label for genes that are not GK or hub, reduce visual clutter
        
        vcolvec <- rep("white", netsize)
        vframevec <- rep("grey", netsize)
        vcolvec[PMindices] <- vframevec[PMindices] <- "red"
        vcolvec[hubindices] <- vframevec[hubindices] <- "blue"
        vcolvec[intersect(PMindices, hubindices)] <- vframevec[intersect(PMindices, hubindices)] <- "purple"
        
        
        #Give GK nodes different levels of transparency depending on the mismatch, higher ranking recommendation = more opaque
        if (length(PMindices) > 0){
          alphavec <- rep(1, netsize)
          roundedMismatchVals <- round(classificationResult$GKswithMismatch,2)
          uniqueRoundedMismatchVals <- unique(unlist(roundedMismatchVals))
          nrUniqueMismatches <- length(uniqueRoundedMismatchVals)
          if (nrUniqueMismatches > 1){
            alphavals <- seq(1,0.1,by=-0.7/(nrUniqueMismatches-1))
            for (gk in 1:length(roundedMismatchVals)){
              genename <- colnames(roundedMismatchVals)[gk]
              chosenalpha <- alphavals[which(uniqueRoundedMismatchVals == unlist(roundedMismatchVals[gk]))]
              vcolvec[which(network$genes == genename)] <- adjustcolor(vcolvec[which(network$genes == genename)], chosenalpha)
            }
          }#more than one unique mismatch value
        }#end GK existence check
        
        
        
        #Change layout scheme of interaction graph
        if (input$LayoutAlgo == "nicely"){layoutChoice=layout_nicely(gr)} #igraph automatically searches for a fitting layout
        else if (input$LayoutAlgo == "circle"){layoutChoice=layout.circle(gr)}
        else if (input$LayoutAlgo == "tree"){layoutChoice=layout_as_tree(gr)}
        else if (input$LayoutAlgo == "grid"){layoutChoice=layout_on_grid(gr)}
        else if (input$LayoutAlgo == "GEM"){layoutChoice=layout_with_gem(gr)}
        else if (input$LayoutAlgo == "graphopt"){layoutChoice=layout_with_graphopt(gr)} #Force-directed layout, scales relatively well to large graphs
        else if (input$LayoutAlgo == "LGL"){layoutChoice=layout_with_lgl(gr)}
        else if (input$LayoutAlgo == "fr"){layoutChoice=layout_with_fr(gr)}
        else if (input$LayoutAlgo == "kk"){layoutChoice=layout=layout_with_kk(gr)}
        else if (input$LayoutAlgo == "dh"){layoutChoice=layout_with_dh(gr)}
      
        
        par(mar = c(0,0,0,0)) #remove whitespace around plotOutput
        if (input$shownodelabels){
          intgraph_plot <- plot(gr, vertex.label = nodelabels, vertex.color=vcolvec, edge.color="grey", vertex.label.color="black", layout = layoutChoice,
                                vertex.size=2*Z+2*abs(min(Z))+5, vertex.label.size=Z+abs(min(Z)+5), edge.arrow.size=0.5, loop.size=0.5,
                                vertex.frame.color=vframevec, vertex.label.font=2, #2=bold labels
          )
          
          output$saveplot <- downloadHandler(
            file = paste0(file_path_sans_ext(input$networkTXTfile$name), "_interactiongraph.pdf"),
            content = function(file) {
              pdf(file=file)
              plot(gr, vertex.label = nodelabels, vertex.color=vcolvec, edge.color="grey", vertex.label.color="black", layout = layoutChoice,
                   vertex.size=2*Z+2*abs(min(Z))+5, vertex.label.size=Z+abs(min(Z)+5), edge.arrow.size=0.5, 
                   loop.size=0.5, main=paste0("Interaction graph of the ", file_path_sans_ext(input$networkTXTfile$name), " network"),
                   vertex.frame.color=vframevec, vertex.label.font=2, #2=bold labels
              )
              dev.off()
            })
          
        } else {
          intgraph_plot <- plot(gr, vertex.label = rep("", netsize), vertex.color=vcolvec, edge.color="grey", vertex.label.color="black", layout = layoutChoice,
               vertex.size=2*Z+2*abs(min(Z))+5, vertex.label.size=Z+abs(min(Z)+5), edge.arrow.size=0.5, loop.size=0.5,
               vertex.frame.color=vframevec, vertex.label.font=2, #2=bold labels
          )
          
          output$saveplot <- downloadHandler(
            file = paste0(file_path_sans_ext(input$networkTXTfile$name), "_interactiongraph.pdf"),
            content = function(file) {
              pdf(file=file)
              plot(gr, vertex.label = nodelabels, vertex.color=vcolvec, edge.color="grey", vertex.label.color="black", layout = layoutChoice,
                   vertex.size=2*Z+2*abs(min(Z))+5, vertex.label.size=Z+abs(min(Z)+5), edge.arrow.size=0.5, 
                   loop.size=0.5, main=paste0("Interaction graph of the ", file_path_sans_ext(input$networkTXTfile$name), " network"),
                   vertex.frame.color=vframevec, vertex.label.font=2, #2=bold labels
              )
              dev.off()
            })
          
        }
        #Less pronounced size differences in vertices, but scaling with connectivity is still present
      })#end of renderPlot
    
      })#End of inner observer for input$threshold slider
      
      #Tables with network measures
      output$table <- renderTable(measuresmatrix, rownames=TRUE, colnames=TRUE)
      output$sortedVBvec <- renderTable(sortedVB, rownames=TRUE, colnames=TRUE)
      output$sortedDPvec <- renderTable(sortedDP, rownames=TRUE, colnames=TRUE)
      output$sortedZvec <- renderTable(sortedZ, rownames=TRUE, colnames=TRUE)
 
      
      
      
    }) #End of scope of observer for currently loaded network
}
