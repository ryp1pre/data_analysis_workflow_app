# Application for streamlining the analysis of data
# 
# Version 5
# Author: r.y.penchev@gmail.com
# 
# New to Ver.5
# - Parallel coordinates plot using the 'parcoords' library
# 
# Key data frames used in the code:
# - df_sel: contains the variables selected by the user to work with
# - df_sel_row: contains the same vars as 'df_sel' but with some rows de-selected from the user
# - df_sel_nocorr: this is the 'df_sel_row' data frame but with removed cross correlated vars
# 

# LIBRARIES ----
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(DT)
library(psych)               # Used for data summary
library(plotly)
library(caret)
library(rlang) 
library(factoextra)          # Extract & visualize multivariate data analysis
library(FactoMineR)          # Multivariate exploratory data analysis
library(NbClust)             # Number of clusters indices
library(dbscan)              # DBSCAN - density based clustering
library(ggsci)               # Color palettes
library(parcoords)           # Interactive parallel coordinates plot
library(d3r)                 # Required for the parcoords plot colours

# HEADER ----
header <- dashboardHeader(title = "Powder", titleWidth = 230, disable = FALSE)


# SIDEBAR ----
sidebar <- dashboardSidebar(
    hr(),
    sidebarMenu(id = "tabs",
                menuItem("Data import", tabName = "table", icon=icon("table"), selected=TRUE),
                menuItem("Correlations", tabName="correlation", icon=icon("braille")),
                menuItem("Similarities", tabName="similarities_hc", icon=icon("equals")),
                menuItem("Outliers", tabName="similarities_dbscan", icon=icon("buromobelexperte")),
                menuItem("Parallel Plot", tabName="parallel_plot", icon=icon("chart-line")),
                menuItem("About", tabName = "about", icon = icon("question"))  
                ), # end of sidebarMenu
    
    hr(),
    conditionalPanel("input.tabs == 'table'",
                    h4("User Input:"),
                    fileInput("file","Upload csv file", 
                              accept = c("text/csv",
                                         "text/comma-separated-values,text/plain",
                                         ".csv")),
                    # Input: Select file separator
                    radioButtons("sep", "Separator",
                                 choices = c(Semicolon = ";",
                                             Comma = ",",
                                             Tab = "\t"),
                                 selected = ","),
                    # Select variables to display
                    uiOutput("var_select")
    ), 
    
    conditionalPanel("input.tabs == 'correlation'",
                     h4("User Input:"),
                     sliderInput("slider_treshold", "Treshold", min = 0.7, 
                                 max = 1, value = 0.8, step = 0.05, ticks = TRUE),
                     # Copy the line below to make a text input box
                     textInput("var_subset", label = "Variables subset*"),
                     helpText("* Use comma (,) and/or colon (:) seperated numbers! ")
    ), 
    
    conditionalPanel("input.tabs == 'similarities_hc'",
                     h4("User Input:"),
                     # Input: Select the dataset to work with
                     radioButtons("dataset", "Dataset",
                                  choices = c(Correlated = "pre_corr_analysis",
                                              Uncorrelated = "post_corr_analysis"),
                                  selected = "post_corr_analysis"),
                     
                     # Choose PCA pre-processing
                     checkboxInput("pca", "PCA Pre-Processing", FALSE),
                     uiOutput("pc_number_select"),
                     
                     pickerInput(inputId = "pick_distance", label = "Distance", choices = c(Euclidean = "euclidean", 
                                                                                            Manhattan = "manhattan"), 
                                 selected = "Euc"),
                     
                     pickerInput(inputId = "pick_agg_method", label = "Linkage", choices = c(Maximum = "complete",
                                                                                             Ward.D2 = "ward.D2",
                                                                                             Minimum = "single",
                                                                                             Mean = "average"), 
                                 selected = "complete", multiple = FALSE),
                     
                     numericInput("vis_value", label = "Number Of Clusters to View", value = 2, min = 2, max = 9)
    ),
    
    conditionalPanel("input.tabs == 'similarities_dbscan'",
                     h4("User Input:"),
                     numericInput("dbscan_MinPts", label = "Minimum Points", value = 2, min = 2),
                     numericInput("dbscan_eps", label = "Epsilon", value = 0.1),
                     helpText("DBSCAN uses the post correlation analysis data!")
                     
    ),
    
    conditionalPanel("input.tabs == 'parallel_plot'",
                     h4("User Input:"),
                     # Input: Select the dataset to work with
                     radioButtons("dataset2", "Dataset",
                                  choices = c(Correlated = "pre_corr_analysis",
                                              Uncorrelated = "post_corr_analysis"),
                                  selected = "post_corr_analysis"),
                     radioButtons("brush_mode", "Sellection on:",
                                  choices = c(Axis = "brush_axis",
                                              Plot = "brush_plot"),
                                  selected = "brush_axis"),
                     # Select variables to display
                     uiOutput("colour_select")
                    
    ) # end of conditionalPanel section
) # end of sidebar


# BODY ----
body <- dashboardBody(
    tabItems(
        tabItem(tabName = "table",
            fluidRow(
                box(width = 12, title = "Data Overview", status = "primary",solidHeader = TRUE, collapsible = TRUE,
                    DT::dataTableOutput("table1")
                )
            ),
            fluidRow(
                box(width = 12, title = "Data Summary", status = "primary",solidHeader = TRUE, collapsible = TRUE,
                    verbatimTextOutput("summary1")
                )
            ),
        ),
        
        tabItem(tabName = "correlation",
            fluidRow(
                tabBox(title = "Cross-correlation: Pre", width = 6, id = "tabset1", height = 700,
                    tabPanel("Dynamic",
                        plotlyOutput("heatmap_pre")
                    ),
                    tabPanel("Static",
                        plotOutput("matrix_pre")
                    )
                ),
                    
                tabBox(title = "Cross-correlation: Post", width = 6, id = "tabset2", height = 700,
                    tabPanel("Dynamic",
                        plotlyOutput("heatmap_post")
                    ),
                    tabPanel("Static",
                        plotOutput("matrix_post")
                    )
                )
            ),
            fluidRow(
                box(width = 6, title = "Pair scatterplot", status = "primary", height = 400, solidHeader = TRUE, collapsible = TRUE,
                    plotlyOutput("scatterplot", height = 305),
                    verbatimTextOutput("selection")
                ),
                    
                box(width = 6, title = "Messages", status = "primary", height = 400, solidHeader = TRUE, collapsible = TRUE,
                    verbatimTextOutput("msg")
                )
            )
        ),
        
        tabItem(tabName = "similarities_hc",
            fluidRow(
                box(width = 8, title = "Hierarchical Clustering Dendrogram", status = "primary",height = 600, solidHeader = TRUE, collapsible = TRUE,
                    plotOutput("plot_dendrogram")
                ),
                    
                tabBox(title = "Guide", width = 4, id = "tabset3", height = 600,
                    tabPanel("Number of Clusters",
                        plotOutput("plot_cluster_number")
                    ),
                    tabPanel("Number of PC",
                        plotOutput("scree_plot")
                    )
                )
                    
            ),
            fluidRow(
                box(width = 8, title = "Clusters Scatter Plot", status = "primary",height = 600, solidHeader = TRUE, collapsible = TRUE,
                    plotOutput("plot_scatter")
                ),
                    
                box(width = 4, title = "Goodness Of Clustering", status = "primary",height = 600, solidHeader = TRUE, collapsible = TRUE,
                    plotOutput("plot_silhouette")
                )
            )
        ),
        
        tabItem(tabName = "similarities_dbscan",
            fluidRow(
                box(width = 8, title = "DBSCAN", status = "primary",height = 700, solidHeader = TRUE, collapsible = TRUE,
                    plotOutput("plot_dbscan", height = 600)
                ),
                    
                box(width = 4, title = "Info DBSCAN", status = "primary",height = 700, solidHeader = TRUE, collapsible = TRUE,
                    plotOutput("plot_knn_distance"),
                    verbatimTextOutput("msg_dbscan")  
                )
            )
        ),
        
        tabItem(tabName = "parallel_plot",
            fluidRow(
                box(width = 12, title = "Parallel Coordinate Plot", status = "primary",height = 800, solidHeader = TRUE, collapsible = TRUE,
                    parcoordsOutput("plot_parallel", height = 700)
                )
            )
        ),
        
        tabItem(tabName = "about")
    ) # end of tabItems
) # end of BODY


# UI ----
ui <- dashboardPage(header, sidebar, body)


# SERVER ----
server <- function(input, output, session) { 
    
# Process the data file ----
{
    # Read the data file
    df <- reactive({
        req(input$file)
        read.csv(input$file$datapath,
                 header = TRUE,
                 sep = input$sep)
    })
    
    # Set the row labels to variable 1 e.g. batch number
    # Remove all non-numeric variables
    dfl <- reactive({                    
        data <- df()
        row.names(data) <- df()[,1]
        data.numeric <- data[,sapply(data, is.numeric)]
        data.numeric
    })
}
    
# Select variables and rows to analyse ----
{
    # Dynamically generate UI input when data is uploaded
    output$var_select <- renderUI({
        pickerInput(inputId = "select_var", label = "List of Variables", choices = names(dfl()), selected = names(dfl()), multiple = TRUE)
    })
    
    # Select variables to work with
    df_sel <- reactive({
        req(input$select_var)
        df_sel <- dfl() %>% select(input$select_var)
    })
    
    # Select observations (rows) to work with
    df_sel_row <- reactive({
        s = input$table1_rows_selected
        if (length(s)) {
            df_sel_row <- df_sel()[-c(s),]
        } else {
            df_sel_row <- df_sel()
        }

    })
}
    
# Data Table and Summary ----
    # Show data in a table
    output$table1 <- DT::renderDataTable({
        datatable(df_sel(),
            filter = 'top', 
            class="cell-border stripe", 
            rownames = TRUE,
            extensions = c('Buttons'),
            options = list(dom = "Blfrtip", scrollX = TRUE,
                           buttons = list("copy", list(extend = "collection",buttons = c("csv", "excel", "pdf"), text = "Save")), # end of buttons customization
                           pageLength=8, lengthChange = TRUE, autoWidth = TRUE, 
                           searchHighlight = TRUE) # end of options
        ) # end of datatable
    })

    # Print summary
    observe({
        output$summary1 <- renderPrint({
            describe(df_sel_row())
        })
    })

    
# Cross-correlation analysis ----
    # Compute the correlation matrix pre (i.e. of the initial data)
    correlation_pre <- reactive({
        correlation_pre <- round(cor(df_sel_row()), 3)
    })
    
    # Get the names of the variables
    nms_pre <- reactive({
        nms_pre <- names(df_sel_row())
    })
    
    output$heatmap_pre <- renderPlotly({
        plot_ly(x = nms_pre(), y = nms_pre(), z = correlation_pre(), width = 600, height = 600,
                key = correlation_pre(), type = "heatmap", source = "heatplot") %>%
                layout(xaxis = list(title = ""), 
                       yaxis = list(title = ""))
    })
    
    output$selection <- renderText({
        s <- event_data("plotly_click")
        if (length(s) == 0) {
            "Click on a cell in the heatmap to display a scatterplot"
        } else {
            cat("You selected: \n\n")
            as.list(s)
        }
    })
    
    output$scatterplot <- renderPlotly({
        s <- event_data("plotly_click", source = "heatplot")
        if (length(s)) {
            vars <- c(s[["x"]], s[["y"]])
            d <- setNames(df_sel_row()[vars], c("x", "y"))
            yhat <- fitted(lm(y ~ x, data = d))
            plot_ly(d, x = ~x, width = 600, height = 300) %>%
                    add_markers(y = ~y) %>%
                    add_lines(y = ~yhat) %>%
                    layout(xaxis = list(title = s[["x"]]), 
                           yaxis = list(title = s[["y"]]), 
                           showlegend = FALSE)
        } else {
            plotly_empty()
        }
    })
    
    # Cross-correlation of post-correlation analysis dataset
    
    # Filter based on the correlation treshold, return the column numbers recommended for removal
    highCorr <- reactive({
        highCorr <- findCorrelation(correlation_pre(), cutoff = input$slider_treshold)
    })
    
    df_sel_nocorr <- reactive({
        df_sel_nocorr <- df_sel_row()[,-highCorr()]
    })
    
    output$heatmap_post <- renderPlotly({
        # Compute the correlation matrix post removal of high correlations
        correlation_post <- round(cor(df_sel_nocorr()), 3) 
        nms_post <- names(df_sel_nocorr())
       
        
        plot_ly(x = nms_post, y = nms_post, z = correlation_post, width = 600, height = 600,
                key = correlation_post, type = "heatmap", source = "heatplot") %>%
                layout(xaxis = list(title = ""), 
                       yaxis = list(title = ""))
    })
    
    output$msg <- renderPrint({
        cat("Number of removed variables:\n")
        print(length(highCorr()))
        cat("Variables positions in dataset:\n")
        print(sort(highCorr()))
        cat("Variables names:\n")
        print(colnames(df_sel_row()[sort(highCorr())]))
    })
    
    # Static cross-correlation matrices
    output$matrix_pre <- renderPlot({
        # This static pair plot can be controlled by the textbox 'var_subset'
        if (input$var_subset == "") {
            pairs.panels(df_sel_row(), 
                         method = "pearson", # correlation method
                         hist.col = "#FCF928",
                         density = TRUE,  # show density plots
                         ellipses = FALSE, # show correlation ellipses
                         stars = FALSE # show the significance of correlations
            ) 
        } else {
        col_sel <- paste("c(", input$var_subset, ")", sep = "")
        pairs.panels(df_sel_row()[,rlang::eval_bare(rlang::parse_expr(col_sel))], 
                     method = "pearson", # correlation method
                     hist.col = "#FCF928",
                     density = TRUE,  # show density plots
                     ellipses = FALSE, # show correlation ellipses
                     stars = FALSE # show the significance of correlations
        )
        }
    })
    
    output$matrix_post <- renderPlot({
        pairs.panels(df_sel_nocorr(), 
                     method = "pearson", # correlation method
                     hist.col = "#FCF928",
                     density = TRUE,  # show density plots
                     ellipses = FALSE, # show correlation ellipses
                     stars = FALSE # show the significance of correlations
        )
    })
    
# Hierarchical Clustering ----

    # Dynamically generate UI slider to select number of Principal Components
    output$pc_number_select <- renderUI({
        if (input$pca) { # show slider bar to choose no. principal components
            numericInput("number_pc", "Select No. of PCs", min = 2, value = 2, step = 1)
        }
    })
    
    res.hc <- reactive({
        if (input$pca) {
        # Compute hierarchical clustering with PCA
            if (input$dataset == "pre_corr_analysis") {
                res.pca <- PCA(df_sel_row(), ncp =  input$number_pc, graph = FALSE)                     
                ind <- get_pca_ind(res.pca)
                df_pc <- ind$coord
                hcut(df_pc, k = input$vis_value, hc_func = "hclust", 
                     hc_method = input$pick_agg_method, hc_metric = input$pick_distance, stand = TRUE)
            } else {
                res.pca <- PCA(df_sel_nocorr(), ncp =  input$number_pc, graph = FALSE)                  
                ind <- get_pca_ind(res.pca)
                df_pc <- ind$coord
                hcut(df_pc, k = input$vis_value, hc_func = "hclust", 
                     hc_method = input$pick_agg_method, hc_metric = input$pick_distance, stand = TRUE)
            } # end if
        } else {
        # Compute hierarchical clustering without PCA
           if (input$dataset == "pre_corr_analysis") {
               hcut(df_sel_row(), k = input$vis_value, hc_func = "hclust", 
               hc_method = input$pick_agg_method, hc_metric = input$pick_distance, stand = TRUE)
           } else {
               hcut(df_sel_nocorr(), k = input$vis_value, hc_func = "hclust", 
               hc_method = input$pick_agg_method, hc_metric = input$pick_distance, stand = TRUE)
           } # end if
        } # end if
    })
    
    # Plot hierarchical clustering results
    output$plot_dendrogram <- renderPlot({
            fviz_dend(res.hc(), rect = TRUE, cex = 0.5,k_colors = "lancet", graph = TRUE, rect_border = "lancet", 
                      rect_fill = TRUE, ggtheme = theme_void(), main = "")
    })
    
    # Plot the guide of number of clusters
    output$plot_cluster_number <- renderPlot({
        if (input$pca) {
            # Hierarchical clustering with PCA
            if (input$dataset == "pre_corr_analysis") {
                res.pca <- PCA(df_sel_row(), ncp =  input$number_pc, graph = FALSE)
                ind <- get_pca_ind(res.pca)
                df_pc <- ind$coord
                k <- NbClust(df_pc, distance = input$pick_distance, min.nc = 2, max.nc = 9, method = input$pick_agg_method, index = "all")
                fviz_nbclust(k, barfill = "#00468BFF")
            } else {
                res.pca <- PCA(df_sel_nocorr(), ncp =  input$number_pc, graph = FALSE)
                ind <- get_pca_ind(res.pca)
                df_pc <- ind$coord
                k <- NbClust(df_pc, distance = input$pick_distance, min.nc = 2, max.nc = 9, method = input$pick_agg_method, index = "all")
                fviz_nbclust(k, barfill = "#00468BFF")
            } # end if
        } else {
            # Hierarchical clustering without PCA
            if (input$dataset == "pre_corr_analysis") {
                k <- NbClust(df_sel_row(), distance = input$pick_distance, min.nc = 2, max.nc = 9, method = input$pick_agg_method, index = "all")
                fviz_nbclust(k, barfill = "#00468BFF")
            } else {
                k <- NbClust(df_sel_nocorr(), distance = input$pick_distance, min.nc = 2, max.nc = 9, method = input$pick_agg_method, index = "all")
                fviz_nbclust(k, barfill = "#00468BFF")  
            } # end if
        } # end if
    })
    
    output$plot_silhouette <- renderPlot({
        fviz_silhouette(res.hc(), ggtheme = theme_light()) + scale_fill_lancet()+ scale_color_lancet() 
    })
    
    output$plot_scatter <- renderPlot({
        # Uses PCA to visualize the clusters on a 2D plane
        fviz_cluster(res.hc(), repel = TRUE, ggtheme = theme_minimal()) + scale_fill_lancet()+ scale_color_lancet()
    })
    
    output$scree_plot <- renderPlot({
        # Visualize scree plot
        if (input$dataset == "pre_corr_analysis") {
            res.pca <- PCA(df_sel_row(), ncp =  ncol(df_sel_row()), graph = FALSE)
            fviz_eig(res.pca, addlabels=TRUE, hjust = -0.3, barfill = "#00468BFF", linecolor ="red") +
                labs(title = "", x = "Principal Components", y = "% of variances") +
                theme_light()
        } else {
            res.pca <- PCA(df_sel_nocorr(), ncp =  ncol(df_sel_nocorr()), graph = FALSE)
            fviz_eig(res.pca, addlabels=TRUE, hjust = -0.3, barfill = "#00468BFF", linecolor ="red") +
                labs(title = "", x = "Principal Components", y = "% of variances") +
                theme_light()
        } # end if
        
    })
    

# DBSCAN Clustering ----
    df_sel_nocorr_mat <- reactive({
        df_sel_nocorr_mat <- as.matrix(df_sel_nocorr())
    })
    
    output$plot_knn_distance <- renderPlot({
        dbscan::kNNdistplot(df_sel_nocorr_mat(), k = input$dbscan_MinPts)
    })
    
    output$plot_dbscan <- renderPlot({
        res.dbscan <- dbscan::dbscan(df_sel_nocorr_mat(), minPts = input$dbscan_MinPts, eps = input$dbscan_eps)
        fviz_cluster(res.dbscan, df_sel_nocorr_mat(), geom = c("point", "text"), repel = TRUE, ggtheme = theme_minimal()) +
            scale_fill_lancet()+ scale_color_lancet()   
    })
    
    output$msg_dbscan <- renderPrint({
       res.dbscan <- dbscan::dbscan(df_sel_nocorr_mat(), minPts = input$dbscan_MinPts, eps = input$dbscan_eps)
       res.dbscan
    })

    
# Parallel coordinates plot ----

    # Dynamically generate UI input to select variable by which to colour the PCP
    output$colour_select <- renderUI({
        pickerInput(inputId = "pcp_colour", label = "Colour by:", choices = names(dfl()), selected = NULL, multiple = FALSE) 
    })
    
    output$plot_parallel<- renderParcoords({
    # Set the brush mode        
    if (input$brush_mode == "brush_axis") {
        brush <- "1d-axes-multy"
    } else {
        brush <- "2D-strums"
    }
        
    if (input$dataset2 == "pre_corr_analysis") {
        parcoords(df_sel_row(), height = 600, rownames = TRUE, brushMode = brush, reorderable = TRUE,
                     alphaOnBrushed = 0.15,
                     color = list(
                         colorBy = input$pcp_colour,
                         colorScale = "scaleOrdinal",
                         colorScheme = "schemeCategory10"
                     ),
                     withD3 = TRUE)
    } else {
        parcoords(df_sel_nocorr(), height = 600, rownames = TRUE, brushMode = brush, reorderable = TRUE,
                     alphaOnBrushed = 0.15,
                     color = list(
                         colorBy = input$pcp_colour,
                         colorScale = "scaleOrdinal",
                         colorScheme = "schemeCategory10"
                     ),
                     withD3 = TRUE)
    } # end if
    }) # end renderParcoords

    
} # end of server

shinyApp(ui, server)
