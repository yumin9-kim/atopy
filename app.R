options(shiny.sanitize.errors = F)
source("global2.R")
library(shinycustomloader);library(survival);library(MatchIt);library(survey);library(ggplot2)
library(shinymanager);library(jskm);library(DT);library(jsmodule);library(forestplot);library(tsibble);library(prophet)
nfactor.limit <- 21

#setwd("/home/js/ShinyApps/Sev-cardio/MS-registry/Tx_of_moderately_severeMS")

#credentials <- data.frame(
#  user = c("admin", "Sev-cardio"),
#  password = c("zarathuadmin", "Sev-cardio"),
#  admin = c(T, F),
#  stringsAsFactors = FALSE
#)

#create_db(credentials_data = credentials, sqlite_path = "database.sqlite")

conti_vars <- setdiff(names(out), factor_vars)



ui <- navbarPage("Atopy",
                 tabPanel("Table 1: Baseline", icon = icon("percentage"),
                          sidebarLayout(
                            sidebarPanel(
                              tb1moduleUI("tb1"),
                            ),
                            mainPanel(
                              withLoader(DTOutput("table1"), type="html", loader="loader6"),
                              wellPanel(
                                h5("Normal continuous variables  are summarized with Mean (SD) and t-test(2 groups) or ANOVA(> 2 groups)"),
                                h5("Non-normal continuous variables are summarized with median [IQR or min,max] and kruskal-wallis test"),
                                h5("Categorical variables  are summarized with table")
                              )
                            )
                          )
                          
                 ),
                 navbarMenu("Population estimation: GEE", icon = icon("list-alt"),
                            tabPanel("Linear",
                                     sidebarLayout(
                                       sidebarPanel(
                                         GEEModuleUI2("linear")
                                       ),
                                       mainPanel(
                                         withLoader(DTOutput("lineartable"), type="html", loader="loader6")
                                       )
                                     )
                            ),
                            tabPanel("Binomial",
                                     sidebarLayout(
                                       sidebarPanel(
                                         GEEModuleUI2("logistic")
                                       ),
                                       mainPanel(
                                         withLoader(DTOutput("logistictable"), type="html", loader="loader6")
                                       )
                                     )
                            )
                 ),
                 tabPanel("Individual prediction",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("ID_pred", "ID",  unique(zz_fg$ID), unique(zz_fg$ID)[1]),
                              selectInput("Dep_pred", "Dependent variable", varlist$Symptom, "sum_score"),
                              selectInput("Indep_pred", "Independent variable", varlist[3:4], setdiff(vars.pred, c("season", "dow")), multiple = T),
                              radioButtons("Method_pred", "Method", c("ARIMA", "Prophet"), "ARIMA", inline = T),
                              h4("Only for prophet model"),
                              textInput("changepoint", h5("Changepoint prior scale: (0.001 ~ 0.5)"), 0.05),
                              textInput("seasonality", h5("Seasonality prior scale (0.01 ~ 10)"), 10),
                              actionButton("action_pred", "Run prediction")
                            ),
                            mainPanel(
                              withLoader(plotOutput("predplot", width = "100%"), type="html", loader="loader6"),
                              h3("Download options"),
                              wellPanel(
                                uiOutput("downloadControls_kap"),
                                downloadButton("downloadButton_kap", label = "Download the plot")
                              ),
                              tabsetPanel(type = "pills",
                                          tabPanel("Estimation", 
                                                   withLoader(DTOutput("predest", width = "100%"), type="html", loader="loader6")
                                          ),
                                          tabPanel("Fitted value", 
                                                   withLoader(DTOutput("predtable", width = "100%"), type="html", loader="loader6")
                                          ),
                                          tabPanel("Likelihood", 
                                                   verbatimTextOutput("predres")),
                                          tabPanel("Prophet components plot", 
                                                   withLoader(plotOutput("prophetcomp", width = "100%"), type="html", loader="loader6"),
                                          )
                              )
                            )
                            
                            
                            
                          )
                          
                 )
                 
)



server <- function(input, output, session) {
  
  # check_credentials returns a function to authenticate users
  
  out_tb1 <- callModule(tb1module2, "tb1", data = reactive(out[, .SD[1], keyby = "ID", .SDcols = varlist$Base]), data_label = reactive(out.label), data_varStruct = NULL, nfactor.limit = nfactor.limit, showAllLevels = F)
  
  output$table1 <- renderDT({
    tb <- out_tb1()$table
    cap <- out_tb1()$caption
    out.tb1 <- datatable(tb, rownames = T, extensions = "Buttons", caption = cap,
                         options = c(jstable::opt.tb1("tb1"),
                                     list(columnDefs = list(list(visible=FALSE, targets= which(colnames(tb) %in% c("test","sig"))))
                                     ),
                                     list(scrollX = TRUE)
                         )
    )
    if ("sig" %in% colnames(tb)){
      out.tb1 = out.tb1 %>% formatStyle("sig", target = 'row' ,backgroundColor = styleEqual("**", 'yellow'))
    }
    return(out.tb1)
  })
  
  
  out_linear <- callModule(GEEModuleLinear2, "linear", data = reactive(out), data_label = reactive(out.label), data_varStruct = reactive(varlist), nfactor.limit = nfactor.limit, id.gee = reactive("ID"))
  
  output$lineartable <- renderDT({
    hide = which(colnames(out_linear()$table) == "sig")
    datatable(out_linear()$table, rownames=T, extensions = "Buttons", caption = out_linear()$caption,
              options = c(jstable::opt.tbreg(out_linear()$caption),
                          list(columnDefs = list(list(visible=FALSE, targets =hide))
                          ),
                          list(scrollX = TRUE)
              )
    ) %>% formatStyle("sig", target = 'row',backgroundColor = styleEqual("**", 'yellow'))
  })
  
  out_logistic <- callModule(GEEModuleLogistic2, "logistic", data = reactive(out), data_label = reactive(out.label), data_varStruct = reactive(varlist), nfactor.limit = nfactor.limit, id.gee = reactive("ID"))
  
  output$logistictable <- renderDT({
    hide = which(colnames(out_logistic()$table) == "sig")
    datatable(out_logistic()$table, rownames=T, extensions = "Buttons", caption = out_logistic()$caption,
              options = c(jstable::opt.tbreg(out_logistic()$caption),
                          list(columnDefs = list(list(visible=FALSE, targets =hide))
                          ),
                          list(scrollX = TRUE)
              )
    ) %>% formatStyle("sig", target = 'row',backgroundColor = styleEqual("**", 'yellow'))
  })
  
  
  obj.ind <- eventReactive(input$action_pred, {
    data <- dplyr::filter(zz_fg, ID == input$ID_pred)
    if (input$Method_pred == "ARIMA"){
      fit <- data %>% 
        model(ARIMA(as.formula(paste0(input$Dep_pred, " ~ ", paste(input$Indep_pred, collapse = "+")))))
    }
    
    if (input$Method_pred == "Prophet"){
      data <- dplyr::filter(tibble(zz), ID == input$ID_pred) %>%
        dplyr::mutate(ds=date, y=sum_score, cap = 24, floor = 0)
      
      vars.pred <- setdiff(input$Indep_pred, c("season", "dow"))
      data2 <- data %>% dplyr::select(c(date, ds, y, input$Dep_pred, vars.pred)) %>% na.omit()
      
      mod <- prophet(yearly.seasonality = T, changepoint.prior.scale = as.numeric(input$changepoint), seasonality.prior.scale = as.numeric(input$seasonality))
      for (i in vars.pred){
        mod <- mod %>% add_regressor(i)
      }
      fit <- mod %>% fit.prophet(data2)
    }
    
    return(fit)
  })
  
  obj.predtable <- reactive({
    if (input$Method_pred == "ARIMA"){
      fit <- obj.ind()
      
      res.ARIMA <- data.table(dplyr::bind_cols(fitted(fit), sum_score = lapply(1:nrow(fit), function(x){
        fit[, 2][[1]][[1]]$data$sum_score
      }) %>% Reduce(c, .)))
      
      table <- datatable(res.ARIMA[, -c(1, 2)], rownames=F, extensions = "Buttons", caption = "Individual data",
                         options = c(jstable::opt.data(input$ID_pred),
                                     
                                     list(scrollX = TRUE)
                         )
      )}
    
    else if (input$Method_pred == "Prophet"){
      mod <- obj.ind()
      pred <- predict(mod)
      tab <- data.table(dplyr::bind_cols(date = ymd(pred$ds), predict = pred$yhat, sum_score = mod$history$sum_score))
      tab.prophet <- tab %>% merge(data.table(dplyr::filter(tibble(zz), ID == input$ID_pred))[, .(date)], by = "date", all.y = T) %>% .[order(date)] %>% 
        as_tsibble() %>% fill_gaps() %>% as.data.table()
      RMSE <- tab %>% summarize(rmse = mean((predict-sum_score)^2)) %>% round(., 3)
      table <- datatable(tab.prophet, rownames=F, extensions = "Buttons", caption = paste0("RMSE = ", RMSE),
                         options = c(jstable::opt.data(input$ID_pred),
                                     
                                     list(scrollX = TRUE)
                         )
    )}
    return(table)
  })
  
  output$predtable <- renderDT({
    obj.predtable()
  })
  
  output$predest <- renderDT({
    if (input$Method_pred == "ARIMA"){
      fit <- obj.ind()
      datatable(fit[, 2][[1]][[1]]$fit$par, rownames = F, extensions = "Buttons", caption = "Result",
                options = c(jstable::opt.tbreg(input$ID_pred),
                            
                            list(scrollX = TRUE)
                )
      ) %>% formatRound(columns=2:5, digits=3)
    } else{
      
    }
  })
  
  output$predres <- renderPrint({
    if (input$Method_pred == "ARIMA"){
      obj.ind()[, 2][[1]][[1]]$fit$fit
    }
  })
  
  output$prophetcomp <- renderPlot({
    if (input$Method_pred == "Prophet"){
      mod <- obj.ind()
      pred <- predict(mod)
      prophet_plot_components(mod, pred)}
  })
  
  obj.predplot <- reactive({
    if (input$Method_pred == "ARIMA"){
      fit <- obj.ind()
      
      res.ARIMA <- data.table(dplyr::bind_cols(fitted(fit), sum_score = lapply(1:nrow(fit), function(x){
        fit[, 2][[1]][[1]]$data$sum_score
      }) %>% Reduce(c, .))) %>% melt(idvars = "ID", measure.vars = c(".fitted", "sum_score"))
      
      
      ggpubr::ggline(res.ARIMA[ID == input$ID_pred], "date", "value", color = "variable", plot_type  = "l", xlab = "", ylab = "sum_score")
    } else if (input$Method_pred == "Prophet"){
      mod <- obj.ind()
      pred <- predict(mod)
      
      tab.prophet <- data.table(dplyr::bind_cols(date = as.Date(pred$ds), predict = pred$yhat, sum_score = mod$history$sum_score)) %>% 
        merge(data.table(dplyr::filter(tibble(zz), ID == input$ID_pred))[, .(date)], by = "date", all.y = T) %>% 
        as_tsibble() %>% fill_gaps() %>% as.data.table() %>%
        melt(measure.vars = c("predict", "sum_score"))
      
      ggpubr::ggline(tab.prophet, "date", "value", color = "variable", plot_type  = "l", xlab = "", ylab = "sum_score")
    }
    
    
  })
  
  output$downloadControls_kap <- renderUI({
    fluidRow(
      column(4,
             selectizeInput("kap_file_ext", "File extension (dpi = 300)", 
                            choices = c("jpg","pdf", "tiff", "svg", "pptx"), multiple = F, 
                            selected = "pptx"
             )
      ),
      column(4,
             sliderInput("fig_width_kap", "Width (in):",
                         min = 5, max = 20, value = 10
             )
      ),
      column(4,
             sliderInput("fig_height_kap", "Height (in):",
                         min = 5, max = 20, value = 6
             )
      )
    )
  })
  
  output$predplot <- renderPlot({
    obj.predplot()
    
  })
  
  output$downloadButton_kap <- downloadHandler(
    filename =  function() {
      paste(input$ID_pred, "_", input$Method_pred , "_plot.", input$kap_file_ext ,sep="")
    },
    # content is a function with argument file. content writes the plot to the device
    content = function(file) {
      withProgress(message = 'Download in progress',
                   detail = 'This may take a while...', value = 0, {
                     for (i in 1:15) {
                       incProgress(1/15)
                       Sys.sleep(0.01)
                     }
                     
                     if (input$kap_file_ext == "pptx"){
                       my_vec_graph <- rvg::dml(ggobj  = obj.predplot())
                       doc <- officer::read_pptx()
                       doc <- officer::add_slide(doc, layout = "Title and Content", master = "Office Theme")
                       doc <- officer::ph_with(doc, my_vec_graph, location = officer::ph_location(width = input$fig_width_kap, height = input$fig_height_kap))
                       print(doc, target = file)
                       
                     } else{
                       ggsave(file, obj.km(), dpi = 300, units = "in", width = input$fig_width_kap, height =input$fig_height_kap)
                     }
                     
                   })
      
      
    })
  
  
  
  session$onSessionEnded(function() {
    session$close()
  })
  
  
  
  
}



shinyApp(ui, server)