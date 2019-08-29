library(shiny)
library(fbAntares)
library(DT)
library(data.table)
library(rAmCharts)
library(manipulateWidget)


giveTableError <- function(dta){
  
  dtaerror <- dta[, .SD, .SDcols = c("Period", "idDayType", "error1", "error2")]
  setnames(dtaerror, old = c("error1", "error2"), new = c("Inf_error", "Sup_error"))
  dtaerror[, c("Inf_error", "Sup_error") := list(round(Inf_error, 3), round(Sup_error, 3))]
  dtaerror
}

get_plot_output_list <- function(dta, input_n, ylim, xlim, combi) {
  # Insert plot output objects the list
  
  plot_output_list <- lapply(input_n, function(i) {
    plotname <- paste("plot1", i, sep="")
    plot_output_object <- combineWidgetsOutput(plotname)
    plot_output_object <- renderCombineWidgets({
      combineWidgets(
        list = lapply(1:nrow(combi), function(X) {
          fbAntares:::graphFlowBased2D(flowbased = dta, ctry1 = combi[X, 1], ctry2 = combi[X, 2], 
                           hour = dta$Period[i], dayType = dta$idDayType[i], 
                           xlim = xlim, ylim = ylim)
        })
      )
    })
    attributes(plot_output_object)$outputFunc <- function (
      outputId, width = "100%", height = "1000px")
    {
      htmlwidgets::shinyWidgetOutput(outputId, "combineWidgets",
                                     width, height, package = "manipulateWidget")
    }
    # print(attributes(plot_output_object)$outputFunc)
    
    plot_output_object
  })
  
  do.call(tagList, plot_output_list) # needed to display properly.
  
  return(plot_output_list)
}
