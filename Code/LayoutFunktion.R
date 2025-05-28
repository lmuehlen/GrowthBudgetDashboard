


plotlyLayoutDZ<-function(p,
                         title="Test",
                         subtitle="123",
                         includeLegend=FALSE,
                         ylim=c(NULL,NULL),
                         xlim=c(NULL,NULL),
                         width=577.5,
                         height=450,
                         tooltip="text",
                         hovermode="x unified",
                         hoverdistance=5,
                         tooltip_bgcolor="white",
                         tooltip_bordercolor="#ee6174",
                         xlabel=NULL,
                         ylabel=NULL,
                         title_size=18,
                         subtitle_size=16,
                         axis_size=14,
                         tooltip_size=16,
                         margin_top=50,
                         hoverformat=NULL,
                         toImageButton=FALSE,
                         itemclick="toggle"){



  if (length(ylim)==2){
    ylim <- ylim
  } else {

    ylim <- c(NULL, NULL)
  }

  if (length(xlim)==2){
    xlim <- xlim
  } else {

    xlim <- c(NULL, NULL)
  }

  if (includeLegend){
    margin_top <- margin_top+30

  }

  PlotLayoutDZ<-
    p%>%
    layout(
    hovermode=hovermode,
    hoverdistance=hoverdistance,
    plot_bgcolor  = "rgba(0,0,0,0)",   # inside the axes
    paper_bgcolor = "rgba(0,0,0,0)",    # the margin / outer “paper”
    ##Title & Subtitle####
    title = list(
      text = paste0(
        "<span style='font-size:",title_size,",px;'><b>",title,"</b></span>",
        "<br><span style='font-size",subtitle_size,":12px;'>",subtitle,"</span>"
      ),
      x = 0.0,                     # left-align title block (optional)
      xanchor = "left",
      y=0.96,
      yanchor ="top",
      font=list(color="#181c44")#color="#181c44"
    ),
    #necessary to leave enough space
    margin = list(
      t = margin_top,    # ↑ top  margin, px  ‒ increase until the title block clears
      b =  0,    # ↓ bottom margin, px ‒ more room for axis title / footnotes
      l =  0,    # optional: tweak left / right as well
      r =  0
    ),

    ##Axis####
    #xaxis
    # remove grid lines
    xaxis = list(
      hoverformat=hoverformat,
      title    = list(
        text=xlabel,
        font=list(size = axis_size, family = "Open Sans")
        ),
      tickfont=list(size = axis_size, family = "Open Sans",color="#181c44"),
      #make ticks transparent
      tickcolor = "rgba(0,0,0,0)",
      ##remove grid lines
      showgrid = FALSE,
      #remove white halo
      spikethickness = -2,
      range = c(xlim[1], xlim[2])
    ),
    ##
    #yaxis
    yaxis = list(
      title    = list(
        text=ylabel,
        font=list(size = axis_size, family = "Open Sans")
      ),
      tickfont=list(size = axis_size, family = "Open Sans",color="#181c44"),
      showgrid = TRUE,
      gridwidth = 2,
      griddash  = "2px",
     # gridcolor = "#181c44",
      ticks = "outside",   # draw ticks to the left of the axis line
      ticklen= 8,
      tickcolor          = "rgba(0,0,0,0)",#transparent
    ticklabelposition  = "outside",  # (explicit; default since Plotly 2.15)
    # set limit of yaxis to a and b
      range = c(ylim[1], ylim[2]),      # (optional)
    zeroline = FALSE # remove the zero line
    ),

    ##Tooltip (hover)####

    hoverlabel = list(align = "left",
                      font  = list(size = tooltip_size, family = "Open Sans",color="#181c44"),
                      bgcolor     = tooltip_bgcolor,
                      bordercolor = tooltip_bordercolor
    ),

   # Legend####
    ## Change the legend position to be on top of the plot
    legend =  list(
      orientation = "h",         # horizontal
      yanchor     = "bottom",   # anchor to the bottom of the legend
      y           = 1,        # position at the top of the plot
      xanchor     = "center",     # anchor to the left of the legend
      x           = 0.5,        # position at the left of the plot
      font        = list(size = axis_size, family = "Open Sans",color="#181c44"),
      bgcolor     = "rgba(255,255,255,0)", #transparent
      bordercolor = "rgba(0,0,0,0)",#transparent
      borderwidth = 0,
      itemclick  = itemclick,
      itemdoubleclick  = FALSE
    ),
    #conditionally keep Legend
    showlegend = includeLegend,

    #Turn dragmode of
    dragmode=FALSE
  )%>%
    config(
            displayModeBar = FALSE,    # <<< remove the top-right bar entirely
            scrollZoom     = FALSE,    # turn off wheel zoom
            doubleClick    = FALSE,    # ignore double-click to autoscale
            editable       = FALSE,     # forbid any on-plot editing
            locale = "de"
          )

  # if(toImageButton){
  # PlotLayoutDZ<-PlotLayoutDZ%>%
  #   #Rest config
  #   config(
  #
  #     displayModeBar = "hover",      # or TRUE for always-visible
  #     displaylogo    = FALSE,        # hide the Plotly logo
  #
  #     ## keep ONE button and drop the others --------------------
  #     modeBarButtonsToRemove = list( # everything *except* "toImage"
  #       "zoom2d","pan2d","select2d","lasso2d",
  #       "zoomIn2d","zoomOut2d","autoScale2d","resetScale2d",
  #       "hoverClosestCartesian","hoverCompareCartesian",
  #       "toggleSpikelines","toggleHover"       # add more if you see them
  #     ),
  #
  #     ## fine-tune the download camera --------------------------
  #     toImageButtonOptions = list(
  #       format   = "png",                    # only PNG
  #       filename = gsub("\\s+", "_", title), # use your plot title
  #       width    = width,                    # inherit from function arg
  #       height   = height,
  #       scale    = 2                         # 2× DPI
  #     ),
  #
  #    # displayModeBar = FALSE,    # <<< remove the top-right bar entirely
  #     scrollZoom     = FALSE,    # turn off wheel zoom
  #     doubleClick    = FALSE,    # ignore double-click to autoscale
  #     editable       = FALSE,     # forbid any on-plot editing
  #     locale = "de"
  #   )
  # } else {
  #   PlotLayoutDZ<-PlotLayoutDZ%>%
  #     #Rest config
  #     config(
  #       displayModeBar = FALSE,    # <<< remove the top-right bar entirely
  #       scrollZoom     = FALSE,    # turn off wheel zoom
  #       doubleClick    = FALSE,    # ignore double-click to autoscale
  #       editable       = FALSE,     # forbid any on-plot editing
  #       locale = "de"
  #     )
  # }

  newDep <- htmlDependency(name = "plotly-latest",
                           version = "2.21.1",
                           src = list(href = "https://cdn.plot.ly"),
                           script = "plotly-2.21.0.min.js")

  # add that dependency to plot
  PlotLayoutDZ$dependencies[[6]] <- newDep



  return(PlotLayoutDZ)
}

## Add 1+1##

