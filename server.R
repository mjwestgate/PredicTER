# to do:
  # add 'final number of articles' to count_dframe (new final row), plot 2
  # export data option
  # error in DEAS entries when switching between SRs and SMs - Major problem for now
  # no way to know whether you are currently looking at data for SRs or SMs

library(shiny)
library(shinydashboard)
library(plotly)
library(viridisLite)

server<-function(input, output, session){

	options(warn=-1) # hide incompatibility between shiny and plotly
	# https://github.com/hrbrmstr/metricsgraphics/issues/49

	# dynamic UI control
	# set reactive values to show whether to display critical appraisal lines & selectors
	show_ca_line <- reactiveValues(x = TRUE)# , value="65.4")
	show_data_stages <- reactiveValues(x = TRUE)
	ca_value <- list(x = "76.0")
	ca_current_status <- list(x = FALSE)

	# reactive values to store plot data and images
	plot_data <- reactiveValues(
    p1 = data.frame(x = 0),
    p2 = data.frame(x = 0)
  )

 	# set defaults for when to show selector for critical appraisal stage
 	observeEvent(
 		input$tabs, {
		if(any(input$tabs == "syst_map")){
			show_ca_line$x <- FALSE
			show_data_stages$x <- FALSE
			insertUI(
				selector = "#placeholder_selector",
				ui = tags$div(
					list(
						selectInput(
							inputId = "include_ca",
							label = "Include critical appraisal stage?",
							choices = c("No", "Yes")
						)
					),
					id = "ca_selector"
				)
			)
		}else{
			removeUI(selector = "#ca_selector")
			ca_value$x <- "76.0"
			if(ca_current_status$x){
				textInput(
          inputId = "row7_appraisal_percent",
          label = NULL,
          value = ca_value$x
        )
			}else{
				show_ca_line$x <- TRUE
			}
			show_data_stages$x <- TRUE
		}
	})

	# link reactive values to state of include_ca
 	observeEvent(input$include_ca, {
		if(input$include_ca == "Yes"){
			ca_value$x <- "27.4"
			if(ca_current_status$x){
				updateTextInput(
          session,
          inputId = "row7_appraisal_percent",
          value = ca_value$x
        )
			}else{
				show_ca_line$x<-TRUE
			}
		}else{
			show_ca_line$x<-FALSE
		}
	})

	# add or substract lines as required
	observe({
		# set options fow when critical appraisal is required (or not)
		if(show_ca_line$x){
			if(ca_current_status$x == FALSE){
			    ca_current_status$x <- TRUE
				# insert correct line
			    insertUI(
			      selector = '#placeholder_ca',
			      ui = tags$div(
    					list(
    						splitLayout(
    							HTML("Critical<br>appraisal"),
    							textInput(
                    inputId = "row7_appraisal_nperday",
                    label = NULL,
                    value = "11.7"
                  ),
    							textInput(
                    inputId = "row7_appraisal_percent",
                    label = NULL,
                    value = ca_value$x
                  ),
    							textInput(
                    inputId = "row7_appraisal_checked",
                    label = NULL,
                    value = "0"
                  ),
    							cellWidths = c("25%", "25%", "25%", "25%")
    						)
    					),
			        id = "critical_appraisal"
			      )
			    )
			}
		}else{
		 	removeUI(selector = "#critical_appraisal")
		 	ca_current_status$x <- FALSE
		}
	})

	# NOTE: if this is included in the previous 'observe', any change that triggers
	# either show_ca_line or show_data_stages triggers both; hence separate here.
	observe({
		 # ditto for data extraction and preparation (SRs only)
		 if(show_data_stages$x){
		    insertUI(
		      selector = '#placeholder_SRs',
		      ui = tags$div(
    				list(
    					splitLayout(
    						HTML("Data<br>extraction"),
    						textInput(
                  inputId = "row8_dataextract_nperday",
                  label = NULL,
                  value = "6.9"
                ),
    						textInput(
                  inputId = "row8_dataextract_percent",
                  label = NULL,
                  value = "100.0"
                ),
    						textInput(
                  inputId = "row8_dataextract_checked",
                  label = NULL,
                  value = "0"
                ),
    						cellWidths = c("25%", "25%", "25%", "25%")
    					),
    					splitLayout(
    						HTML("Data<br>preparation"),
    						textInput(
                  inputId = "row9_dataprep_nperday",
                  label = NULL,
                  value = "24"
                ),
    						textInput(
                  inputId = "row9_dataprep_percent",
                  label = NULL,
                  value = "59.2"
                ),
    						textInput(
                  inputId = "row9_dataprep_checked",
                  label = NULL,
                  value = "0"
                ),
    						cellWidths = c("25%", "25%", "25%", "25%")
    					)
    				),
		        id = "data_stages"
		      )
		    )
		 }else{
		 	removeUI(selector = '#data_stages')
		 }
	})

	output$review_type_text <- renderUI({
		if(any(input$tabs=="syst_map")){
			actionLink(
        inputId = "help_review_type",
        label = "Currently selected: Systematic Map"
      )
		}else{
			actionLink(
        inputId = "help_review_type",
        label = "Currently selected: Systematic Review"
      )
		}
	})

	# update default values depending on whether we have a systematic map or a systematic review
	# note: only those values that differ between SRs and SMs are updated
	observeEvent(input$tabs, {
		if(any(input$tabs=="syst_review")){
			updateTextInput(session,
        inputId = "n_search",
        value = "11786"
      )
			updateTextInput(session,
        inputId = "n_grey",
        value = "16"
      )
			updateTextInput(session,
        inputId = "unique_percent",
        value = "72.1"
      )
			updateTextInput(session,
        inputId = "row2_title_percent",
        value = "14.6"
      )
			updateTextInput(session,
        inputId = "row3_abstract_percent",
        value = "25.0"
      )
			updateTextInput(session,
        inputId = "row4_retrieved_percent",
        value = "150.9"
      )
			updateTextInput(session,
        inputId = "row5_fulltext_percent",
        value = "21.4"
      )
			updateTextInput(session,
        inputId = "synthesis",
        value = "15.0"
      )
		}else{ # i.e. syst_map
			updateTextInput(session,
        inputId = "n_search",
        value = "34236"
      )
			updateTextInput(session,
        inputId = "n_grey",
        value = "22"
      )
			updateTextInput(session,
        inputId = "unique_percent",
        value = "66.1"
      )
			updateTextInput(session,
        inputId = "row2_title_percent",
        value = "18.1"
      )
			updateTextInput(session,
        inputId = "row3_abstract_percent",
        value = "26.4"
      )
			updateTextInput(session,
        inputId = "row4_retrieved_percent",
        value = "110.6"
      )
			updateTextInput(session,
        inputId = "row5_fulltext_percent",
        value = "35.4"
      )
			updateTextInput(session,
        inputId = "synthesis",
        value = "0"
      )
		}
	})

	# create a data.frame to store y axis labels, to match to later data summary tables
	row_lookup <- data.frame(
		row_label = paste0("row", c(1:9)),
		plot1_stage = c(
			"Removing duplicates", "Title screening", "Abstract screening",
			"Full text retrieval", "Full text screening", "Meta-data extraction",
			"Critical appraisal", "Data extraction", "Data preparation"),
		plot2_stage = c(
			"Search results", "Titles screened", "Abstracts screened",
			"Full texts retrieved", "Full texts screened", "Metadata extracted",
			"Appraised", "Data extracted", "Data prepared"),
		stringsAsFactors = FALSE
	)

	# calculate and plot summary values
	observe({
		# look up relevant data using lapply
		x <- names(input)
		lookup_check <- grepl("^row[0-9]", x, perl=TRUE)
		lookup_names <- x[which(lookup_check)]
		lookup_dframe <- as.data.frame(
      do.call(rbind, strsplit(lookup_names, "_")),
      stringsAsFactors = FALSE
    )
		colnames(lookup_dframe) <- c("row", "stage", "column")
		lookup_dframe$value <- unlist(lapply(lookup_names,
      function(a){input[[a]]}
    ))

		# convert to data.frame
		count_dframe <- as.data.frame(
			do.call(rbind, split(lookup_dframe[, "value"], lookup_dframe$row)),
			stringsAsFactors = FALSE
    )
		colnames(count_dframe) <- c("checked", "nperday", "percent")
		initial_dframe <- data.frame(
			checked = c(0),
			nperday = c(1),
			percent = as.numeric(input$unique_percent)
    )
		count_dframe <- as.data.frame(
      rbind(initial_dframe, count_dframe),
      stringsAsFactors=FALSE
    )
		count_dframe$checked <- 1 + (as.numeric(count_dframe$checked) * 0.01)
		count_dframe$nperday <- as.numeric(count_dframe$nperday)
		count_dframe$percent <- as.numeric(count_dframe$percent) * 0.01

		# calculate number of articles, and time taken to process them
		count_dframe$cumulative_percent <- cumprod(count_dframe$percent)
		n_initial <- as.numeric(input$n_search) # + (50 * as.numeric(input$n_bib))
		n_articles <- round(count_dframe$cumulative_percent * n_initial, 0)
		count_dframe$count_pre <- c(
      round(n_initial, 0),
      n_articles[1:(length(n_articles)-1)]
    )
		count_dframe$count_post <- n_articles
		count_dframe$time_days <- (count_dframe$count_pre * count_dframe$checked) / count_dframe$nperday
		count_dframe$time_days[1] <- as.numeric(input$unique_time)

		# ensure ordering is correct
		count_dframe$row_order <- c(1:nrow(count_dframe))
		count_dframe$row_label <- paste0("row", count_dframe$row_order)
		if(show_ca_line$x == FALSE){
			count_dframe <- count_dframe[-which(count_dframe$row_label == "row7"), ]
		}
		if(show_data_stages$x == FALSE){
			exclude_rows <- which(
        grepl("^row[8-9]", count_dframe$row_label, perl = TRUE)
      )
			count_dframe <- count_dframe[-exclude_rows, ]
		}
		count_dframe <- merge(
      count_dframe,
      row_lookup,
      by = "row_label",
      all = FALSE
    )
		count_dframe <- count_dframe[order(count_dframe$row_order), ]

		# create time data.frame
		time_dframe <- data.frame(
			stage = c(
				"Administration", "Planning time", "Protocol development",
				"Searching (academic)", "Searching (grey)", "Checking bibliographies",
				"Synthesis", "Report writing", "Communication", "Meetings"),
			value = c(
				0,
				as.numeric(input$planning),
				as.numeric(input$protocol),
				as.numeric(input$n_databases) / as.numeric(input$n_db_perday),
				(as.numeric(input$n_grey) / as.numeric(input$n_grey_perday)) +
					as.numeric(input$time_grey_add),
				0,
				as.numeric(input$synthesis),
				as.numeric(input$report),
				as.numeric(input$comms),
				as.numeric(input$meetings)
			),
			stringsAsFactors = FALSE
		)

		# create a grouping dframe for arranging colors in plot
		group_lookup<-data.frame(
			stage = c(
				"Administration", "Planning time", "Protocol development",
				"Searching (academic)", "Searching (grey)", "Checking bibliographies",
				"Removing duplicates", "Title screening", "Abstract screening",
				"Full text retrieval", "Full text screening", "Meta-data extraction",
				"Critical appraisal", "Data extraction", "Data preparation",
				"Synthesis", "Report writing", "Communication", "Meetings"),
			group_order = c(
        rep(1, 3),
        rep(2, 4),
        rep(3, 4),
        rep(4, 4),
        rep(5, 4)
      )
		)

		# calculate which extra rows to add from count_dframe and insert to time_dframe
		count_dframe_small <- count_dframe[, c("plot1_stage", "time_days")]
		colnames(count_dframe_small) <- c("stage", "value")
		time_dframe <- as.data.frame(rbind(
			time_dframe[1:6,],
			count_dframe_small,
			time_dframe[7:10, ]
    ))
		time_dframe$order <- c(1:nrow(time_dframe))
		time_dframe$y <- factor(
			c(nrow(time_dframe):1),
			levels = c(1:nrow(time_dframe)),
			labels = rev(time_dframe$stage)
    )

		# calculate time on checking bibliographies for new entries
    n_new_refs <- as.numeric(input$n_bib) * 50
    time_total <- sum(count_dframe$time_days[2:5]/count_dframe$checked[2:5])
    n_articles_total <- count_dframe$count_post[1]
    time_dframe$value[6] <- (time_total / n_articles_total) * n_new_refs
		# counts_duplicates <- as.numeric(input$n_bib) * 50 #  * c(1, 0.192)
		# time_dframe$value[6] <- sum(counts_duplicates / count_dframe$nperday[2:3])

		# calculate administration time
		time_dframe$value[1] <- sum(time_dframe$value) * as.numeric(input$pc_admin) * 0.01
		time_dframe <- merge(
      time_dframe,
      group_lookup,
      by = "stage",
      all = FALSE
    )
		time_dframe <- time_dframe[order(time_dframe$order), ]
		time_dframe$group_factor <- factor(
			time_dframe$group_order,
			levels = c(1:5),
			labels = c("Planning", "Searching", "Screening", "DEAS", "Reporting")
    )
		time_dframe$caption <- paste0(round(time_dframe$value, 1), " days")
		time_dframe_clean <- time_dframe[, c("stage", "group_factor", "value")]
		colnames(time_dframe_clean) <- c("stage", "stage_group", "time_days")
		plot_data$p1 <- time_dframe_clean

		# draw plot 1 (time)
		color_palette <- viridisLite::viridis(
      n = 5,
      begin = 0,
      end = 0.9,
      direction = -1
    )
		output$plot_days <- renderPlotly({
			p <- plot_ly(
				data = time_dframe,
				x = ~value,
				y = ~y,
				color = ~group_factor,
				colors = color_palette,
				hoverinfo = "text",
				text = ~caption,
				type = "bar",
				orientation = "h"
			) %>%
			layout(
				xaxis = list(title="Number of Days"),
				yaxis = list(title=""),
				autosize = TRUE,
				margin = list(l=150, r=10, b=50, t=10, pad=4)
			)
			p
		})

		# draw plot 2 (articles)
		count_dframe$y <- factor(
			c(nrow(count_dframe):1),
			levels = c(1:nrow(count_dframe)),
			labels = rev(count_dframe$plot2_stage)
    )
		count_dframe$caption <- paste0("n = ", round(count_dframe$count_pre, 0))
		count_dframe_clean <- count_dframe[, c("plot1_stage",
			"count_pre", "nperday", "percent", "checked", "count_post", "time_days")]
		colnames(count_dframe_clean) <- c("review_stage", "article_count_pre", "n_per_day",
			"proportion_retained", "proportion_checked", "article_count_post", "time_days")
		count_dframe_clean$n_per_day[1] <- NA
		plot_data$p2 <- count_dframe_clean

		output$plot_articles <- renderPlotly({
			p <- plot_ly(
				data = count_dframe,
				x = ~ count_pre,
				y = ~y,
				marker=list(color = color_palette[3]),
				hoverinfo = "text",
				text = ~caption,
				type = "bar",
				orientation = "h"
			) %>%
			layout(
				xaxis = list(
					tick0 = 0,
					dtick = 1,
					title = "Number of Articles",
					type = "log",
					exponentformat = "E"
				),
				yaxis = list(title = ""),
				autosize = TRUE,
				margin = list(l=150, r=10, b=50, t=10, pad=4)
			)
			p
		})

		# boxes
		output$total_box <- renderValueBox({
			valueBox(
				value = paste0(
          round(sum(time_dframe$value)/as.numeric(input$fte), 0),
          " days"
        ),
				subtitle =  paste0(
          "Total Time Taken (",
          round(as.numeric(input$fte), 1),
          " FTE)"
        ),
				icon = icon("calendar"),
				color = "black"
			)
		})
		output$fte_box <- renderValueBox({
			valueBox(
				value = paste0(round(sum(time_dframe$value), 0), " days"),
				subtitle = "Full-Time Equivalent",
				icon = icon("calendar-minus-o"),
				color = "black"
			)
		})
		output$admin_box <- renderValueBox({
			valueBox(
				value = paste0(round(time_dframe$value[1], 0), " days"),
				subtitle = "Administration",
				icon = icon("calendar-plus-o"),
				color = "black"
			)
		})

		# downloads
		output$download_table1 <- downloadHandler(
			filename = "PredicTER_times_output.csv",
			content = function(file){write.csv(plot_data$p1, file, row.names=FALSE)}
		)
		output$download_table2 <- downloadHandler(
			filename = "PredicTER_days_output.csv",
			content = function(file){write.csv(plot_data$p2, file, row.names=FALSE)}
		)

	}) # end observe

	# modals to view data
	observeEvent(input$view_data_1, {
	shiny::showModal(
		shiny::modalDialog(
			renderTable(plot_data$p1),
			title = "Table 1: Time taken per stage",
			footer = shiny::modalButton("Close"),
			size = "m",
			easyClose = FALSE
		)
	)})

	observeEvent(input$view_data_2, {
	shiny::showModal(
		shiny::modalDialog(
			renderTable(plot_data$p2),
			title = "Table 2: Article counts per stage",
			footer = shiny::modalButton("Close"),
			size = "l",
			easyClose = FALSE
		)
	)})


	# pop-up windows (modals) to show help files
	observeEvent(input$help_review_type, {
		shiny::showModal(
			shiny::modalDialog(
				includeHTML("0_review_type_help_text.txt"),
				title = "Review Type",
				footer = shiny::modalButton(
          label = "OK",
          icon = shiny::icon("check", lib="font-awesome")
        ),
				easyClose = TRUE
      )
    )
  })
	observeEvent(input$help_planning, {
		shiny::showModal(
			shiny::modalDialog(
				includeHTML("1_planning_help_text.txt"),
				title = "Planning",
				footer = shiny::modalButton(
          label = "OK",
          icon = shiny::icon("check", lib="font-awesome")
        ),
				easyClose = TRUE
      )
    )
  })
	observeEvent(input$help_searching, {
		shiny::showModal(
			shiny::modalDialog(
				includeHTML("2_searching_help_text.txt"),
				title = "Searching",
        footer = shiny::modalButton(
          label = "OK",
          icon = shiny::icon("check", lib="font-awesome")
        ),
				easyClose = TRUE
      )
    )
  })
	observeEvent(input$help_screening, {
		shiny::showModal(
			shiny::modalDialog(
				includeHTML("3_screening_help_text.txt"),
				title = "Screening",
        footer = shiny::modalButton(
          label = "OK",
          icon = shiny::icon("check", lib="font-awesome")
        ),
				easyClose = TRUE
      )
    )
  })
	observeEvent(input$help_deas, {
		if(any(input$tabs=="syst_review")){
      deas_help <- "4_DEAS_SR_help_text.txt"
		}else{
      deas_help <- "4_DEAS_SM_help_text.txt"
    }
		shiny::showModal(
			shiny::modalDialog(
				includeHTML(deas_help),
				title = "Data Extraction, Appraisal and Synthesis",
        footer = shiny::modalButton(
          label = "OK",
          icon = shiny::icon("check", lib="font-awesome")
        ),
				easyClose = TRUE
      )
    )
  })
	observeEvent(input$help_reporting, {
		shiny::showModal(
			shiny::modalDialog(
				includeHTML("5_reporting_help_text.txt"),
				title = "Reporting",
        footer = shiny::modalButton(
          label = "OK",
          icon = shiny::icon("check", lib="font-awesome")
        ),
				easyClose = TRUE
      )
    )
  })
	observeEvent(input$about, {
		shiny::showModal(
			shiny::modalDialog(
				includeHTML("6_about_PredicTER.txt"),
				title = "PredicTER",
        footer = shiny::modalButton(
          label = "OK",
          icon = shiny::icon("check", lib="font-awesome")
        ),
				easyClose = TRUE
      )
    )
  })

}