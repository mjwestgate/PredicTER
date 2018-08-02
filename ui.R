library(shinydashboard)
library(shiny)
library(plotly)

header <- shinydashboard::dashboardHeader(
	title = tags$a(tags$img(
    src = "PredicTER_logo.png",
    width = 250,
    height = 40
    )
  ),
	titleWidth = 350
)

sidebar <- shinydashboard::dashboardSidebar(
	width = 350,
	sidebarMenu(
		id = "tabs",
		menuItem("Review Type",
			menuSubItem(
        tabName = "syst_review",
        text = "Systematic Review",
        selected = TRUE
      ),
			menuSubItem(
        tabName = "syst_map",
        text = "Systematic Map"
      ),
			uiOutput("review_type_text"),
			actionLink(
        inputId = "help_review_type",
        label = icon("question-circle")
      )
		),
		menuItem("Planning",
      icon = icon("pencil"),
      expandedName = "planning",
			br(),
			splitLayout(
				HTML("Number of days spent<br>on project planning"),
				textInput(
          inputId = "planning",
          label = NULL,
          value="20"
        ),
				cellWidths = c("60%", "40%")
			),
			splitLayout(
				HTML("Number of days spent<br>on protocol development"),
				textInput(
          inputId = "protocol",
          label = NULL,
          value = "5"
        ),
				cellWidths = c("60%", "40%")
			),
			splitLayout(
				HTML("Percentage of project time<br>spent on administration"),
				textInput(
          inputId = "pc_admin",
          label = NULL,
          value = "19"
        ),
				cellWidths = c("60%", "40%")
			),
			splitLayout(
				HTML("Proportion of lead author time<br>spent on project (FTE)"),
				textInput(
          inputId = "fte",
          label = NULL,
          value = "0.5"
        ),
				cellWidths = c("60%", "40%")
			),
			actionLink(
        inputId = "help_planning",
        label = icon("question-circle")
      )
		),
		menuItem("Searching",
      icon = icon("search"),
      expandedName = "searching",
			h4("Databases"),
			splitLayout(
				p("Type"),
				HTML("Number searched<br>(total)"),
				HTML("Number searched<br>per day"),
				cellWidths = c("25%", "40%", "35%")
			),
			splitLayout(
				HTML("Academic<br>Databases"),
				textInput(
          inputId = "n_databases",
          label = NULL,
          value = "9"
        ),
				textInput(
          inputId = "n_db_perday",
          label = NULL,
          value = "4"
        ),
				cellWidths = c("20%", "40%", "40%")
			),
			splitLayout(
				HTML("Grey<br>Literature"),
				textInput(
          inputId = "n_grey",
          label = NULL,
          value = "16"
        ),
				textInput(
          inputId = "n_grey_perday",
          label = NULL,
          value = "12"),
				cellWidths = c("20%", "40%", "40%")
			),
			splitLayout(
				HTML("Reference<br>Lists<br>Checked"),
				textInput(
          inputId = "n_bib",
          label = NULL,
          value = "10"),
				cellWidths = c("20%", "40%")
			),
			br(),
			h4("Further Information"),
			splitLayout(
				HTML("Days spent on additional<br>search methods"),
				textInput(
          inputId = "time_grey_add",
          label = NULL,
          value = "2"),
				cellWidths = c("60%", "40%")
			),
			splitLayout(
				HTML("<b>Total number of articles<br>located</b> (all searches)"),
				textInput(
          inputId = "n_search",
          label = NULL,
          value = "11786"),
				cellWidths = c("60%", "40%")
			),
			splitLayout(
				HTML("% unique articles<br>(i.e. % left after de-duplication)"),
				textInput(
          inputId = "unique_percent",
          label = NULL,
          value = "72.1"
        ),
				cellWidths = c("60%", "40%")
			),
			splitLayout(
				p("Days spent removing duplicates"),
				textInput(
          inputId = "unique_time",
          label = NULL,
          value = "1.4"),
				cellWidths = c("60%", "40%")
			),
			actionLink(
        inputId = "help_searching",
        label = icon("question-circle")
      )
		),
		menuItem("Screening",
      icon = icon("book"),
      expandedName ="screening",
			splitLayout(
				HTML("Review<br>stage"),
				HTML("N. screened<br>per day"),
				HTML("Percent<br>retained"),
				HTML("Percent<br>checked"),
				cellWidths = c("30%", "25%", "25%", "20%")
			),
			splitLayout(
				HTML("Title<br>screening"),
				textInput(
          inputId = "row2_title_nperday",
          label = NULL,
          value = "854"
        ),
				textInput(
          inputId = "row2_title_percent",
          label = NULL,
          value = "14.6"
        ),
				textInput(
          inputId = "row2_title_checked",
          label = NULL,
          value = "10"),
				cellWidths = c("25%", "25%", "25%", "25%")
			),
			splitLayout(
				HTML("Abstract<br>screening"),
				textInput(
          inputId = "row3_abstract_nperday",
          label = NULL,
          value = "192"
        ),
				textInput(
          inputId = "row3_abstract_percent",
          label = NULL,
          value = "25.0"
        ),
				textInput(
          inputId = "row3_abstract_checked",
          label = NULL,
          value = "10"
        ),
				cellWidths = c("25%", "25%", "25%", "25%")
			),
			splitLayout(
				HTML("Full text<br>retrieval"),
				textInput(
          inputId = "row4_retrieved_nperday",
          label = NULL,
          value = "171"
        ),
				textInput(
          inputId = "row4_retrieved_percent",
          label = NULL,
          value = "131.1"
        ),
				textInput(
          inputId = "row4_retrieved_checked",
          label = NULL,
          value = "0"
        ),
				cellWidths = c("25%", "25%", "25%", "25%")
			),
			splitLayout(
				HTML("Full text<br>screening"),
				textInput(
          inputId = "row5_fulltext_nperday",
          label = NULL,
          value = "44"
        ),
				textInput(
          inputId = "row5_fulltext_percent",
          label = NULL,
          value = "23.0"
        ),
				textInput(
          inputId = "row5_fulltext_checked",
          label = NULL,
          value = "10"
        ),
				cellWidths = c("25%", "25%", "25%", "25%")
			),
			actionLink(
        inputId = "help_screening",
        label = icon("question-circle")
      )
		),
		menuItem("Data Extraction, Appraisal & Synthesis",
      icon = icon("table"),
      expandedName ="data",
			tags$div(id = 'placeholder_selector'), # location for critical appraisal selector
			splitLayout(
				HTML("Review<br>stage"),
				HTML("N. screened<br>per day"),
				HTML("Percent<br>retained"),
				HTML("Percent<br>checked"),
				cellWidths = c("30%", "25%", "25%", "20%")
			),
			splitLayout(
				HTML("Meta-data<br>extraction"),
				textInput(
          inputId = "row6_metadata_nperday",
          label = NULL,
          value = "16.7"
        ),
				textInput(
          inputId = "row6_metadata_percent",
          label = NULL,
          value = "100.0"
        ),
				textInput(
          inputId = "row6_metadata_checked",
          label = NULL,
          value = "0"
        ),
				cellWidths = c("25%", "25%", "25%", "25%")
			),
			tags$div(id = 'placeholder_ca'),  # location for critical appraisal content
			tags$div(id = 'placeholder_SRs'), # location for data extraction and prep.
			actionLink("help_deas", icon("question-circle"))
		),
		menuItem("Reporting", icon=icon("bullhorn"), expandedName="reporting",
			br(),
			splitLayout(
				HTML("Number of days spent on<br>quant. or qual. synthesis"),
				textInput(
          inputId = "synthesis",
          label = NULL,
          value = "15"
        ),
				cellWidths = c("60%", "40%")
			),
			splitLayout(
				HTML("Number of days spent<br>on report writing"),
				textInput(
          inputId = "report",
          label = NULL,
          value = "15"
        ),
				cellWidths = c("60%", "40%")
			),
			splitLayout(
				HTML("Number of days spent<br>on communication"),
				textInput(
          inputId = "comms",
          label = NULL,
          value = "10"
        ),
				cellWidths = c("60%", "40%")
			),
			splitLayout(
				HTML("Number of days spent<br>in meetings"),
				textInput(
          inputId = "meetings",
          label = NULL,
          value = "4"
        ),
				cellWidths = c("60%", "40%")
			),
			actionLink(
        inputId = "help_reporting",
        label = icon("question-circle")
      )
		),
		br(),
		br(),
		actionLink(
      inputId = "about",
      label = "About PredicTER"
    )
	)
)


body<-shinydashboard::dashboardBody(

    tags$style(HTML("
		.content-wrapper,
			.right-side {
				background-color: #e2e2e2;
			}
		.small-box.bg-black {
			background-color: #777777 !important;
			color: #fff !important;
		}
		.skin-black .main-header .logo {
			background-color: #777777;
		}
		.skin-black .main-header .logo:hover {
			background-color: #777777;
		}
		.skin-black .main-header .navbar {
			background-color: #777777;
		}
		.box.box-solid.box-primary>.box-header {
			color:#fff;
			background:#444444
		}
		.box.box-solid.box-primary{
			border-bottom-color:#444444;
			border-left-color:#444444;
			border-right-color:#444444;
			border-top-color:#444444;
		}
		.download_class{
			background-color: #444444;
			color: #e2e2e2;
		}
		#view
	")),
	fluidRow(
		infoBoxOutput("total_box"),
		infoBoxOutput("fte_box"),
		infoBoxOutput("admin_box")
	),
	fluidRow(
		box(
      width = 8,
      title = "Number of Days",
      solidHeader = TRUE,
      status = "primary",
			plotly::plotlyOutput(
        outputId = "plot_days",
        height = 650
      ),
			br(),
			actionButton(
        inputId = "view_data_1",
        label = "View data",
        style = "background-color: #444444; color: #e2e2e2;"
      ),
			downloadButton(
        outputId = "download_table1",
        label = "Download csv",
        class = "download_class"
      )
		),
		box(
      width = 4,
      title = "Number of Articles",
      solidHeader = TRUE,
      status = "primary",
			plotly::plotlyOutput(
        outputId = "plot_articles",
        height = 650
      ),
			br(),
			actionButton(
        inputId = "view_data_2",
        label = "View data",
        style = "background-color: #444444; color: #e2e2e2;"
      ),
			downloadButton(
        outputId = "download_table2",
        label = "Download csv",
        class = "download_class"
      )
		)
	)
)

ui <- shinydashboard::dashboardPage(
  header,
  sidebar,
  body,
  title = "PredicTER",
  skin = "black")