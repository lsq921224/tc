#file_names <- list.files(path = "data/transcript/")
#library(RMySQL)
#mydb = dbConnect(MySQL(), user='test', password='', dbname='configuration', host='172.26.128.56')
#res <- dbSendQuery(mydb, "SELECT siteID, siteName FROM sites where active = 1")
#site_id_names <- dbFetch(res, n = -1)
#site_ids <- c(site_id_names[1]$siteID)
#site_names <- c(site_id_names[2]$siteName)
#sn <- paste(site_names, site_ids, sep='=')

shinyUI(fluidPage(
	titlePanel("Transcripts Demo"),
	fluidRow(
		sidebarPanel(
			includeScript("www/js/jquery-ui.custom.min.js"),
			# Copy the line below to make a date range selector
  			wellPanel(
				dateRangeInput("dates", label = h3("Date range"), start = "2014-10-14" ),
				selectInput("client", "Choose Client", c("All",site_names)),
				uiOutput("business_unit"),
				uiOutput("agent_group")
				#selectInput("business_unit", "Choose Business Unit", c("1","2")),
				#selectInput("agent_group", "Choose Agent Group", c("1","2"))
			),
			#helpText("Choose Log File"),
			#selectInput("files", "Choose Log File", file_names
			#),
			#helpText("Choose Exit Status"),
			wellPanel(
				checkboxGroupInput("choose_status","Choose Exit Status",c("customerExited" = "chat.customerExited","agentExited" = "chat.agentExited","customerLostConnection - Lost Connection" = "lost_lost", "customerLostConnection - Navigate to untagged page" = "lost_navi"), selected="agentExited")
			),
			helpText("Sort Exit Status (Drap and Drop status within this panel)"),
			wellPanel(
				uiOutput('sortable_rui')
			),
			#helpText("Sort Option"),
			wellPanel(
				selectInput("sort_option", "Sort Option", c("Default", "Browser", "Operating System", "AgentGroupID", "AgentGroupName", "AutomatonID", "AutomatonName", "AutomatonType", "BusinessUnitID","BusinessUnitName" ,"BusinessRuleID", "BusinessRuleName", "Site ID","Device Type", "Launch Type"), selected = NULL)
			),
			#helpText("Graph Option"),
			wellPanel(
				radioButtons("graph_option", "Graph Option", c("Bar Chart", "Pie Chart"), selected = "Bar Chart", inline = TRUE)
			)
		),
		
		mainPanel(
			verbatimTextOutput("showorder"),
			plotOutput("barPlot")
		)
	)
)
)

