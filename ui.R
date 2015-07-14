file_names <- list.files(path = "data/transcript/")

shinyUI(fluidPage(
	titlePanel("Transcripts Demo"),
	sidebarLayout(
		sidebarPanel(
			includeScript("www/js/jquery-ui.custom.min.js"),
			helpText("Choose Log File"),
			selectInput("files", NULL, file_names
			),
			helpText("Choose Exit Status"),
			wellPanel(
			checkboxGroupInput("choose_status","",c("customerExited" = "chat.customerExited","agentExited" = "chat.agentExited","customerLostConnection - Lost Connection" = "lost_lost", "customerLostConnection - Navigate to untagged page" = "lost_navi"), selected="agentExited")
			),
			helpText("Sort Exit Status (Drap and Drop status within this panel)"),
			wellPanel(
				uiOutput('sortable_rui')
			),
			helpText("Sort Option"),
			wellPanel(
				selectInput("sort_option", NULL, c("Default", "Browser", "Operating System", "AgentGroupID", "AgentGroupName", "AutomatonID", "AutomatonName", "AutomatonType", "BusinessUnitID","BusinessUnitName" ,"BusinessRuleID", "BusinessRuleName", "Site ID","Device Type", "Launch Type", "Sale Qualified"), selected = NULL)
			),
			helpText("Graph Option"),
			wellPanel(
				radioButtons("graph_option", NULL, c("Bar Chart", "Pie Chart"), selected = "Bar Chart", inline = TRUE)
			)
		),
		
		mainPanel(
			verbatimTextOutput("showorder"),
			plotOutput("barPlot")
		)
	)
)
)

