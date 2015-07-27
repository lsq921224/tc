library(solr)
library(RJSONIO)
#library(RMySQL)

options(shiny.trace = TRUE)

# exit types
#exit_types <- c("chat.agentExited", "chat.customerExited", "chat.customerLostConnection & message=The customer lost network connectivity", "chat.customerLostConnection & message=the customer navigated to an untagged page")

# customer lost connectoin type
lost_lost = "lost_lost"
lost_navi = "lost_navi"
customer_lost = "chat.customerLostConnection"
lost_message = "Customer connection lost. The customer lost network connectivity"
navi_message = "Either browser was closed, or the customer navigated to an untagged page"
url = "http://172.26.129.237/solr/select"


shinyServer(function(input,output){
	cons<-dbListConnections(MySQL())
		for(con in cons)
			dbDisconnect(con)
#con = dbConnect(MySQL(), user='test', password='', dbname='configuration', host='172.26.128.56')
	jsonFile <- reactive(
	{
		# log file 
		#jsonData = paste("data/transcript/", input$files, sep = "")
		#print(jsonData)
		#jsonData_text = readLines(jsonData)
		sDate = input$dates[1]
		eDate = input$dates[2]
		startDate = paste0("[",sDate,"T00:00:00Z TO ",eDate,"T23:59:99Z]")
		query = paste0("startDate:",startDate)
		client = input$client
		if (client != "All")
		{
			clientID = h[[client]]
			query = paste0(query, " AND siteID:",clientID)
			if (input$bu == "All")
				query = paste0(query, " AND businessUnitID:", "*")
			else if (input$bu != "na")
				query = paste0(query, " AND businessUnitID:", input$bu)

			if (input$ag == "All")
				query = paste0(query, " AND agentGroupID:", "*")
			else if (input$ag != "na")
				query = paste0(query, " AND agentGroupID:", input$ag)
		}
		fields = c("engagementID", "startDate","siteID","automatonID","automatonName","automatonType","agentID","agentAlias","agentName","businessUnitID","businessUnitName","browserType","deviceType","operatingSystem","agentGroupID","agentGroupName","businessRuleID","businessRuleName","transcript_en")
		jsonData = solr_search(q = query, base = url, fl = fields, wt = "json", raw= TRUE, rows = 2000)
		#jsonData_str = paste(jsonData, collapse = "")
		# transfer log file to R object
		#jsonData_str = str_sub(jsonData, end = nchar(jsonData) - )
		transcript = RJSONIO::fromJSON(jsonData[1])
		class(transcript)
		return(transcript)
	}
	)

	output$sortable_rui <- renderUI(
	{
		returnOrder("sortable", input$choose_status)
	}
	)

	output$business_unit <- renderUI(
	{

		if (input$client != "All")
		{
		
		siteId <- h[[input$client]]
		query = paste0("SELECT businessUnitID FROM business_units where siteID = ", siteId)
		mydb = dbConnect(RMySQL::MySQL(), user='test', password='', dbname='configuration', host='172.26.128.56')
		res <- dbSendQuery(mydb, query )
		d <- dbFetch(res, n = -1)
		bu <- c(d)	
		dbDisconnect(mydb)
		if (!is.null(bu$businessUnitID))
		 if (length(bu$businessUnitID))
		  selectInput("bu", "Choose Business Unit", c("All", bu$businessUnitID))
		 else
		  selectInput("bu", "Choose Business Unit", c("No Business Unit Available" = "na"))
		}
	}
	)

 	output$agent_group <- renderUI(
        {

		if (input$client != "All")
		{
                siteId <- h[[input$client]]
                query = paste0("SELECT agent_group_id FROM agent_groups where site_id = ", siteId)
		mydb = dbConnect(RMySQL::MySQL(), user='test', password='', dbname='configuration', host='172.26.128.56')
		res <- dbSendQuery(mydb, query )
                d <- dbFetch(res, n = -1)
                bu <- c(d)
		dbDisconnect(mydb)
		if (!is.null(bu$agent_group_id))
		 if (length(bu$agent_group_id) > 0)
                	selectInput("ag", "Choose Agent Group", c ("All", bu$agent_group_id))
		 else
		  	selectInput("ag", "Choose Agent Group", c("No Agent Group Available" = "na"))
		}
        }
        )

	
	output$showorder <- renderPrint(
	{
		print("Date Range")
		print(input$dates)
		print("Client")
		print(input$client)
		print("BU")
		print(input$bu)
		print("AG")
		print(input$ag)
		print("Exit Status Sequence")
		print(input$sortable)
		print("Sort By")
		print(input$sort_option)
		#startDate = paste0("[",input$dates[1],"T00:00:00Z TO ",input$dates[2],"T23:59:99Z]")
		#print(startDate)
		#sDate = input$dates[1]
		#eDate = input$dates[2]
		#startDate = paste0("[",sDate,"T00:00:00Z TO ",eDate,"T23:59:99Z]")
		#jsonData = solr_search(q = startDate, base = url, wt = "json", raw = TRUE)
		#print(solr_search(q= "*:*", base = url, wt = "json"))	
		#print(jsonData[1])
		#jsonData = paste("data/transcript/", input$files, sep = "")
		#print(jsonData)
		sDate = input$dates[1]
		eDate = input$dates[2]
		startDate = paste0("[",sDate,"T00:00:00Z TO ",eDate,"T23:59:99Z]")
		query = paste0("startDate:",startDate)
		client = input$client
		if (client != "All")
		{
			clientID = h[[client]]
			query = paste0(query, " AND siteID:",clientID)
			if (input$bu == "All")
				query = paste0(query, " AND businessUnitID:", "*")
			else if (input$bu != "na")
				query = paste0(query, " AND businessUnitID:", input$bu)

			if (input$ag == "All")
				query = paste0(query, " AND agentGroupID:", "*")
			else if (input$ag != "na")
				query = paste0(query, " AND agentGroupID:", input$ag)
		}
		print(query)
		#print(jsonFile()[[2]]$docs)
		#jsonData = solr_search(q = query, base = url, wt = "json", raw= TRUE)
		#print(jsonData)
	}
	)

	output$barPlot <- renderPlot({

	# log file 
#	jsonData = paste("data/transcript/", input$files, sep = "")
	#print(jsonData)
#	jsonData_text = readLines(jsonData)
#	jsonData_str = paste(jsonData, collapse = "")

	# transfer log file to R object
#	transcript = RJSONIO::fromJSON(jsonData_str)
#	class(transcript)

	transcript <- jsonFile()[[2]]
	#if (transcript$numFound == 0)
	#	return
	status <- input$sortable
	sort_op <- input$sort_option
	chart_op <- input$graph_option

	total <- c()
	results <- c()

	num_engagements = length(transcript$docs)
	num_status = length(status)
	count <- 0
	if (num_engagements != 0)
	{
	#print(num_engagements)
	for ( k in 1:num_engagements)
	{
  		if (num_status == 0)
    			break
  		a <- rep(-1, times = num_status)
  		found_match <- TRUE
		switch(sort_op,
			"Default" = {},
			"Browser" = {
				total <- c(transcript$docs[[k]]$browserType, total)
			},
			"Operating System" = {
				total <- c(transcript$docs[[k]]$operatingSystem, total)
			},
			"AutomatonID" = {
				n_units = length(transcript$docs[[k]]$automatonID)
  				for (p in 1:n_units)
  				{
    					total <- c(transcript$docs[[k]]$automatonID[p], total)
  				}
			},
			"AutomatonName" = {
				n_units = length(transcript$docs[[k]]$automatonName)
  				for (p in 1:n_units)
  				{
    					total <- c(transcript$docs[[k]]$automatonName[p], total)
  				}
			},
			"AutomatonType" = {
				n_units = length(transcript$docs[[k]]$automatonType)
  				for (p in 1:n_units)
  				{
    					total <- c(transcript$docs[[k]]$automatonType[p], total)
  				}
			},
			"AgentGroupID" = {
				n_units = length(transcript$docs[[k]]$agentGroupID)
  				for (p in 1:n_units)
  				{
    					total <- c(transcript$docs[[k]]$agentGroupID[p], total)
  				}
			},
			"AgentGroupName" = {
				n_units = length(transcript$docs[[k]]$agentGroupName)
  				for (p in 1:n_units)
  				{
    					total <- c(transcript$docs[[k]]$agentGroupName[p], total)
  				}
			},
			"AgentID" = {
				n_units = length(transcript$docs[[k]]$agentID)
  				for (p in 1:n_units)
  				{
    					total <- c(transcript$docs[[k]]$agentID[p], total)
  				}
			},
			"AgentFullName" = {
				n_units = length(transcript$docs[[k]]$agentName)
  				for (p in 1:n_units)
  				{
    					total <- c(transcript$docs[[k]]$agentName[p], total)
  				}
			},
			"AgentAlias" = {
				n_units = length(transcript$docs[[k]]$agentAlias)
  				for (p in 1:n_units)
  				{
    					total <- c(transcript$docs[[k]]$agentAlias[p], total)
  				}
			},
			"BusinessUnitID" = {
				n_units = length(transcript$docs[[k]]$businessUnitID)
  				for (p in 1:n_units)
  				{
    					total <- c(transcript$docs[[k]]$businessUnitID[p], total)
  				}
			},
			"BusinessUnitName" = {
				n_units = length(transcript$docs[[k]]$businessUnitName)
  				for (p in 1:n_units)
  				{
    					total <- c(transcript$docs[[k]]$businessUnitName[p], total)
  				}
			},
			"BusinessRuleID" = {
				total <- c(transcript$docs[[k]]$businessRuleID, total)
			},
			"BusinessRuleName" = {
				total <- c(transcript$docs[[k]]$businessRuleName, total)
			},
			"Device Type" = {
				total <- c(transcript$docs[[k]]$deviceType, total)
			},
			"Launch Type" = {
				total <- c(transcript$docs[[k]]$launchType, total)
			},
			"Sale Qualified" = {
				total <- c(transcript$docs[[k]]$saleQualified, total)
			},
			"Site ID" = {
				total <- c(transcript$docs[[k]]$siteID, total)
			}	
									

		)
  		trans2 = transcript$docs[[k]]$transcript_en;
		if (!is.null(trans2))
		{
		trans = RJSONIO::fromJSON(trans2)
  		num_dialogs = length(trans)
  		#print (num_dialogs)
  		for (j in 1:num_dialogs)
  		{
    			for (i in 1:num_status)
    			{
      			#print(trans[[j]]$type)
				chat_type <- trans[[j]]$type
				if (chat_type == status[i])
      				{
        				a[i] <- j
      				}	
				else if (status[i] == lost_lost)
				{
					if (chat_type == customer_lost && trans[[j]]$text == lost_message )
						a[i] <- j
				}
				else if (status[i] == lost_navi)
				{
					if (chat_type == customer_lost && trans[[j]]$text  == navi_message )
						a[i] <- j
				}
    			}
  		}
		}
  	for (index in a)
  	{
    		if (index == -1)
    		{
      			found_match = FALSE
      			break
    		}
  	}
  	#print(a)
  	if (found_match && !is.unsorted(a))
	{
    		#count <- count + 1
		switch(sort_op,
		"Default" = {
			count <- count + 1
		},
		"Browser" = {
			results <- c(transcript$docs[[k]]$browserType, results)
		},
		"Operating System" = {
			results <- c(transcript$docs[[k]]$operatingSystem, results)
		},
		"AutomatonID" = {
			n_units = length(transcript$docs[[k]]$automatonID)
				for (p in 1:n_units)
				{
					results <- c(transcript$docs[[k]]$automatonID[p], results)
				}
		},
		"AutomatonName" = {
			n_units = length(transcript$docs[[k]]$automatonName)
				for (p in 1:n_units)
				{
					results <- c(transcript$docs[[k]]$automatonName[p], results)
				}
		},
		"AutomatonType" = {
			n_units = length(transcript$docs[[k]]$automatonType)
				for (p in 1:n_units)
				{
					results <- c(transcript$docs[[k]]$automatonType[p], results)
				}
		},
		"AgentGroupID" = {
			n_units = length(transcript$docs[[k]]$agentGroupID)
				for (p in 1:n_units)
				{
					results <- c(transcript$docs[[k]]$agentGroupID[p], results)
				}
		},
		"AgentGroupName" = {
			n_units = length(transcript$docs[[k]]$agentGroupName)
				for (p in 1:n_units)
				{
					results <- c(transcript$docs[[k]]$agentGroupName[p], results)
				}
		},
		"AgentID" = {
			n_units = length(transcript$docs[[k]]$agentID)
				for (p in 1:n_units)
				{
					results <- c(transcript$docs[[k]]$agentID[p], results)
				}
		},
		"AgentFullName" = {
			n_units = length(transcript$docs[[k]]$agentName)
				for (p in 1:n_units)
				{
					results <- c(transcript$docs[[k]]$agentName[p], results)
				}
		},
		"AgentAlias" = {
			n_units = length(transcript$docs[[k]]$agentAlias)
				for (p in 1:n_units)
				{
					results <- c(transcript$docs[[k]]$agentAlias[p], results)
				}
		},
		"BusinessUnitID" = {
			n_units = length(transcript$docs[[k]]$businessUnitID)
  			for (p in 1:n_units)
  			{
    				results <- c(transcript$docs[[k]]$businessUnitID[p], results)
  			}
		},
		"BusinessUnitName" = {
			n_units = length(transcript$docs[[k]]$businessUnitName)
  			for (p in 1:n_units)
  			{
    				results <- c(transcript$docs[[k]]$businessUnitName[p], results)
  			}
		},
		"BusinessRuleID" = {
			results <- c(transcript$docs[[k]]$businessRuleID, results)
		},
		"BusinessRuleName" = {
			results <- c(transcript$docs[[k]]$businessRuleName, results)
		},
		"Device Type" = {
			results <- c(transcript$docs[[k]]$deviceType, results)
		},
		"Launch Type" = {
			results <- c(transcript$docs[[k]]$launchType, results)
		},
		"Sale Qualified" = {
			results <- c(transcript$docs[[k]]$saleQualified, results)
		},
		"Site ID" = {
			results <- c(transcript$docs[[k]]$siteID, results)
		}
		
		)
  		
	}
	}	

	if (length(status) >= 1)
	{
	
	if (sort_op == "Default")
	{	
		if (chart_op == "Bar Chart")
		{
			counts <- c(count, num_engagements - count)
			pcts <- c(count/num_engagements, (num_engagements - count) / num_engagements)				       #cat (counts[1], file=stderr())
			barplot(pcts, main = "Bar Chart of Chats", names.arg = c("#chats in this exit status order", "other chats"))
		}
		else
		{
			counts <- c(count, num_engagements - count)
			lbls <- c("#chats in this exit status order", "other chats")
			pct <- round(counts/sum(counts) * 100)
			lbls <- paste(lbls, pct)
			lbls <- paste(lbls, "%", sep="")
			pie(counts,labels = lbls, col = rainbow(length(lbls)),main = "Pie Chart of Chats")
		}
	}
	else
	{
		if (length(results) >= 1)
		{

		all_table <- table(factor(total))
		result_table <- table(factor(results))
		counts <- c()
		for (name in names(result_table))
		{
			counts <- c(result_table[name] / all_table[name], counts)
		}
		if (chart_op == "Bar Chart")
		{
			#pct <- round(counts/sum(counts))
			barplot(counts, main = "Bar Chart of Chats", names.arg = names(result_table))
		}
		else
		{
			lbls <- names(result_table)
			pct <- round(counts * 100)
			lbls <- paste(lbls, pct)
			lbls <- paste(lbls, "%", sep="")
			pie(counts,labels = lbls, col = rainbow(length(lbls)),main = "Pie Chart of Chats")
		
		}
	}
	}
	}
	}
})
	#dbDisconnect(con)
})

