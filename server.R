library(RJSONIO)
options(shiny.trace = TRUE)

# exit types
#exit_types <- c("chat.agentExited", "chat.customerExited", "chat.customerLostConnection & message=The customer lost network connectivity", "chat.customerLostConnection & message=the customer navigated to an untagged page")

# customer lost connectoin type
lost_lost = "lost_lost"
lost_navi = "lost_navi"
customer_lost = "chat.customerLostConnection"
lost_message = "Customer connection lost. The customer lost network connectivity"
navi_message = "Either browser was closed, or the customer navigated to an untagged page"

shinyServer(function(input,output){

	jsonFile <- reactive(
	{
		# log file 
		jsonData = paste("data/transcript/", input$files, sep = "")
		#print(jsonData)
		#jsonData_text = readLines(jsonData)
		jsonData_str = paste(jsonData, collapse = "")
	
		# transfer log file to R object
		transcript = RJSONIO::fromJSON(jsonData_str)
		class(transcript)
		return(transcript)
	}
	)

	output$sortable_rui <- renderUI(
	{
		returnOrder("sortable", input$choose_status)
	}
	)
	
	output$showorder <- renderPrint(
	{
		print("Log File")
		print(input$files)
		print("Exit Status Sequence")
		print(input$sortable)
		print("Sort By")
		print(input$sort_option)
		
	
	#jsonData = paste("data/transcript/", input$files, sep = "")
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

	transcript <- jsonFile()
	status <- input$sortable
	sort_op <- input$sort_option
	chart_op <- input$graph_option

	total <- c()
	results <- c()

	num_engagements = length(transcript[3]$engagements)
	num_status = length(status)
	count <- 0

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
				total <- c(transcript[3]$engagements[[k]]$browserType, total)
			},
			"Operating System" = {
				total <- c(transcript[3]$engagements[[k]]$operatingSystem, total)
			},
			"AutomatonID" = {
				n_units = length(transcript[3]$engagements[[k]]$automatons)
  				for (p in 1:n_units)
  				{
    					total <- c(transcript[3]$engagements[[k]]$automatons[[p]][[1]], total)
  				}
			},
			"AutomatonName" = {
				n_units = length(transcript[3]$engagements[[k]]$automatons)
  				for (p in 1:n_units)
  				{
    					total <- c(transcript[3]$engagements[[k]]$automatons[[p]][[2]], total)
  				}
			},
			"AutomatonType" = {
				n_units = length(transcript[3]$engagements[[k]]$automatons)
  				for (p in 1:n_units)
  				{
    					total <- c(transcript[3]$engagements[[k]]$automatons[[p]][[3]], total)
  				}
			},
			"AgentGroupID" = {
				n_units = length(transcript[3]$engagements[[k]]$agentGroups)
  				for (p in 1:n_units)
  				{
    					total <- c(transcript[3]$engagements[[k]]$agentGroups[[p]][[1]], total)
  				}
			},
			"AgentGroupName" = {
				n_units = length(transcript[3]$engagements[[k]]$agentGroups)
  				for (p in 1:n_units)
  				{
    					total <- c(transcript[3]$engagements[[k]]$agentGroups[[p]][[2]], total)
  				}
			},
			"AgentID" = {
				n_units = length(transcript[3]$engagements[[k]]$agents)
  				for (p in 1:n_units)
  				{
    					total <- c(transcript[3]$engagements[[k]]$agents[[p]][[1]], total)
  				}
			},
			"AgentFullName" = {
				n_units = length(transcript[3]$engagements[[k]]$agents)
  				for (p in 1:n_units)
  				{
    					total <- c(transcript[3]$engagements[[k]]$agents[[p]][[2]], total)
  				}
			},
			"AgentAlias" = {
				n_units = length(transcript[3]$engagements[[k]]$agents)
  				for (p in 1:n_units)
  				{
    					total <- c(transcript[3]$engagements[[k]]$agents[[p]][[3]], total)
  				}
			},
			"BusinessUnitID" = {
				n_units = length(transcript[3]$engagements[[k]]$businessUnits)
  				for (p in 1:n_units)
  				{
    					total <- c(transcript[3]$engagements[[k]]$businessUnits[[p]][[1]], total)
  				}
			},
			"BusinessUnitName" = {
				n_units = length(transcript[3]$engagements[[k]]$businessUnits)
  				for (p in 1:n_units)
  				{
    					total <- c(transcript[3]$engagements[[k]]$businessUnits[[p]][[2]], total)
  				}
			},
			"BusinessRuleID" = {
				total <- c(transcript[3]$engagements[[k]]$businessRuleID, total)
			},
			"BusinessRuleName" = {
				total <- c(transcript[3]$engagements[[k]]$businessRuleName, total)
			},
			"Device Type" = {
				total <- c(transcript[3]$engagements[[k]]$deviceType, total)
			},
			"Launch Type" = {
				total <- c(transcript[3]$engagements[[k]]$launchType, total)
			},
			"Sale Qualified" = {
				total <- c(transcript[3]$engagements[[k]]$saleQualified, total)
			},
			"Site ID" = {
				total <- c(transcript[3]$engagements[[k]]$siteID, total)
			}	
									

		)
  		trans = transcript[3]$engagements[[k]]$transcript;
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
					if (chat_type == customer_lost && trans[[j]]$content == lost_message )
						a[i] <- j
				}
				else if (status[i] == lost_navi)
				{
					if (chat_type == customer_lost && trans[[j]]$content == navi_message )
						a[i] <- j
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
			results <- c(transcript[3]$engagements[[k]]$browserType, results)
		},
		"Operating System" = {
			results <- c(transcript[3]$engagements[[k]]$operatingSystem, results)
		},
		"AutomatonID" = {
			n_units = length(transcript[3]$engagements[[k]]$automatons)
				for (p in 1:n_units)
				{
					results <- c(transcript[3]$engagements[[k]]$automatons[[p]][[1]], results)
				}
		},
		"AutomatonName" = {
			n_units = length(transcript[3]$engagements[[k]]$automatons)
				for (p in 1:n_units)
				{
					results <- c(transcript[3]$engagements[[k]]$automatons[[p]][[2]], results)
				}
		},
		"AutomatonType" = {
			n_units = length(transcript[3]$engagements[[k]]$automatons)
				for (p in 1:n_units)
				{
					results <- c(transcript[3]$engagements[[k]]$automatons[[p]][[3]], results)
				}
		},
		"AgentGroupID" = {
			n_units = length(transcript[3]$engagements[[k]]$agentGroups)
				for (p in 1:n_units)
				{
					results <- c(transcript[3]$engagements[[k]]$agentGroups[[p]][[1]], results)
				}
		},
		"AgentGroupName" = {
			n_units = length(transcript[3]$engagements[[k]]$agentGroups)
				for (p in 1:n_units)
				{
					results <- c(transcript[3]$engagements[[k]]$agentGroups[[p]][[2]], results)
				}
		},
		"AgentID" = {
			n_units = length(transcript[3]$engagements[[k]]$agents)
				for (p in 1:n_units)
				{
					results <- c(transcript[3]$engagements[[k]]$agents[[p]][[1]], results)
				}
		},
		"AgentFullName" = {
			n_units = length(transcript[3]$engagements[[k]]$agents)
				for (p in 1:n_units)
				{
					results <- c(transcript[3]$engagements[[k]]$agents[[p]][[2]], results)
				}
		},
		"AgentAlias" = {
			n_units = length(transcript[3]$engagements[[k]]$agents)
				for (p in 1:n_units)
				{
					results <- c(transcript[3]$engagements[[k]]$agents[[p]][[3]], results)
				}
		},
		"BusinessUnitID" = {
			n_units = length(transcript[3]$engagements[[k]]$businessUnits)
  			for (p in 1:n_units)
  			{
    				results <- c(transcript[3]$engagements[[k]]$businessUnits[[p]][[1]], results)
  			}
		},
		"BusinessUnitName" = {
			n_units = length(transcript[3]$engagements[[k]]$businessUnits)
  			for (p in 1:n_units)
  			{
    				results <- c(transcript[3]$engagements[[k]]$businessUnits[[p]][[2]], results)
  			}
		},
		"BusinessRuleID" = {
			results <- c(transcript[3]$engagements[[k]]$businessRuleID, results)
		},
		"BusinessRuleName" = {
			results <- c(transcript[3]$engagements[[k]]$businessRuleName, results)
		},
		"Device Type" = {
			results <- c(transcript[3]$engagements[[k]]$deviceType, results)
		},
		"Launch Type" = {
			results <- c(transcript[3]$engagements[[k]]$launchType, results)
		},
		"Sale Qualified" = {
			results <- c(transcript[3]$engagements[[k]]$saleQualified, results)
		},
		"Site ID" = {
			results <- c(transcript[3]$engagements[[k]]$siteID, results)
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
			pcts <- c(count/num_engagements, (num_engagements - count) / num_engagements)			
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
})

})

