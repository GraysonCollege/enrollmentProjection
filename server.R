# name: shiny app for projections
# author: Anderson Zhu
# email: zhua@grayson.edu

options(warn=-1)
list_of_packages <- c("ggplot2", "dplyr", "shiny", "rmarkdown")

for (p in list_of_packages){
    if (!require(p, character.only = T)) {
        cat("Installing required packages!")
        install.packages(p, dependencies = T, quiet = T, verbose = F)
        require(p, character.only = T, quietly = T)
    } else {
        require(p, character.only = T, quietly = T)
    }
    cat("Done installing all the required packages.")
}




options(shiny.maxRequestSize = 9*1024^2)

myTheme <- theme(panel.grid.major=element_blank(),
                 panel.grid.minor=element_blank(),
                 panel.grid = element_blank(),   
                 panel.border = element_rect(fill = NA, colour = "black", size=1),
                 panel.background = element_rect(fill = "white", colour = "black"), 
                 strip.background = element_rect(fill = NA),
                 axis.text.x = element_text(angle = 45, hjust = 1))

# function to calculate the PMA
pma <- function(actual, pred, pred_var, shift){
    # the output data will only contain 3 variables
    # year, semesterFall, projection variable
    n = nrow(actual)
    m = nrow(pred)
    
    var_name = paste0("projection_PMA_", shift, "yr")
    
    actual[[var_name]] = NA
    pred[[pred_var]] = pred[[var_name]] = NA
    
    dat = rbind(actual, pred)
    for(i in (n+1):(n+m)){
        dat[[pred_var]][i] = mean(dat[[pred_var]][c((i-shift):(i-1))])
    }
    
    for (i in 1:(n+m)){
        if (i <= shift){
            dat[[var_name]][i] = dat[[pred_var]][i]
        } else {
            dat[[var_name]][i] = mean(dat[[pred_var]][c((i-shift):(i-1))])
        }
    }
    
    vars_to_keep = c("year", "semesterFall", var_name)
    actual = dat[1:n, vars_to_keep]
    pred = dat[(n+1):(n+m),vars_to_keep] 
    return(list(actual = actual, pred = pred))
}




shinyServer( function(input, output) {

#----------Generate desired data----------#
    grabData <- reactive({
        
        #--------read enrollment data into shiny--------#
        enrollment_data = read.csv("data/enrollment_data.csv", stringsAsFactors = F)

        # create desired variables
        enrollment_data <- enrollment_data %>% 
            rowwise() %>%
            mutate(year = as.integer(unlist(strsplit(`Semester`, split = " "))[1]), 
                   semester = as.character(unlist(strsplit(`Semester`, split = " "))[2]),
                   enrollment = `Total`) %>%
            arrange(year, desc(semester)) 
        
        # filter the desired years
        enrollment_data = enrollment_data %>%
            filter(year >= input$years[1] & year <= input$years[2]) #%>% 
            # filter(semester %in% input$semester) 
        
        
        #--------read unemployment data into shiny--------#
        unemployment_data = read.csv("data/unemployment_rate_last_year.csv", stringsAsFactors = F)
        
        #--------merge two datasets--------#
        desiredData = left_join(enrollment_data, unemployment_data, by = "year") %>% 
            transform(credits = Credits, hours = Hours) %>%
            mutate(semesterFall = ifelse(semester == "Fall", 1, 0)) %>%
            select(year, semesterFall, unemployment_rate_last_year, enrollment, credits, hours)
        
        #--------create new data for prediction--------#
        # create empty data frame having year and semesterFall columns
        # check the last semester
        if (desiredData[nrow(desiredData), 'semesterFall'] == 1){
            # if it is Fall semester, we only predict spring and fall next year
            predData = as.data.frame(matrix(NA, nrow = 2, ncol = 2))
            names(predData) = c("year", "semesterFall")
            mx_yr = max(desiredData$year)
            predData[,'year'] = rep(mx_yr+1,2)
            predData[,'semesterFall'] = c(0,1)
        } else {
            # if it is Spring semester, we will predict Fall this year, Spring and Fall next year
            predData = as.data.frame(matrix(NA, nrow = 3, ncol = 2))
            names(predData) = c("year", "semesterFall")
            mx_yr = max(desiredData$year)
            predData[,'year'] = c(mx_yr, rep(mx_yr+1,2))
            predData[,'semesterFall'] = c(1,0,1)
        }
        
        # merge new data with unemployment rate last year
        predData = left_join(predData, unemployment_data, by = "year")
        
        return(list(desiredData = desiredData, predData = predData))
    }) # close reactive data
    
#--------------------------------------------------#   
#-----------Generate projection plot---------------#   
#--------------------------------------------------#   
    output$plot <- renderPlot({

        # obtain desired and preed data
        desired_dat = grabData()$desiredData
        pred_dat = grabData()$predData
        
        cols = c("0" = "blue", "1" = "red")
        
        # choose prediction variable

        if (input$target == "Enrollment"){
            pred_var = "enrollment"
            lo = 1000
            hi = 6000
        } else if (input$target == "Attempted Credits") {
            pred_var = "credits"
            lo = 10000
            hi = 65000
        } else{
            pred_var = "hours"
            lo = 400000
            hi = 1350000
        }
        

        if (input$model == "Basic Linear Regression"){
            formula = paste0(pred_var, " ~ year + unemployment_rate_last_year + semesterFall")
            
            fit = lm(formula, data = desired_dat)
            
            # make predictions on desired data to see the performance
            desired_dat$prejection_LR = fit$fitted.values
            
            # make predictions for the following semesters
            pred_dat$prejection_LR = predict.lm(fit, newdata = pred_dat) 
            
            # merger two data sets
            merge_dat = rbind(desired_dat %>% select(year, semesterFall, unemployment_rate_last_year, prejection_LR), 
                              pred_dat)
            
            # making plots again
            plt = ggplot(aes(x = year, y = prejection_LR), data = merge_dat) +
                geom_point(aes(shape = as.factor(semesterFall))) + 
                scale_x_continuous(breaks = seq(min(merge_dat$year), max(merge_dat$year), 1),
                                   labels = seq(min(merge_dat$year), max(merge_dat$year), 1)) + 
                scale_shape_manual(name = "Semester\n Projections",
                                   values = c("0" = 0, "1" = 2),
                                   breaks = c("0", "1"),
                                   labels = c("Spring", "Fall")) + 
                geom_point(aes(x = year, y = get(pred_var), color = as.factor(semesterFall)), size = 2, data = desired_dat) + 
                scale_color_manual(name = "Semester",
                                   values = c("0" = "red", "1" = "blue"),
                                   breaks = c("0", "1"),
                                   labels = c("Spring", "Fall")) +
                xlab("Year") + 
                ylab(input$target) + 
                ylim(c(lo, hi)) + 
                myTheme
            print(plt)
        } else if(input$model == "Prior Moving Average (3yrs)"){
            # average 3 previous projection data
            # projection separatedly
            shift = 3
            
            # projection for Spring
            actual_spring = desired_dat %>% filter(semesterFall == 0) %>% 
                select(year, semesterFall, contains(pred_var))
            pred_spring = pred_dat %>% filter(semesterFall == 0) %>%
                select(year, semesterFall)
                        
            actual_s = pma(actual_spring, pred_spring, pred_var, shift)[[1]] 
            pred_s = pma(actual_spring, pred_spring, pred_var, shift)[[2]] # only return 3 variables
            
            # projection for Fall 
            actual_fall = desired_dat %>% filter(semesterFall == 1) %>% 
                select(year, semesterFall, contains(pred_var))
            pred_fall = pred_dat %>% filter(semesterFall == 1) %>%
                select(year, semesterFall)
            
            actual_f = pma(actual_fall, pred_fall, pred_var, shift)[[1]] 
            pred_f = pma(actual_fall, pred_fall, pred_var, shift)[[2]] # only return 3 variables
            
            
            actual = rbind(actual_s, actual_f)
            preds = rbind(pred_s, pred_f)
            
            # merger two data sets
            merge_dat = rbind(actual, preds)
            
            # making plots again
            plt = ggplot(aes(x = year, y = projection_PMA_3yr), data = merge_dat) +
                geom_point(aes(shape = as.factor(semesterFall))) + 
                scale_x_continuous(breaks = seq(min(merge_dat$year), max(merge_dat$year), 1),
                                   labels = seq(min(merge_dat$year), max(merge_dat$year), 1)) + 
                scale_shape_manual(name = "Semester\n Projections",
                                   values = c("0" = 0, "1" = 2),
                                   breaks = c("0", "1"),
                                   labels = c("Spring", "Fall")) + 
                geom_point(aes(x = year, y = get(pred_var), color = as.factor(semesterFall)), size = 2, data = desired_dat) + 
                scale_color_manual(name = "Semester",
                                   values = c("0" = "red", "1" = "blue"),
                                   breaks = c("0", "1"),
                                   labels = c("Spring", "Fall")) +
                xlab("Year") + 
                ylab(input$target) + 
                ylim(c(lo, hi)) + 
                myTheme
            print(plt)
            
        } else if(input$model == "Prior Moving Average (5yrs)"){
            # average 5 previous projection data
            shift = 5
            
            # projection for Spring
            actual_spring = desired_dat %>% filter(semesterFall == 0) %>% 
                select(year, semesterFall, contains(pred_var))
            pred_spring = pred_dat %>% filter(semesterFall == 0) %>%
                select(year, semesterFall)
            
            actual_s = pma(actual_spring, pred_spring, pred_var, shift)[[1]] 
            pred_s = pma(actual_spring, pred_spring, pred_var, shift)[[2]] # only return 3 variables
            
            # projection for Fall 
            actual_fall = desired_dat %>% filter(semesterFall == 1) %>% 
                select(year, semesterFall, contains(pred_var))
            pred_fall = pred_dat %>% filter(semesterFall == 1) %>%
                select(year, semesterFall)
            
            actual_f = pma(actual_fall, pred_fall, pred_var, shift)[[1]] 
            pred_f = pma(actual_fall, pred_fall, pred_var, shift)[[2]] # only return 3 variables
            
            
            actual = rbind(actual_s, actual_f)
            preds = rbind(pred_s, pred_f)
            
            # merger two data sets
            merge_dat = rbind(actual, preds)
            
            # making plots again
            plt = ggplot(aes(x = year, y = projection_PMA_5yr), data = merge_dat) +
                geom_point(aes(shape = as.factor(semesterFall))) + 
                scale_x_continuous(breaks = seq(min(merge_dat$year), max(merge_dat$year), 1),
                                   labels = seq(min(merge_dat$year), max(merge_dat$year), 1)) + 
                scale_shape_manual(name = "Semester\n Projections",
                                   values = c("0" = 0, "1" = 2),
                                   breaks = c("0", "1"),
                                   labels = c("Spring", "Fall")) + 
                geom_point(aes(x = year, y = get(pred_var), color = as.factor(semesterFall)), size = 2, data = desired_dat) + 
                scale_color_manual(name = "Semester",
                                   values = c("0" = "red", "1" = "blue"),
                                   breaks = c("0", "1"),
                                   labels = c("Spring", "Fall")) +
                xlab("Year") + 
                ylab(input$target) + 
                ylim(c(lo, hi)) + 
                myTheme
            print(plt)
        }
    }) # close renderPlot
    
#---------------------------------------------#   
#---------Generate projection tables----------#
#---------------------------------------------#   
    output$contents = renderDataTable({
        
        # obtain desired and preed data
        desired_dat = grabData()$desiredData
        pred_dat = grabData()$predData
        
        # choose prediction variable
        
        if (input$target == "Enrollment"){
            pred_var = "enrollment"
            lo = 1000
            hi = 6000
        } else if (input$target == "Attempted Credits") {
            pred_var = "credits"
            lo = 10000
            hi = 65000
        } else{
            pred_var = "hours"
            lo = 500000
            hi = 1500000
        }
        
        if (input$model == "Basic Linear Regression"){
            formula = paste0(pred_var, " ~ year + unemployment_rate_last_year + semesterFall")
            
            fit = lm(formula, data = desired_dat)
            
            # make predictions on desired data to see the performance
            desired_dat$prejection_LR = fit$fitted.values
            
            # make predictions for the following semesters
            pred_dat$prejection_LR = predict.lm(fit, newdata = pred_dat) 
            
            # merger two data sets
            merge_dat = rbind(desired_dat %>% select(year, semesterFall, unemployment_rate_last_year, prejection_LR), 
                              pred_dat)
            
            # print the projections
            if (input$showData){
                print(desired_dat)
            } else {
                print(pred_dat %>% select(-unemployment_rate_last_year))
            }
            
            
        } else if(input$model == "Prior Moving Average (3yrs)"){
            # average 3 previous projection data
            shift = 3
            
            # projection for Spring
            actual_spring = desired_dat %>% filter(semesterFall == 0) %>% 
                select(year, semesterFall, contains(pred_var))
            pred_spring = pred_dat %>% filter(semesterFall == 0) %>%
                select(year, semesterFall)
            
            actual_s = pma(actual_spring, pred_spring, pred_var, shift)[[1]] 
            pred_s = pma(actual_spring, pred_spring, pred_var, shift)[[2]] # only return 3 variables
            
            # projection for Fall 
            actual_fall = desired_dat %>% filter(semesterFall == 1) %>% 
                select(year, semesterFall, contains(pred_var))
            pred_fall = pred_dat %>% filter(semesterFall == 1) %>%
                select(year, semesterFall)
            
            actual_f = pma(actual_fall, pred_fall, pred_var, shift)[[1]] 
            pred_f = pma(actual_fall, pred_fall, pred_var, shift)[[2]] # only return 3 variables
            
            
            actual = rbind(actual_s, actual_f)
            preds = rbind(pred_s, pred_f)
            
            # print the projections
            if (input$showData){
                print(actual) %>% arrange(year, semesterFall)
            } else {
                print(preds)
            }
            
        } else if(input$model == "Prior Moving Average (5yrs)"){
            # average 5 previous projection data
            shift = 5
            
            # projection for Spring
            actual_spring = desired_dat %>% filter(semesterFall == 0) %>% 
                select(year, semesterFall, contains(pred_var))
            pred_spring = pred_dat %>% filter(semesterFall == 0) %>%
                select(year, semesterFall)
            
            actual_s = pma(actual_spring, pred_spring, pred_var, shift)[[1]] 
            pred_s = pma(actual_spring, pred_spring, pred_var, shift)[[2]] # only return 3 variables
            
            # projection for Fall 
            actual_fall = desired_dat %>% filter(semesterFall == 1) %>% 
                select(year, semesterFall, contains(pred_var))
            pred_fall = pred_dat %>% filter(semesterFall == 1) %>%
                select(year, semesterFall)
            
            actual_f = pma(actual_fall, pred_fall, pred_var, shift)[[1]] 
            pred_f = pma(actual_fall, pred_fall, pred_var, shift)[[2]] # only return 3 variables
            
            
            actual = rbind(actual_s, actual_f)
            preds = rbind(pred_s, pred_f)
            
            # print the projections
            if (input$showData){
                print(actual) %>% arrange(year, semesterFall)
            } else {
                print(preds)
            }
        }
        
    }, options = list(lengthMenu = c(5, 30, 50), pageLength = 10)) # close renderTable
    
#-----------------------------------------------#   
#---------Generate downloadable report----------#
#-----------------------------------------------# 
    output$downloadReport <- downloadHandler(
        filename = function() {
            paste('projection-report', sep = '.', switch(
                input$format, PDF = 'pdf', HTML = 'html', Word = 'docx'
            ))
        },
        
        content = function(file) {
            src <- normalizePath('report.Rmd')
            
            current_path = normalizePath(getwd())
            # temporarily switch to the temp dir, in case you do not have write
            # permission to the current working directory
            # owd <- setwd(tempdir())
            # on.exit(setwd(owd))
            # file.copy(src, 'report.Rmd', overwrite = TRUE)

            # abs_file_name = paste(normalizePath(getwd()),
            #                       paste('projection-report', sep = '.', switch(
            #                           input$format, PDF = 'pdf', HTML = 'html', Word = 'docx'
            #                       )), sep = "\\")
            # setwd(current_path)
            out <- render('report.Rmd', switch(
                input$format,
                PDF = pdf_document(), HTML = html_document(), Word = word_document()
            ), output_dir = current_path)
            file.rename(out, file)
        }
    )
    
}) # close shinyServer