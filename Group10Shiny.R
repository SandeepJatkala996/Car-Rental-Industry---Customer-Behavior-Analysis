#------------------------------------------------
# (C) Ahmet Ozkul - University of New Haven
# Spring 2023
# Car Rental Industry - Customer Behavior Analysis
#------------------------------------------------

library(shiny)
library(plotly)
library(forecast)
library(readxl)
library(tidyverse)
library(dplyr)

# Read your dataset first

library(readr)
my_data <- read_csv("CarRentalDataV1.csv")
View(my_data)

#Remove any NA and adjust the columns
na_count <- sum(is.na(my_data))

my_data[my_data == ""] <- NA
my_data_cleaned <- na.omit(my_data)

#Change to numeric factor
numeric_data <- my_data_cleaned[sapply(my_data_cleaned, is.numeric)]

#Remove Unnecessary Columns
my_data_filtered <- my_data_cleaned[my_data_cleaned$rating >= 3, ]
my_data_filtered <- my_data_filtered[, !(names(my_data_filtered) %in% c('location.country', 'location.latitude', 'location.longitude'))]

#Final check for any blank values and remove them
my_data_filtered <- my_data_filtered[!is.na(my_data_filtered$fuelType), ]

#Make a copy of dataset for prediction model
my_data_pred <- my_data_filtered
my_data_pred <- my_data_pred[, !(names(my_data_pred) %in% c('location.city','vehicle.model','vehicle.make','location.state','airportcity'))]


# Add rating Category Column
my_data_filtered$rating_category <- ifelse(my_data_filtered$rating >= 5, 'good', ifelse(my_data_filtered$rating >= 4.9, 'average', 
                                    ifelse(my_data_filtered$rating >= 4, 'bad', 'worst')))
my_data_filtered$rating_category <- factor(my_data_filtered$rating_category, levels = c('worst', 'bad', 'average', 'good'))

# The User Interface (UI) code is below:

ui <- fluidPage(
  titlePanel('Car Rental Industry - Customer Behavior Analysis') ,        

    mainPanel(
      tabsetPanel(
        tabPanel('METADATA' ,                               
                 h5("In this study, we investigate a Car Rental Dataset"),
                 h5("The data is publicly available on this link:"),
                 h5("https://www.kaggle.com/datasets/kushleshkumar/cornell-car-rental-dataset/data?select=CarRentalDataV1.csv"),
                 h5("The following list provides specifics on the columns in the dataset."),
                 h5("fuelType: Type of fuel used by car"),
                 h5("rating : Cumulative rating of the car by customers"),
                 h5("renterTripsTaken : No. of trips taken for this Car (duration unknown)"),
                 h5("reviewCount: No. of reviews"),
                 h5("location.city: City located "),
                 h5("location.state: State located "),
                 h5("owner.id: ID of the Owner"),
                 h5("rate.daily: Daily Rate in Dollars"),
                 h5("vehicle.make: Make of the Vehicle"),
                 h5("vehicle.model: Model of the Vehicle"),
                 h5("vehicle.type: Type of Vehicle"),
                 h5("vehicle.year: Year of the Model/Make"),
                 h5("airportcity: Nearest Airport City of the Vehicle "),
                 verbatimTextOutput('info1')),
        
        
        tabPanel('MODEL' ,                                 
                 h4("A Logistic regression model is proposed. The dataset is split as training (70%) and
                    validation (30%) randomly. Please select some predictors to create your own 
                    regression model."),
                 h4("The outcome variable is Rating Category"),
                 checkboxGroupInput("pred", "Which predictors do you like to include?", colnames(my_data_pred)),
                 h4("Regression Summary Report:"),
                 verbatimTextOutput('model1'),
                 h4("Accuracy measures on the validation partition:"),
                 verbatimTextOutput('model2')),

        tabPanel('DISCUSSION' ,                          # What is the problem and result
                 h5("In this study, we investigated a Car Rental Dataset"),
                 h5("The following model is developed based on the lowest RMSE score and higher Adj-Rsqrd on the validation partition."),
                 h5("Rating Category ~ rate.daily + vehicle.type + vehicle.year + renterTripsTaken") ,
                 h5("We excluded the following variables from the model because they are irrelevant."),
                 h5("location.city,vehicle.model,vehicle.make,location.state,airportcity"),
                 h5("The small p-values for most coefficients suggest that many predictors are statistically significant."),
                 h5("The null deviance and residual deviance being close indicate that the model explains a substantial amount of variability."),
                 h5("The logistic regression model reveals insights into the dataset,
                    highlighting a positive coefficient for 'renterTripsTaken' (0.1324) suggests that as the number of renter trips taken increases by one unit, 
                    the log-odds of the event happening (success) also increase by the estimated coefficient.
                    The Lower AIC suggests that the model fits the data well relative to its complexity.
                    This model can offer valuable predictions, 
                    aiding decision-making in scenarios where understanding the impact of renter trips, vehicle year, 
                    and daily rate is essential."),
  
        ),

tabPanel('VISUALIZATIONS',  
         plotOutput('plot_q1'),
         h5("#Gasoline fuel type vehicles have been opted more by the customers as per our visual observation. But if we check the vehicle type, Electric car is also opted more compared to others."),
         plotOutput('plot_q2'),
         h5("#We can observe that there is no correlation between the ratings and rental trips. It displays a dispersed arrangement between the variables which suggests a very weak negative correlation."),
         plotOutput('plot_q3'),
         h5("As per the graph, the electric fuel type average daily rate is higher followed by diesel, hybrid and gasoline fuel type vehicles. It helps in analyzing the pricing structures"),
         plotOutput('plot_q4'),
         h5("As per the graph, the hybrid type vehicles have higher mean rate compared to other fuel type vehicles. "),
         plotOutput('plot_q5'),
         h5("There is higher range in pricing for the regions Alabama, New Jersey and Kansas. A higher average daily rate indicated higher demand in the region which helps in maximizing the business in those particular geographical areas"),
),
        
        tabPanel('ABOUT' ,                                     # Info about team members
                 h2("Car Rental Industry - Customer Behavior Analysis"),
                 h3("A Capstone Project by Nithya Chowdary Kandala, Sandeep Jatkala and Hadassah Sanjana Mikkili "),
                 h3("University of New Haven - Fall 2023")
        )
        
      )
    )

)

# The server code is below:

server <- function(input, output, session) {
  
    
    output$model1 <- renderPrint({          
      predictors <- input$pred
      
      # Check if any predictors are selected
      if (length(predictors) == 0) {
        return("Please select at least one predictor.")
      }
      
      formula <- as.formula(paste("rating_category ~", paste(predictors, collapse = "+")))
      logistic_model <- glm(formula, data = my_data_filtered, family = "binomial")
      summary(logistic_model)
    })
    
    output$model2 <- renderPrint({         
      predictors <- input$pred
      
      # Check if any predictors are selected
      if (length(predictors) == 0) {
        return("Please select at least one predictor.")
      }
      
      formula <- as.formula(paste("rating_category ~", paste(predictors, collapse = "+")))
      logistic_model <- glm(formula, data = my_data_filtered, family = "binomial")
      
      # Check if the model was successfully fitted
      if (exists("logistic_model")) {
        predicted_probs <- predict(logistic_model, newdata = validation_data, type = "response")
        predicted_category <- ifelse(predicted_probs < 0.8, 'bad', ifelse(predicted_probs < 0.9, 'average', 'good'))
        accuracy <- mean(predicted_category == validation_data$rating_category)
        paste("Accuracy:", round(accuracy, 4))
      } else {
        "Error: Failed to fit the logistic regression model."
      }
    })
    
  
    # Visualizations
    output$plot_q1 <- renderPlot({
      # Q1: Bar plot comparing the count or percentage of electric and non-electric vehicles
      ggplot(my_data_filtered, aes(x = vehicle.type, fill = fuelType)) +
        geom_bar(position = "dodge") +
        labs(title = "Vehicle Adoption by Fuel type",
             x = "Vehicle Type",
             y = "Count") +
        scale_fill_manual(values = c("ELECTRIC" = "blue", "GASOLINE" = "red", "HYBRID" = "green")) +
        theme_minimal()
    })
    
    output$plot_q2 <- renderPlot({
      # Q2: Scatter plot of Rental Trips and Ratings
      ggplot(my_data_filtered, aes(x = rating, y = renterTripsTaken)) +
        geom_point() +
        labs(title = "Scatter Plot of Rental Trips and Ratings",
             x = "Rating",
             y = "Renter Trips Taken") +
        theme_minimal()
    })
    
    
    output$plot_q3 <- renderPlot({
      # Q4: Bar plot with mean Renter Trips Taken based on Fuel Type
      mean_renter_trips <- my_data_filtered %>%
        group_by(fuelType) %>%
        summarize(mean_renterTripsTaken = mean(renterTripsTaken))
      
      ggplot(mean_renter_trips, aes(x = fuelType, y = mean_renterTripsTaken, fill = fuelType)) +
        geom_col(position = "dodge", color = "black") +
        labs(title = "Mean Renter Trips Taken Based on Fuel Type",
             x = "Fuel Type",
             y = "Mean Renter Trips Taken") +
        geom_text(aes(label = sprintf("%.2f", after_stat(y))),
                  position = position_dodge(width = 0.7), vjust = -0.5) +
        theme_minimal()
    })
    
    output$plot_q4 <- renderPlot({
      # Q5: Bar plot with avg dailyrate based on fuelType
      mean_daily_rate <- my_data_filtered %>%
        group_by(fuelType) %>%
        summarize(avg_daily_rate = mean(rate.daily, na.rm = TRUE))
      
      ggplot(mean_daily_rate, aes(x = fuelType, y = avg_daily_rate, fill = fuelType)) +
        geom_bar(stat = "identity", position = "dodge", color = "black") +
        labs(title = "Average Daily Rate Based on Fuel Type",
             x = "Fuel Type",
             y = "Average Daily Rate") +
        geom_text(aes(label = sprintf("%.2f", after_stat(y))),
                  position = position_dodge(width = 0.7), vjust = -0.5) +
        theme_minimal()
    })
    
    output$plot_q5 <- renderPlot({
      # Q6: Boxplot of Daily Rate vs. Location State
      ggplot(my_data_filtered, aes(x = location.state, y = rate.daily, fill = location.state)) +
        geom_boxplot() +
        labs(title = "Boxplot of Daily Rate vs. Location State",
             x = "Location State",
             y = "Daily Rate") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 90, hjust = 1))
    })
    
  }
  
  shinyApp(ui, server)
  
