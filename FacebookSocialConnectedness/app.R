#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Load in the necessary libraries.

library(shiny)
library(tidyverse)
library(usmap)
library(shinythemes)
library(ggcorrplot)
library("htmltools")
library("vembedr")

# Load in the RDS data from the globals folder.

county_detailed_shiny <- readRDS("globals/county_detailed")
correlations_shiny <- readRDS("globals/correlations")
closeness_county_shiny <- readRDS("globals/closeness_county")


# Define UI for application

ui <- fluidPage(
    
    #Set the aesthetic theme for the shinyapp.
    
    theme = shinytheme("lumen"),
    
    navbarPage(tags$b("Social Connectedness in America"),
               
               # Create the first tab, which will be the first page that people will see when they open up the ShinyApp.
               
               tabPanel("Featured Findings",
                        
                        # Load in the first image for the front page.

                        imageOutput("image", width = "100%", height = "100%"),
                        
                        # Title and subtitle
                        
                        h1(tags$b("Social Connectedness in America"), align = "center"),
                        p(tags$em("Analysis of Facebook's Social Connectedness Index of every American county"), align = "center"),
                        
                        # Include an arrow on the main page so people know to scroll down and read the analysis/graphs below.
                        
                        imageOutput("arrow", width = "100%", height = "100%"),
                        br(), 
                        
                        # Add in a fluidRow to center the main text and graphs on the first page.
                        
                        fluidRow(column(2), column(8, 
                                                   
                        # Add in text to introduce and explain the project.
                        
                        p("In February of 2004, Mark Zuckerberg launched Facebook from his college dorm in Kirkland House. 
                           Today -- a decade and a half later -- that website has become one of the most popular in the world
                           and connects nearly two and a half billion people. It has become the digital representation of people's 
                           social circles; the average person's Facebook friends include their family, friends, coworkers, former
                           classmates, and more. This digital representation of people's natural social networks gives a unique
                           opportunity to analyze the way that people interact with other people in society and who those 
                           people are, as the composition of your social circle can greatly influence the experiences you have,
                           the ideas you encounter, the opportunities you have, and more."),
                        p("Understanding how social circles form in our society and the relative social connectedness of peoples
                           is important because it helps us understand the lives and divides in the ways life is experienced in our
                           society. Why are some people so deeply woven into the social fabric as others are marginalized, excluded, 
                           and isolated from it? Are differences in social connectedness based in politics? Socioeconomic status?
                           Race? Geography? How do the nodes of the American people connect?"),
                        p("To analyze these networks of social connectivity, it would be extremely helpful to have a metric for
                           the connectedness of people across the US. And for better or for worse, Facebook has been tracking 
                           exactly this as they've accumulated the digital representation of our social networks through our 
                           Facebook friends. One specific metric, called the Social Connectedness Index, gives the relative
                           probability that a person in County A is socially connected (i.e. Facebook friends) with a person in
                           County B."),
                        p("In September, I became aware of the Social Connectedness Index from an article in the New York 
                           Times. I emailed Facebook's Data Research Department asking if there was any way I could study / research
                           the data for a class project (Gov1005). And to my surprise, they responded the next day saying that
                           I'd need to sign a waiver but then would be able to have access to the Social Connectedness Index
                           dataset. The data was initially broken down by county, and I also aggregated it up another level
                           to look at associations by state."),
                        p("So which states are the most socially connected? Well it turns out it might not be the states you 
                          expect. In first place is Washington DC. After that the top of the list is overwhelmingly Western 
                          states (like Wyoming, Montana, Colorado, and the Dakotas). This means that the average American is 
                          (as a whole) relatively more likely to be Facebook friends with someone from these states. Also with 
                          high Social Connectedness Index scores: Arizona and Florida. One possible explanation for this is 
                          the gradual migration of elderly Amercians from their home states to these warmer-weather states. At
                          the bottom of the list are overwhelmingly Rust Belt (Ohio, Michigan, Pennsylvania) and Southern 
                          states (Mississippi, Louisiana, Alabama)."), br(),
                        
                        # Include the dotplot of overall social connectedness
                        
                        plotOutput("dotplot", width = "100%", height = "100%"), br(),
                        p("I combined the Social Connectedness Index with demographic data from the American Economic Association, 
                           including racial composition, median income, population, education, and partisan lean. This would give
                           me the opportunity to analyze those questions of how social connectedness and demographic divides
                           are related. After running a correlogram on this combined data, no strong correlations between demographics
                          and social connectedness emerge for the national average (although you can explore individual states'
                          correlations in the Demographic Correlations tab). Other interesting demographic correlations appear,
                          like that income is correlated positively with partisan lean and negatively with the share of industry
                          that is manufacturing or that median age is correlated positively with the county's percentage of the 
                          population that is white (meaning the younger-average counties are also more racially diverse)."), br(),
                        
                        # Include the correlogram plot
                        
                        plotOutput("correlations", width = "100%", height = "100%"), br(),
                        p("We can fit a linear model based on this demographic data to see how well it predicts the social 
                          connectedness between two counties. Overall, the demographic data does not explain particularly well
                          the level of social connectedness -- meaning that social connectedness is overall much more determined by
                          something other than demographic similarity. The plot below shows the relationship between the regression 
                          model's predicted values and the true values of social connectedness -- overall not a particularly strong fit."), br(),
                        
                        # Include the linear model fit graph
                        
                        plotOutput("model", width = "100%", height = "100%"), br(),
                        p("Perhaps one of the most explanatory variables is just the geographic closeness of two places. Counties
                          and states that are near each other are much more likely to be socially connected. With this
                          comes the demographic similarity, so it may be that since counties and states next to each other are demographically
                          similar they tend to be more likely to be connected to demographically similar people but not all people
                          across the nation who are demographically similar. This suggests that social connectedness is similar to the 
                          concept of a gravity model in economics (where nations that are near each other are more likely to trade goods). 
                          The histogram shows just how important geographic closeness is by showing the average share of connections
                          that are within 50 miles (roughly 60+%)."), br(),
                        
                        # Include histogram of share of friends within 50 miles
                        
                        plotOutput("closeness", width = "100%", height = "100%")), column(2)), br(),
                        p(tags$b(tags$em("Explore the rest of the data by looking at the state map of relative connectedness, the demographic
                          correlations for specific states, and the importance of geographic closeness in each state by checking
                          out the other tabs.")), align = "center")
               ),
               
               # Second tab of interactive map of social connections by state.
               
               tabPanel("State Map",
                        sidebarLayout(
                            sidebarPanel(
                                
                                # Set the favicon and wording for tab header overall
                                
                                HTML('<script> document.title = "Social Connectedness in America"; </script>'),
                                tags$head(tags$link(rel="shortcut icon", href="https://vignette.wikia.nocookie.net/marvelcinematicuniverse/images/9/9c/Harvard_shield_wreath.png/revision/latest?cb=20190302143211")),
                                
                                # Add in short description about what this plot is.
                                
                                p(tags$em("What is the relative probability that people from two states are Facebook friends? 
                                          Explore the geographic distribution of social connectedness in America by selecting
                                          a state and seeing which states they are most likely to know other people from.")),
                                
                                # Make plotUSmap interactive by allowing people to select their state using a select input
                                
                                selectInput("mapstate", tags$b("Choose a state:"),
                                            choices = " "
                                )
                            ),
                            mainPanel(
                                
                                # Load in the plot.
                                
                                plotOutput("plot")
                            )
                        )
               ),
               
               # Third tab on demographic correlations by state
               
               tabPanel("Demographic Correlations",
                        sidebarLayout(
                            sidebarPanel(
                                
                                # Short description of the plot
                                
                                p(tags$em("Are people more likely to be friends with people in states that are demographically
                                          similar? Explore the political, economic, racial, and cultural diversity (or lack 
                                          thereof) in American's social connections using the relationship between the Facebook
                                          Social Connectedness Index and county demographic information.")),
                                
                                # Add in first select input to pick state
                                
                                selectInput("state", tags$b("Choose a state:"),
                                            choices = " "
                                ),
                                
                                # Add in second select input to pick the demographic characteristic with set choice options
                                
                                selectInput("demographics", tags$b("Choose a demographic characteristic:"),
                                            choices = c("Share without a High School Degree" = "no_highschool_share", 
                                                        "Median Age" = "median_age",
                                                        "Racial Diversity" = "pct_white_alone",
                                                        "Median Household Income" = "median_hh_income",
                                                        "Poverty Rate" = "share_below_povline",
                                                        "Manufacturing Industry" = "manufacturing_ind_share",
                                                        "Partisan Lean" = "obama_share_vs_mccain")
                                )
                            ),
                            mainPanel(
                                
                                # Load in the plot
                                
                                plotOutput("distPlot")
                            )
                        )
               ),
               
               # Fourth panel of the importance of geographic closeness, facet wrapped by state.
               
               tabPanel("Geographic Closeness",
                        sidebarLayout(
                            sidebarPanel(
                                p(tags$em("The importance of geography in determining who your friends are seems to vary from
                                          state to state -- compare the share of social connections within 50 miles by state.")),
                                p(tags$em("The further to the right a distribution is, the more counties that have geographically small
                                          social circles. The further to the left, the more geographically dispersed the social
                                          connections.")),
                                p(tags$em("This graph may take a few seconds to load as it has all 50 states + DC."))
                            ),
                            mainPanel(
                                
                                # This plot takes a few seconds (like 20 to 30) to load
                                
                                plotOutput("closenessfacet", width = "100%", height = "100%")
                            )
                        )
               ),
               
               # Fifth panel with about and contact information
               
               tabPanel("About this Project",
                        fluidRow(column(3), column(6, 
                            h1("Video Description of Key Findings", align = "center"), 
                            column(3))),
                        fluidRow(column(4), column(4, 
                                  embed_url("https://www.youtube.com/watch?v=uAICC3mt0ZY&feature=youtu.be"),
                            column(4))),
                        fluidRow(column(3), column(6,     
                                  h1("Contact and Info", align = "center"),
                                  p("Project by Bridger Gordon, Harvard Class of 2022, for Gov1005: Data final project", align = "center"),
                                  p("Email: bridgergordon@college.harvard.edu", align = "center"),
                                  p(tags$a(href = "https://github.com/BridgerGordon", "GitHub: BridgerGordon"), align = "center"),
                                  p(tags$a(href = "https://www.linkedin.com/in/bridger-gordon-a20a9b161/", "LinkedIn: Bridger Gordon"), align = "center")), 
                            column(3))
               )
    )
    
)

# Define server logic
server <- function(input, output, session) {
    
    # Create the demographic state plot
    
    output$distPlot <- renderPlot({
        
        # For select input, leave blank if no option selected
        
        if (input$state == " ") {
            return()
        }
        
        # Filter the data based on the state that is selected
        
        filtered <- county_detailed_shiny %>%
            filter(own_state == input$state) %>%
            mutate(fips = friend_state_fips) 
        
        # Format the title based on the demographic choice picked
        
        new_title <- if(input$demographics == "median_age"){
            print("Median Age")
        } else if(input$demographics == "pct_white_alone"){
            print("State's Percentage of Population that is White")
        } else if(input$demographics == "no_highschool_share"){
            print("State's Share Without High School Degree")
        } else if(input$demographics == "median_hh_income"){
            print("Median Household Income")
        } else if(input$demographics == "share_below_povline"){
            print("State's Share Below Poverty Rate")
        } else if(input$demographics == "manufacturing_ind_share"){
            print("State's Manufacturing Industry Share")
        } else if(input$demographics == "obama_share_vs_mccain"){
            print("Obama 2012 Vote Share")
        } 
        
        # Create a ggplot scatterplot with a linear regression line that shows the association between social connectedness and demographics.
        
        ggplot(filtered, aes_string(x = input$demographics, y = "log_prob")) +
            geom_point() +
            geom_smooth(method = lm, se = FALSE) +
            labs(title = paste("Relationship between ", input$state, "'s Social Connectedness Index and \n ", new_title, sep = ""),
                 x = new_title,
                 y = "Relative Probability of Social Connectedness")  + 
            theme(plot.title = element_text(face = "bold.italic", hjust = 0.5, size = 20))
    },
    
    # Set the best height for aesthetic purposes
    
    height = 580
    )
    
    # Create the US map plot
    
    output$plot <- renderPlot({
        
        # Return blank if no choice selected yet
        
        if (input$mapstate == " ") {
            return()
        }
        
        # Filter the dataset based on what state was selected
        
        test <- county_detailed_shiny %>%
            filter(own_state == input$mapstate) %>%
            mutate(fips = friend_state_fips)
        
        # Create the US map by state to show social connectedness.
        
        plot_usmap(data = test, values = "log_prob", regions = "states", size = 0.05) + 
            theme(panel.background = element_rect(color = "white", fill = "white")) +
            scale_fill_continuous(low = "white", high = "blue4", name = "Rel. Prob.") +
            labs(title = paste("Map of Facebook's Social Connectedness Index for ", input$mapstate, sep = "")) + 
            theme(plot.title = element_text(face = "bold.italic", hjust = 0.5, size = 20))
    },
    
    # Set the best height and width for aesthetic purposes
    
    height = 600,
    width = 900
    )
    
    # Create the correlogram
    
    output$correlations <- renderPlot({
        
        # Calculate the correlation matrix
        
        corr <- round(cor(correlations_shiny), 1)
        
        # Put it in a ggcorrplot with formatting to show the correlation numbers and theme / aesthetics

        ggcorrplot(corr, hc.order = TRUE, 
                   type = "lower", 
                   insig = "blank",
                   colors = c("blue", "white", "red"),
                   lab = TRUE,
                   lab_size = 5,
                   lab_col = "white",
                   ggtheme=theme_minimal) +
            labs(title = "Correlations Between Demographics and Social Connectedness")  + 
            theme(plot.title = element_text(face = "bold.italic", hjust = 0.5, size = 20),
                  legend.position = "none")
        
    }, 
    
    # Set the best height and width for aesthetic purposes
    
    height = 600,
    width = 900
    )
    
    # Create a dotplot that shows the level of social connectedness with the error bars
    
    output$dotplot <- renderPlot({
        
        county_detailed_shiny %>%
            ungroup() %>%
            group_by(friend_state) %>%
            summarize(mean = mean(log_prob),
                      sdev = sd(log_prob)) %>%
            ggplot(aes(x = reorder(friend_state, +mean), y = mean)) +
            
            # add in the error bars using the standard deviation
            
            geom_pointrange(aes(ymin = mean - sdev, ymax = mean + sdev)) +
            coord_flip() +
            scale_y_continuous(limits = c(3,7)) +
            theme_light() +
            labs(title = "What States are the Most Socially Connected to Other States",
                 y = "Log Relative Probability of Social Connectedness",
                 x = " ") + 
            theme(plot.title = element_text(face = "bold.italic", hjust = 0.5, size = 20))
        
    }, 
    
    # Set the best height and width for aesthetic purposes
    
    height = 600,
    width = 900
    )
    
    # Plot the fit of the linear model
    
    output$model <- renderPlot({
        
        # Clean the data for the model by dropping any NAs that would make the GLM not work and ungroup
        
        model_data <- county_detailed_shiny %>%
            filter(!is.na(obama_share_vs_mccain)) %>%
            ungroup()
        
        # Create the model using GLM with all the demographic factors
        
        model2 <- glm(data = model_data, log_prob ~ no_highschool_share + 
                          total_population + median_age + median_hh_income + 
                          pct_white_alone + share_below_povline + manufacturing_ind_share + obama_share_vs_mccain)
        
        # Predict using the model
        
        x_2 <- model_data %>% 
            mutate(prediction = predict(model2))
        
        # Show how well the predictions fit the actual values of social connectedness
        
        ggplot(x_2, aes(x=log_prob, y=prediction))+
            geom_jitter() +
            geom_smooth(method = "lm") +
            xlim(3,13) +
            ylim(3,7) +
            theme_light() +
            labs(title = "Fit of Demographic Regression Model on Social Connectedness Index",
                 y = "Predicted Probability",
                 x = "Actual Log Probability") + 
            theme(plot.title = element_text(face = "bold.italic", hjust = 0.5, size = 20))
        
    }, 
    
    # Set the best height and width for aesthetic purposes
    
    height = 600,
    width = 900
    )
    
    # Create the simple histogram of what share of friends are within 50 miles
    
    output$closeness <- renderPlot({
        
        closeness_county_shiny %>%
            ggplot(aes(x = sh_usfr_within_50miles)) +
            geom_histogram(bins = 30) +
            theme_light() +
            labs(title = "How Important is Geographic Closeness to Social Connections",
                 y = "# of Counties",
                 x = "Share of Social Connections that are Within 50 Miles") + 
            theme(plot.title = element_text(face = "bold.italic", hjust = 0.5, size = 20))
        
    }, 
    
    # Set the best height and width for aesthetic purposes
    
    height = 600,
    width = 900
    )
    
    # Create the same histogram but facet_wrap by state
    
    output$closenessfacet <- renderPlot({
        
        closeness_county_shiny %>%
            
            # Mutate into a five number FIPS code and use that to add state names
            
            mutate(own_county = as.character(own_county)) %>%
            mutate(own_county = str_pad(own_county, 5, pad = "0")) %>%
            mutate(own_state_fips = substr(own_county, 1, nchar(own_county)-3)) %>%
            mutate(own_state = cdlTools::fips(own_state_fips, to = "Name")) %>% 
            mutate(own_state = ifelse(own_state == "Deleware", "Delaware", own_state)) %>%
            ggplot(aes(x = sh_usfr_within_50miles)) +
            geom_histogram(bins = 30) +
            facet_wrap(~ own_state, nrow = 6, ncol = 9) + 
            theme_bw() +
            labs(title = "How Important is Geographic Closeness to Social Connections in Each State",
                 y = "# of Counties",
                 x = "Share of Social Connections that are Within 50 Miles") + 
            theme(plot.title = element_text(face = "bold.italic", hjust = 0.5, size = 20))
    }, 
    
    # Set the best height and width for aesthetic purposes
    
    height = 600,
    width = 900
    )
    
    # Load in the image for the top of front page
    
    output$image <- renderImage({
            # Return a list containing the filename and alt text
            list(src = './globals/network.jpg', 
                 height = 444,
                 width = 800, style="display: block; margin-left: auto; margin-right: auto;")
    }, deleteFile = FALSE
    )
    
    # Load in the arrow image for the front page
    
    output$arrow <- renderImage({
        # Return a list containing the filename and alt text
        list(src = './globals/arrow.png', 
             height = 50,
             width = 50, style="display: block; margin-left: auto; margin-right: auto;")
    }, deleteFile = FALSE
    )
    
    
    # Set the selectInput choices to be all distinct state values
    
    observe({
        updateSelectInput(
            session,
            "state",
            choices = unique(county_detailed_shiny$own_state)
        )
    })
    
    # Set the selectInput choices to be all distinct state values
    
    observe({
        updateSelectInput(
            session,
            "mapstate",
            choices = unique(county_detailed_shiny$own_state)
        )
    })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)

