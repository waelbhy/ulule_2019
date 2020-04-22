PROJECT NAME : Shiny application : Ulule campaign funding rpogram.

DESCRIPTION : In a few words, this is an application that produces results on project funding campaigns (taken from Ulule's official website). This application suggests three different graph pages that allow the user to visualize : the number of created campaigns, the percentage of completed campaigns, and the average sum raised (in EUR) by the campaigns. The first graph of each page will offer the option to filter results by project category and/or year of interest (the results are also always automatically sorted by month of debut). The second graph will give out the same information but over the course of the years, whereas the last one will aggregate results according to months (aggregated on all years, in order to detect a potential seasonality) .

TABLE OF CONTENTS : - a www folder containing all static resources (images, css code, html file...) - ui.R and server.R : the user interface and server parts (respectively) of our application -our main data file data_ulule_2019_managed.csv that was generated by cleaning and performing the required treatment on the original data file - proj.rmd that contains the code that allows us to produce the desired data file.

ACCESS : This application can be accessed by downloading the aforementionned files and running the application on your machine, or by accessing the following link :  https://dorianherve.shinyapps.io/ulule2019/

USAGE : Launching the application will get you to the home page of the application which contains a brief description of the project and data provenance. At the top of the page, you will have a navigation bar at your disposal, where the "About" tab will lead you to the homepage, "Nb" will lead you to the "number of created campaigns" graphs page, "%" to the "percentage of completed campaigns" graphs page, and finally "Avg" to the "average sum raised (in EUR) by the campaigns" graphs page. 
The graph interactions work similarily on all three graph pages : You can filter the year of project funding debut by dragging the slider bar input (by default the year of interest is 2015), and you can also use the radio buttons to filter on a specific project category of your choice (by default, the results shown are for all categories). Upon interacting with one or more of those parameters, the graphs will automatically update to show the desired output.  As intended, the year filter will only apply to the first graph.

![welcome](https://user-images.githubusercontent.com/49319690/70246254-bac15180-1777-11ea-90d1-5d4770b30d1d.PNG)

![capture_graph](https://user-images.githubusercontent.com/49319690/70246280-ca409a80-1777-11ea-8705-bf0eda2516be.png)

![worldcloud](https://user-images.githubusercontent.com/49319690/70246370-f1976780-1777-11ea-9b00-57be753e4c57.png)

![final_graph](https://user-images.githubusercontent.com/49319690/70246405-ffe58380-1777-11ea-8856-a19f6f10aa06.png)


CONTRIBUTING : In order to improve this application, we suggest to implement further results trimming options, as well as diversify visualization options.

CREDITS : We would like to thank Mr. Antoine GIRARD and Mr. Vincent COUALLIER for their directives that led to the successful completion of this project.