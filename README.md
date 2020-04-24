PROJECT NAME : Shiny application : Ulule campaign funding rpogram.

DESCRIPTION : In a few words, this is an application that produces results on project funding campaigns (taken from Ulule's official website). This application suggests three different graph pages that allow the user to visualize : the number of created campaigns, the percentage of completed campaigns, and the average sum raised (in EUR) by the campaigns. The first graph of each page will offer the option to filter results by project category and/or year of interest (the results are also always automatically sorted by month of debut). The second graph will give out the same information but over the course of the years, whereas the last one will aggregate results according to months (aggregated on all years, in order to detect a potential seasonality) .

TABLE OF CONTENTS : <ul>
<li> a www folder containing all static resources (images, css code, html file...) </li>
<li>ui.R and server.R : the user interface and server parts (respectively) of our application</li>
<li> our main data file data_ulule_2019_managed.csv that was generated by cleaning and performing the required treatment on the original data file</li> 
<li>proj.rmd that contains the code that allows us to produce the desired data file.</li> </ul>

ACCESS : This application can be accessed by downloading the aforementionned files and running the application on your machine, or by accessing the following link :  https://dorianherve.shinyapps.io/ulule2019/

USAGE : Launching the application will get you to the home page of the application which contains a brief description of the project and data provenance. At the top of the page, you will have a navigation bar at your disposal, where the "About" tab will lead you to the homepage, "Nb" will lead you to the "number of created campaigns" graphs page, "%" to the "percentage of completed campaigns" graphs page, and finally "Avg" to the "average sum raised (in EUR) by the campaigns" graphs page. 
The graph interactions work similarily on all three graph pages : You can filter the year of project funding debut by dragging the slider bar input (by default the year of interest is 2015), and you can also use the radio buttons to filter on a specific project category of your choice (by default, the results shown are for all categories). Upon interacting with one or more of those parameters, the graphs will automatically update to show the desired output.  As intended, the year filter will only apply to the first graph.

![accueil](https://user-images.githubusercontent.com/49319690/80019793-5d386200-84d8-11ea-9c58-70de4883a2a0.png)

![vizu1](https://user-images.githubusercontent.com/49319690/80021179-6c201400-84da-11ea-9ec9-00e0b4e16bdb.png)

![vizu2](https://user-images.githubusercontent.com/49319690/80021214-76daa900-84da-11ea-8950-7cbb89143f94.png)

![vizu3_g](https://user-images.githubusercontent.com/49319690/80021231-7fcb7a80-84da-11ea-8ed4-043b06a877b3.png)

For example, upon taking a look at the previous result (average amount raised by "Game" projects in 2017), we promptly notice that that September was an exceptionnally successful month for that year. A closer look to the data reveals that a certain project raised more than 1380% of its original goal (90 000 euros) - a whopping 1 247 000 euros ! Find out more about this project at the following adress : https://www.ulule.com/noob-le-jeu-video/

CONTRIBUTING : In order to improve this application, we suggest to implement further results trimming options, as well as diversify visualization options.

CREDITS : We would like to thank Mr. Antoine GIRARD and Mr. Vincent COUALLIER for their directives that led to the successful completion of this project.