# Spotify-Streaming-Analysis-R

## Description
 Data analysis and visualisation on personal Spotify extended streaming history in RStudio and R Markdown. Create a report that shows your favourite songs, albums, artists, and podcasts, timeseries charts, listening by day of the week, and more. Use this repo to explore your Spotify extended streaming history in greater detail than available on wrapped, or other online instant summaries.
 
 No Web API access is required.

 # Table of Contents
 
- [How to Run the Project for your Personal Data](#how-to-run-the-project-for-your-personal-data)
- [Output Example](#output-example)
  
## How to Run the Project for your Personal Data

### Download your Data
- Go to [Spotify privacy settings](https://www.spotify.com/uk/account/privacy/) and log in if required.
- Request your personal extended streaming history by selecting the Extended streaming history" box and clicking "Request Data".
- Wait for Spotify to email you to let you know that your **Extended streaming history** is ready for download.\
*Note: this is different to "account data" which will be available first.*
- Once your extended streaming history is available, click the green "download" button in the email. It should download as a zip file.

### Download the Project
- On the Code tab of the project, click the green "Code" button and download ZIP.
- Extract the ZIP file to a sensible place on your device.
- Navigate to "Spotify-Streaming-Analysis-R-main" folder then "Data" folder.
- Rename the "Your_Name_Here" folder to your name. This is useful later on if you want to run the analysis for friends.
- Move your extended streaming history ZIP folder into your personal named folder.

### Run the Project
- Within the "R Scripts" folder, open up "Import and Tidy.r".
- On line 9, change the strName variable to your name.
- Select the entire script with Ctrl+A and run with Ctrl+Enter.
- Now open up "Analysis-Markdown.rmd".
- On line 6, change the strName parameter to your name.
- Click "Knit" and wait for it to run. 
- Find the "Analysis-Markdown.html" file in Spotify-Streaming-Analysis-R-main to view your report.



 
