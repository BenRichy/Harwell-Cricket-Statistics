# Harwell-Cricket-Statistics
Repository for gathering and visualising cricket statistics

To replicate this for your own club, you will need the following:

* PlayCricket API Key
* PlayCricketScorer Pro

At present the app is only set up to deal with clubs that have one team that plays on each day, i.e. small clubs.

Files to be edited:

* cricket_stats_import.qmd
  * IDs in the global chunk are needed to be edited to ensure it can pull data from your club.
  * The club ID is provided to you by PlayCricket when you receive an API key - see details here: https://play-cricket.ecb.co.uk/hc/en-us/articles/115004270145-Do-You-Have-an-API-to-Access-Play-Cricket-Data
  * The Team IDs can be found by going to the club's site on PlayCricket and clicking on the relevant team, e.g. https://harwell.play-cricket.com/Teams
* app/data/match_date_to_league.csv
  * This file is used to match the date of a match to a specific competition
* app/ui.R
  * Your club name will need to be entered in the relevant places

To make use of the detailed statistics, e.g. bowling stats by batter number, you will need to download PCS Pro and export all of the relevant csv files into a single csv.