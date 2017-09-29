###SCRAPE All Playlists for Spintron Show. Default is Golden Age of Rock & Roll.

#package prerequisites
#install.packages("rvest")

library(rvest) 

# identify the Spintron show main page that contains all playlists for the shows.
allplaylists <-html("https://spinitron.com/radio/playlist.php?station=krfc&sv=l&showid=282")

# collect the name of the DJ that hosted to show, for all shows
playlistdj <-html_nodes(allplaylists, "#pltable td:nth-child(5) a")
playlistdj = html_text(playlistdj)


# collect each playlist url for all shows of particular radio show
playlisturls <-html_nodes(allplaylists, "#pltable td:nth-child(6) a")
playlisturls <- paste("http://spinitron.com/radio/",html_attr(playlisturls,"href"), sep="")

##note to self, this also works: "playlisturls <- paste0("http://spinitron.com/radio/",html_attr(playlisturls,"href"))"

# create a dataframe of the playlist DJ and playlists URL for all shows 
allplaylistsurls_df = data.frame(playlistdj, playlisturls)
allplaylistsurls_df


#substring right function for year
substrRight <- function(x){
  substr(x, nchar(x)-3, nchar(x))
}

#trim Day and Time from fullshowdate using a trim function to remove leading and trailing spaces#
trim <- function(x, s){ 
  gsub("^\\s+|\\s+$","", x)
}



# create function to collect each dataframe with the playlist info.
###START OF AllPlaylistsEver (APE) FUNCTION
AllPlaylistsEver <- function(x){
  Bigsteve_show <-html(paste(x))
  fullshowdate <- html_nodes(Bigsteve_show, ".leadpad")
  fullshowdate = html_text(fullshowdate)
  artist <- html_nodes(Bigsteve_show, "span.aw a")
  artist = html_text(artist)
  song <- html_nodes(Bigsteve_show, "span.sn")
  song = html_text(song)
  song = substr(song, 2, nchar(song)-1)
  spin_time <- html_nodes(Bigsteve_show, "p.st")
  spin_time = html_text(spin_time)
  label.and.year <- html_nodes(Bigsteve_show, "span.ld")
  label.and.year = html_text(label.and.year)  
  record.label <- html_nodes(Bigsteve_show, "span.ld a")
  record.label = html_text(record.label)


  label.and.yearc <- gsub("\\(", "", label.and.year)
  label.and.yearc <- gsub("\\)", "", label.and.yearc)
  label_yearclean = label.and.yearc
  
  #trim the show date
  shortShowDate <- substr(fullshowdate, 5, 17)
  shortShowDate = trim(shortShowDate)
  yearwhenshowaired <- trim(substr(shortShowDate, 9, 13))
  
  #get the trackyear ideally without NA coercion
  trackyear <- as.numeric(substrRight(label_yearclean))
  
  
  #album.name <- html_nodes(Bigsteve_show, "span.dn a")
  #album_name = html_text(album_name)
  #Not all songs have an album. Drop for now.   
  
  
  #other_single <- html_nodes(Bigsteve_show, "span.de")
  #other_single = html_text(other_single)
  #Not all songs have an 'other single'. Drop for now. 

  
  weekly_playlist = data.frame(artist, song, label.and.year, record.label, trackyear, spin_time, shortShowDate, yearwhenshowaired, stringsAsFactors=FALSE)
  return(weekly_playlist)
}
###END OF AllPlaylistsEver FUNCTION


### Data Playlist Check -- to checks data in the 4th row, 2nd column
### allplaylistsurls_df[4,2]


n <- nrow(allplaylistsurls_df)

### FOR LOOP TO APPLY AllPlaylistsEver FUNCTION
for (i in 1:n){ 
  if (i==1){
    final_results<-AllPlaylistsEver(paste(allplaylistsurls_df[i,2]))
  } else {
    final_results<-rbind(final_results,AllPlaylistsEver(paste(allplaylistsurls_df[i,2])))
  }
}
### NOTE: This takes a few minutes


###WRITE TO CSV
#write.csv(final_results, file = "ALL_PLAYLISTS_Golden_Age_of_Rock_and_Roll.csv")

### NOTES FOR Golden Age of Rock & Roll project: 
#### TO DO LIST: 
#### remove the last two playlist links as they are incomplete and captured in the third from last link
#### add the playlist DJ and the playlisturl to the final dataframe.
#### add album name and other single
#### add an append feature that only scrapes new playlists, i.e. does not re scrape all other playlists
#### set up AWS database/server
#### automate weekly scrape for new playlists only and addition to AWS data
#### make website of playlists
#### add dashboard of top played: artists, songs, record label, year
#### make repo of how to do this
