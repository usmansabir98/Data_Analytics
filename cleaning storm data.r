load("D:/Data Analytics/Course Material/umair/df_stormData_raw.RData")

# view all types of event types entered
View(sort(unique(df_stormData_raw$EVTYPE)))

# change vector class
df_stormData_raw$EVTYPE <- as.character(df_stormData_raw$EVTYPE)

#removing set of rows 
df_stormData_raw <- df_stormData_raw[-setdiff(grep("summary",df_stormData_raw$EVTYPE, ignore.case = TRUE),grep("blizzard",df_stormData_raw$EVTYPE,ignore.case = TRUE)),]



# check for a particular term in our vector (column)
unique(grep('winter', df_stormData_raw$EVTYPE, value = TRUE, ignore.case = TRUE))

# include in results with 'winter' keyword results with 'sleet' keyword
unique(grep('(winter)|(sleet)', df_stormData_raw$EVTYPE, value = TRUE, ignore.case = TRUE))

# exclude from the above results, string that contain 'snow' or 'blizzard'
setdiff(unique(grep('(winter)|(sleet)', df_stormData_raw$EVTYPE,value = TRUE, ignore.case = TRUE)),
        unique(grep('(snow)|(blizzard)', df_stormData_raw$EVTYPE,value = TRUE, ignore.case = TRUE)))

# assign to indices that contain the words 'winter' or 'sleet' but do not contain
# 'snow' or 'blizzard', the word 'WINTER WEATHER'
indices <- setdiff(grep('(winter)|(sleet)', df_stormData_raw$EVTYPE,ignore.case = TRUE),
                   grep('(snow)|(blizzard)', df_stormData_raw$EVTYPE,ignore.case = TRUE))

df_stormData_raw$EVTYPE[indices] <- 'WINTER WEATHER'

#-------------------------------------------------------------------------------------------

# search for avalanche keyword with fuzzy matching
unique(agrep('avalanche', df_stormData_raw$EVTYPE, value = TRUE, ignore.case = TRUE))

# exclude from the above results, string that contain 'blizzard'
setdiff(unique(agrep('avalanche', df_stormData_raw$EVTYPE,value = TRUE, ignore.case = TRUE)),
        unique(agrep('blizzard', df_stormData_raw$EVTYPE,value = TRUE, ignore.case = TRUE)))

# assign to indices 
indices <- setdiff(grep('avalanche', df_stormData_raw$EVTYPE,ignore.case = TRUE),
                   grep('blizzard', df_stormData_raw$EVTYPE,ignore.case = TRUE))

# reformat all indices to AVALANCHE
df_stormData_raw$EVTYPE[indices] <- 'AVALANCHE'

#-------------------------------------------------------------------------------------------


#-------------------------------------------------------------------------------------------

# search for blizzard keyword with fuzzy matching
unique(agrep('blizzard', df_stormData_raw$EVTYPE, value = TRUE, ignore.case = TRUE))

# assign to indices
indices <- unique(agrep('blizzard', df_stormData_raw$EVTYPE, ignore.case = TRUE))

# reformat all indices to AVALANCHE
df_stormData_raw$EVTYPE[indices] <- 'BLIZZARD'

#-------------------------------------------------------------------------------------------

#-------------------------------------------------------------------------------------------

# search for avalanche keyword with fuzzy matching
unique(agrep('chill', df_stormData_raw$EVTYPE, value = TRUE, ignore.case = TRUE))

# exclude from the above results, string that contain 'blizzard'
setdiff(unique(agrep('chill', df_stormData_raw$EVTYPE,value = TRUE, ignore.case = TRUE, fixed = FALSE)),
        unique(agrep('extreme wind chill', df_stormData_raw$EVTYPE,value = TRUE, ignore.case = TRUE, fixed = FALSE)))

# assign to indices 
indices <- setdiff(agrep('chill', df_stormData_raw$EVTYPE,ignore.case = TRUE, fixed = FALSE),
                   agrep('extreme wind chill', df_stormData_raw$EVTYPE,ignore.case = TRUE, fixed = FALSE))

# reformat all indices to AVALANCHE
df_stormData_raw$EVTYPE[indices] <- 'COLD/WIND CHILL'

#-------------------------------------------------------------------------------------------

#-------------------------------------------------------------------------------------------

# search for extreme wind chill keyword with fuzzy matching
unique(agrep('extreme wind chill', df_stormData_raw$EVTYPE, value = TRUE, ignore.case = TRUE))

# assign to indices 

indices <- unique(agrep('extreme wind chill', df_stormData_raw$EVTYPE, ignore.case = TRUE))

# reformat all indices to AVALANCHE
df_stormData_raw$EVTYPE[indices] <- 'EXTREME WIND CHILL'

#-------------------------------------------------------------------------------------------

#-------------------------------------------------------------------------------------------

setdiff(agrep("flash",df_stormData_raw$EVTYPE, ignore.case = TRUE, value=TRUE),
        agrep("(thunderstorm)|(heavy rain)|(ice storm)",df_stormData_raw$EVTYPE,ignore.case = TRUE, fixed=FALSE, value=TRUE))
# assign to indices 
indices <- setdiff(agrep("flash",df_stormData_raw$EVTYPE, ignore.case = TRUE),
                   agrep("(thunderstorm)|(heavy rain)|(ice storm)",df_stormData_raw$EVTYPE,ignore.case = TRUE, fixed=FALSE))


# reformat all indices to FLASH FLOOD
df_stormData_raw$EVTYPE[indices] <- 'FLASH FLOOD'

#-------------------------------------------------------------------------------------------

df_stormData_raw$EVTYPE[unique(agrep('dense fog', df_stormData_raw$EVTYPE, ignore.case = TRUE))] <- "DENSE FOG"
df_stormData_raw$EVTYPE[unique(agrep('(coastal flood)|(beach erosion)', df_stormData_raw$EVTYPE, ignore.case = TRUE, fixed=FALSE))] <- "COASTAL FLOOD"
df_stormData_raw$EVTYPE[setdiff(agrep("surge",df_stormData_raw$EVTYPE, ignore.case = TRUE),agrep("exposure",df_stormData_raw$EVTYPE,ignore.case = TRUE, fixed=FALSE))] <- 'STORM SURGE'


#-------------------------------------------------------------------------------------------

# search with fuzzy match
unique(agrep('thunderstorm', df_stormData_raw$EVTYPE, value = TRUE, ignore.case = TRUE))
unique(agrep('tstm', df_stormData_raw$EVTYPE, value = TRUE, ignore.case = TRUE, fixed = FALSE))

# exclude from the above results, string that contain particular values
setdiff(unique(agrep('thunderstorm', df_stormData_raw$EVTYPE,value = TRUE, ignore.case = TRUE, fixed = FALSE)),
        unique(agrep('(marine)|(flash)', df_stormData_raw$EVTYPE,value = TRUE, ignore.case = TRUE, fixed = FALSE)))


setdiff(unique(agrep('tstm', df_stormData_raw$EVTYPE,value = TRUE, ignore.case = TRUE, fixed = FALSE)),
        unique(agrep('(marine)|(non tstm)', df_stormData_raw$EVTYPE,value = TRUE, ignore.case = TRUE, fixed = FALSE)))

# assign to indices 
indices <- setdiff(agrep('thunderstorm', df_stormData_raw$EVTYPE,ignore.case = TRUE, fixed = FALSE),
                   agrep('(marine)|(flash)', df_stormData_raw$EVTYPE,ignore.case = TRUE, fixed = FALSE))

indices2 <- setdiff(agrep('tstm', df_stormData_raw$EVTYPE,ignore.case = TRUE, fixed = FALSE),
                   agrep('(marine)|(non tstm)', df_stormData_raw$EVTYPE,ignore.case = TRUE, fixed = FALSE))

# reformat all indices to AVALANCHE
df_stormData_raw$EVTYPE[indices] <- 'THUNDERSTORM WINDS'

df_stormData_raw$EVTYPE[indices2] <- 'THUNDERSTORM WINDS'

#-------------------------------------------------------------------------------------------

#-------------------------------------------------------------------------------------------

# search with fuzzy match
unique(agrep('lightning', df_stormData_raw$EVTYPE, value = TRUE, ignore.case = TRUE))

# exclude from the above results, string that contain particular values
setdiff(unique(agrep('lightning', df_stormData_raw$EVTYPE,value = TRUE, ignore.case = TRUE, fixed = FALSE)),
        unique(agrep('heavy', df_stormData_raw$EVTYPE,value = TRUE, ignore.case = TRUE, fixed = FALSE)))

# assign to indices 
indices <- setdiff(agrep('lightning', df_stormData_raw$EVTYPE,ignore.case = TRUE, fixed = FALSE),
                   agrep('heavy', df_stormData_raw$EVTYPE,ignore.case = TRUE, fixed = FALSE))

# reformat all indices to AVALANCHE
df_stormData_raw$EVTYPE[indices] <- 'LIGHTNING'

#-------------------------------------------------------------------------------------------

#-------------------------------------------------------------------------------------------

# search with fuzzy match
unique(agrep('devil', df_stormData_raw$EVTYPE, value = TRUE, ignore.case = TRUE))

indices <- agrep('devil', df_stormData_raw$EVTYPE, ignore.case = TRUE)

# reformat all indices to AVALANCHE
df_stormData_raw$EVTYPE[indices] <- 'DUST DEVIL'

#-------------------------------------------------------------------------------------------

#-------------------------------------------------------------------------------------------

# search with fuzzy match
unique(agrep('waterspout', df_stormData_raw$EVTYPE, value = TRUE, ignore.case = TRUE))

indices <- agrep('waterspout', df_stormData_raw$EVTYPE, ignore.case = TRUE)

# reformat all indices to AVALANCHE
df_stormData_raw$EVTYPE[indices] <- 'WATERSPOUT'

#-------------------------------------------------------------------------------------------

#-------------------------------------------------------------------------------------------

# search with fuzzy match
unique(agrep('tornado', df_stormData_raw$EVTYPE, value = TRUE, ignore.case = TRUE))

indices <- unique(agrep('tornado', df_stormData_raw$EVTYPE, ignore.case = TRUE))

# reformat all indices to AVALANCHE
df_stormData_raw$EVTYPE[indices] <- 'TORNADO'

#-------------------------------------------------------------------------------------------


unique(grep('(funnel)|(cloud)', df_stormData_raw$EVTYPE, value = TRUE, ignore.case = TRUE, fixed = FALSE))

indices <- grep('(funnel)|(cloud)', df_stormData_raw$EVTYPE, ignore.case = TRUE, fixed = FALSE)

df_stormData_raw$EVTYPE[indices] <- 'FUNNEL CLOUD'

#-------------------------------------------------------------------------------------------

indices <- unique(agrep('volcanic ash', df_stormData_raw$EVTYPE, ignore.case = TRUE))

# reformat all indices to AVALANCHE
df_stormData_raw$EVTYPE[indices] <- 'VOLCANIC ASH'

#-------------------------------------------------------------------------------------------

#-------------------------------------------------------------------------------------------

indices <- unique(agrep('tropical', df_stormData_raw$EVTYPE, ignore.case = TRUE))

# reformat all indices to AVALANCHE
df_stormData_raw$EVTYPE[indices] <- 'TROPICAL STORM'

#-------------------------------------------------------------------------------------------

#-------------------------------------------------------------------------------------------

# search with fuzzy match
unique(agrep('chill', df_stormData_raw$EVTYPE, value = TRUE, ignore.case = TRUE))
unique(agrep('(snow)|(heavy)', df_stormData_raw$EVTYPE, value = TRUE, ignore.case = TRUE, fixed = FALSE))

# exclude from the above results, string that contain particular values
setdiff(unique(agrep('strong wind', df_stormData_raw$EVTYPE,value = TRUE, ignore.case = TRUE, fixed = FALSE)),
        unique(agrep('(marine)|(flood)', df_stormData_raw$EVTYPE,value = TRUE, ignore.case = TRUE, fixed = FALSE)))

# assign to indices 
indices <- setdiff(agrep('strong wind', df_stormData_raw$EVTYPE,ignore.case = TRUE, fixed = FALSE),
                   agrep('(marine)|(flood)', df_stormData_raw$EVTYPE,ignore.case = TRUE, fixed = FALSE))

# reformat all indices to AVALANCHE
df_stormData_raw$EVTYPE[indices] <- 'STRONG WIND'

#-------------------------------------------------------------------------------------------

#-------------------------------------------------------------------------------------------

# search with fuzzy match
unique(agrep('chill', df_stormData_raw$EVTYPE, value = TRUE, ignore.case = TRUE))
unique(agrep('(snow)|(heavy)', df_stormData_raw$EVTYPE, value = TRUE, ignore.case = TRUE, fixed = FALSE))

# exclude from the above results, string that contain particular values
setdiff(unique(agrep('rain', df_stormData_raw$EVTYPE,value = TRUE, ignore.case = TRUE, fixed = FALSE)),
        unique(grep('(erin)|(marine)|(flood)|(sleet)|(snow)|(freez)|(dust)', df_stormData_raw$EVTYPE,value = TRUE, ignore.case = TRUE, fixed = FALSE)))

# assign to indices 
indices <- setdiff(agrep('rain', df_stormData_raw$EVTYPE,ignore.case = TRUE, fixed = FALSE),
                   grep('(erin)|(marine)|(flood)|(sleet)|(snow)|(freez)|(dust)', df_stormData_raw$EVTYPE,ignore.case = TRUE))

# reformat all indices to AVALANCHE
df_stormData_raw$EVTYPE[indices] <- 'HEAVY RAIN'

#-------------------------------------------------------------------------------------------

#-------------------------------------------------------------------------------------------

# search with fuzzy match
unique(agrep('(hurricane)|(typhoon)', df_stormData_raw$EVTYPE, value = TRUE, ignore.case = TRUE, fixed = FALSE))

indices <- unique(agrep('(hurricane)|(typhoon)', df_stormData_raw$EVTYPE, ignore.case = TRUE, fixed = FALSE))

# reformat all indices to AVALANCHE
df_stormData_raw$EVTYPE[indices] <- 'HURRICANE/TYPHOON'

#-------------------------------------------------------------------------------------------

#-------------------------------------------------------------------------------------------

# search with fuzzy match
unique(agrep('flood', df_stormData_raw$EVTYPE, value = TRUE, ignore.case = TRUE))

# exclude from the above results, string that contain particular values
setdiff(unique(agrep('flood', df_stormData_raw$EVTYPE,value = TRUE, ignore.case = TRUE, fixed = FALSE)),
        unique(grep('(flash)|(coastal)|(snow)|(lake)|(ice storm)|(floyd)|(tidal)', df_stormData_raw$EVTYPE,value = TRUE, ignore.case = TRUE, fixed = FALSE)))

# assign to indices 
indices <- setdiff(agrep('flood', df_stormData_raw$EVTYPE,ignore.case = TRUE, fixed = FALSE),
                   grep('(flash)|(coastal)|(snow)|(lake)|(ice storm)|(floyd)|(tidal)', df_stormData_raw$EVTYPE,ignore.case = TRUE, fixed = FALSE))


# reformat all indices to AVALANCHE
df_stormData_raw$EVTYPE[indices] <- 'FLOOD'

#-------------------------------------------------------------------------------------------

#-------------------------------------------------------------------------------------------

# search with fuzzy match
unique(agrep('freezing rain', df_stormData_raw$EVTYPE, value = TRUE, ignore.case = TRUE, fixed = FALSE))
unique(grep('(drizzle)|(spray)', df_stormData_raw$EVTYPE, value = TRUE, ignore.case = TRUE, fixed = FALSE))

# exclude from the above results, string that contain particular values
setdiff(unique(agrep('freezing rain', df_stormData_raw$EVTYPE,value = TRUE, ignore.case = TRUE, fixed = FALSE)),
        unique(grep('heavy snow', df_stormData_raw$EVTYPE,value = TRUE, ignore.case = TRUE, fixed = FALSE)))

# assign to indices 
indices <- setdiff(agrep('freezing rain', df_stormData_raw$EVTYPE,ignore.case = TRUE, fixed = FALSE),
                   grep('heavy snow', df_stormData_raw$EVTYPE,ignore.case = TRUE))
indices2 <- unique(grep('(drizzle)|(spray)', df_stormData_raw$EVTYPE, ignore.case = TRUE))

# reformat all indices to Ice Storm
df_stormData_raw$EVTYPE[indices] <- 'ICE STORM'
df_stormData_raw$EVTYPE[indices2] <- 'ICE STORM'

#-------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------

# search with fuzzy match
unique(agrep('(accident)|(mishap)', df_stormData_raw$EVTYPE, value = TRUE, ignore.case = TRUE, fixed = FALSE))

indices <- unique(agrep('(accident)|(mishap)', df_stormData_raw$EVTYPE, ignore.case = TRUE, fixed = FALSE))

# reformat all indices
df_stormData_raw$EVTYPE[indices] <- 'MARINE ACCIDENT'

#-------------------------------------------------------------------------------------------

#-------------------------------------------------------------------------------------------

# search with fuzzy match
unique(agrep('lake effect snow', df_stormData_raw$EVTYPE, value = TRUE, ignore.case = TRUE, fixed = FALSE))

indices <- unique(agrep('lake effect snow', df_stormData_raw$EVTYPE, ignore.case = TRUE, fixed = FALSE))

# reformat all indices
df_stormData_raw$EVTYPE[indices] <- 'LAKE-EFFECT SNOW'

#-------------------------------------------------------------------------------------------

#-------------------------------------------------------------------------------------------

# search with fuzzy match
unique(agrep('high wind', df_stormData_raw$EVTYPE, value = TRUE, ignore.case = TRUE))

# exclude from the above results, string that contain particular values
setdiff(unique(agrep('high wind', df_stormData_raw$EVTYPE,value = TRUE, ignore.case = TRUE, fixed = FALSE)),
        unique(grep('(heavy snow)|(marine)|(dust)', df_stormData_raw$EVTYPE,value = TRUE, ignore.case = TRUE, fixed = FALSE)))

# assign to indices 
indices <- setdiff(agrep('high wind', df_stormData_raw$EVTYPE,ignore.case = TRUE, fixed = FALSE),
                   grep('(heavy snow)|(marine)|(dust)', df_stormData_raw$EVTYPE,ignore.case = TRUE, fixed = FALSE))

# reformat all indices to AVALANCHE
df_stormData_raw$EVTYPE[indices] <- 'HIGH WIND'

#-------------------------------------------------------------------------------------------

#-------------------------------------------------------------------------------------------

# search with fuzzy match
unique(agrep('heavy snow', df_stormData_raw$EVTYPE, value = TRUE, ignore.case = TRUE))

unique(grep('heavy shower', df_stormData_raw$EVTYPE, value = TRUE, ignore.case = TRUE))

# exclude from the above results, string that contain particular values
setdiff(unique(agrep('HEAVY SNOW', df_stormData_raw$EVTYPE,value = TRUE, ignore.case = TRUE, fixed = FALSE)),
        unique(grep('heavy shower', df_stormData_raw$EVTYPE,value = TRUE, ignore.case = TRUE, fixed = FALSE)))

# assign to indices 
indices <- setdiff(agrep('heavy snow', df_stormData_raw$EVTYPE,ignore.case = TRUE),
                   grep('heavy shower', df_stormData_raw$EVTYPE,ignore.case = TRUE))


indices2 <- unique(agrep('heavy shower', df_stormData_raw$EVTYPE, ignore.case = TRUE))

# reformat all indices to AVALANCHE
df_stormData_raw$EVTYPE[indices] <- 'HEAVY SNOW'

df_stormData_raw$EVTYPE[indices2] <- 'HEAVY RAIN'

#-------------------------------------------------------------------------------------------

#-------------------------------------------------------------------------------------------

# search with fuzzy match
unique(agrep('surf', df_stormData_raw$EVTYPE, value = TRUE, ignore.case = TRUE))

# exclude from the above results, string that contain particular values
setdiff(unique(agrep('surf', df_stormData_raw$EVTYPE,value = TRUE, ignore.case = TRUE, fixed = FALSE)),
        unique(grep('(surge)|(exposure)', df_stormData_raw$EVTYPE,value = TRUE, ignore.case = TRUE)))

# assign to indices 
indices <- setdiff(agrep('surf', df_stormData_raw$EVTYPE,ignore.case = TRUE),
                   grep('(surge)|(exposure)', df_stormData_raw$EVTYPE,ignore.case = TRUE))

# reformat all indices to AVALANCHE
df_stormData_raw$EVTYPE[indices] <- 'HIGH SURF'

#-------------------------------------------------------------------------------------------

#-------------------------------------------------------------------------------------------

# search with fuzzy match
unique(agrep('hail', df_stormData_raw$EVTYPE, value = TRUE, ignore.case = TRUE))
unique(grep('(marine)|(chill)', df_stormData_raw$EVTYPE, value = TRUE, ignore.case = TRUE, fixed = FALSE))

# exclude from the above results, string that contain particular values
setdiff(unique(agrep('hail', df_stormData_raw$EVTYPE,value = TRUE, ignore.case = TRUE)),
        unique(grep('(marine)|(chill)', df_stormData_raw$EVTYPE,value = TRUE, ignore.case = TRUE)))

# assign to indices 
indices <- setdiff(agrep('hail', df_stormData_raw$EVTYPE,ignore.case = TRUE),
                   grep('(marine)|(chill)', df_stormData_raw$EVTYPE,ignore.case = TRUE))

# reformat all indices to AVALANCHE
df_stormData_raw$EVTYPE[indices] <- 'HAIL'

#-------------------------------------------------------------------------------------------

#-------------------------------------------------------------------------------------------

# search with fuzzy match
unique(agrep('smoke', df_stormData_raw$EVTYPE, value = TRUE, ignore.case = TRUE))

indices <- unique(agrep('smoke', df_stormData_raw$EVTYPE, ignore.case = TRUE))

# reformat all indices to AVALANCHE
df_stormData_raw$EVTYPE[indices] <- 'DENSE SMOKE'

#-------------------------------------------------------------------------------------------

#-------------------------------------------------------------------------------------------

# search with fuzzy match
unique(agrep('drought', df_stormData_raw$EVTYPE, value = TRUE, ignore.case = TRUE))

indices <- unique(agrep('drought', df_stormData_raw$EVTYPE, ignore.case = TRUE))

# reformat all indices to AVALANCHE
df_stormData_raw$EVTYPE[indices] <- 'DROUGHT'

#-------------------------------------------------------------------------------------------

#-------------------------------------------------------------------------------------------

# search with fuzzy match
unique(grep('freeze', df_stormData_raw$EVTYPE, value = TRUE, ignore.case = TRUE))
unique(grep('frost', df_stormData_raw$EVTYPE, value = TRUE, ignore.case = TRUE))
unique(grep('(frost)|(freeze)', df_stormData_raw$EVTYPE, value = TRUE, ignore.case = TRUE))

indices <- unique(grep('(frost)|(freeze)', df_stormData_raw$EVTYPE, ignore.case = TRUE))

# reformat all indices to AVALANCHE
df_stormData_raw$EVTYPE[indices] <- 'FROST/FREEZE'

#-------------------------------------------------------------------------------------------

#-------------------------------------------------------------------------------------------

# search with fuzzy match
unique(agrep('sleet', df_stormData_raw$EVTYPE, value = TRUE, ignore.case = TRUE))

indices <- unique(agrep('sleet', df_stormData_raw$EVTYPE, ignore.case = TRUE))

# reformat all indices to AVALANCHE
df_stormData_raw$EVTYPE[indices] <- 'SLEET'

#-------------------------------------------------------------------------------------------

#-------------------------------------------------------------------------------------------

# search with fuzzy match
unique(agrep('rip current', df_stormData_raw$EVTYPE, value = TRUE, ignore.case = TRUE))

indices <- unique(agrep('rip current', df_stormData_raw$EVTYPE, ignore.case = TRUE))

# reformat all indices to AVALANCHE
df_stormData_raw$EVTYPE[indices] <- 'RIP CURRENT'

#-------------------------------------------------------------------------------------------

#-------------------------------------------------------------------------------------------

# search with fuzzy match
unique(grep('heat', df_stormData_raw$EVTYPE, value = TRUE, ignore.case = TRUE))

# exclude from the above results, string that contain particular values
setdiff(unique(grep('heat', df_stormData_raw$EVTYPE,value = TRUE, ignore.case = TRUE)),
        unique(grep('(record)|(excessive)|(extreme)', df_stormData_raw$EVTYPE,value = TRUE, ignore.case = TRUE)))

# assign to indices 
indices <- setdiff(grep('heat', df_stormData_raw$EVTYPE,ignore.case = TRUE),
                   grep('(record)|(excessive)|(extreme)', df_stormData_raw$EVTYPE,ignore.case = TRUE))

# reformat all indices to AVALANCHE
df_stormData_raw$EVTYPE[indices] <- 'HEAT'

#-------------------------------------------------------------------------------------------

#-------------------------------------------------------------------------------------------

# search with fuzzy match
unique(grep('heat', df_stormData_raw$EVTYPE, value = TRUE, ignore.case = TRUE))

# exclude from the above results, string that contain only heat
setdiff(unique(grep('heat', df_stormData_raw$EVTYPE,value = TRUE, ignore.case = TRUE)),
        unique(grep('^heat', df_stormData_raw$EVTYPE,value = TRUE, ignore.case = TRUE)))

# assign to indices 
indices <- setdiff(grep('heat', df_stormData_raw$EVTYPE,ignore.case = TRUE),
                   grep('^heat', df_stormData_raw$EVTYPE,ignore.case = TRUE))

# reformat all indices to excessive heat
df_stormData_raw$EVTYPE[indices] <- 'EXCESSIVE HEAT'

#-------------------------------------------------------------------------------------------

#-------------------------------------------------------------------------------------------

# search with fuzzy match
unique(grep('dust', df_stormData_raw$EVTYPE, value = TRUE, ignore.case = TRUE))

# exclude from the above results, string that contain particular values
setdiff(unique(grep('dust', df_stormData_raw$EVTYPE,value = TRUE, ignore.case = TRUE, fixed = FALSE)),
        unique(grep('devil', df_stormData_raw$EVTYPE,value = TRUE, ignore.case = TRUE, fixed = FALSE)))

# assign to indices 
indices <- setdiff(grep('dust', df_stormData_raw$EVTYPE,ignore.case = TRUE, fixed = FALSE),
                   grep('devil', df_stormData_raw$EVTYPE,ignore.case = TRUE, fixed = FALSE))

# reformat all indices to AVALANCHE
df_stormData_raw$EVTYPE[indices] <- 'DUST STORM'

#-------------------------------------------------------------------------------------------

#-------------------------------------------------------------------------------------------

# search with fuzzy match
unique(agrep('fire', df_stormData_raw$EVTYPE, value = TRUE, ignore.case = TRUE))

# exclude from the above results, string that contain particular values
setdiff(unique(agrep('fire', df_stormData_raw$EVTYPE,value = TRUE, ignore.case = TRUE, fixed = FALSE)),
        unique(grep('(freez)|(snow)', df_stormData_raw$EVTYPE,value = TRUE, ignore.case = TRUE, fixed = FALSE)))

# assign to indices 
indices <- setdiff(grep('fire', df_stormData_raw$EVTYPE,ignore.case = TRUE, fixed = FALSE),
                   grep('(freez)|(snow)', df_stormData_raw$EVTYPE,ignore.case = TRUE, fixed = FALSE))

# reformat all indices to AVALANCHE
df_stormData_raw$EVTYPE[indices] <- 'WILDFIRE'

#-------------------------------------------------------------------------------------------

#-------------------------------------------------------------------------------------------

# search with fuzzy match
unique(agrep('ice storm', df_stormData_raw$EVTYPE, value = TRUE, ignore.case = TRUE))

indices <- unique(agrep('ice storm', df_stormData_raw$EVTYPE, ignore.case = TRUE))

# reformat all indices to AVALANCHE
df_stormData_raw$EVTYPE[indices] <- 'ICE STORM'

#-------------------------------------------------------------------------------------------

#-------------------------------------------------------------------------------------------

# search all with warm and hot keywords
unique(grep('warm|hot', df_stormData_raw$EVTYPE, value = TRUE, ignore.case = TRUE, fixed = FALSE))

# exclude from the above results, string that contain dry value
# as dry warmth will be considered as drought and not as heat
setdiff(unique(grep('warm|hot', df_stormData_raw$EVTYPE,value = TRUE, ignore.case = TRUE, fixed = FALSE)),
        unique(grep('dry', df_stormData_raw$EVTYPE,value = TRUE, ignore.case = TRUE, fixed = FALSE)))

# assign to indices 
indices <- setdiff(grep('warm|hot', df_stormData_raw$EVTYPE,ignore.case = TRUE),
                   grep('dry', df_stormData_raw$EVTYPE,ignore.case = TRUE))

# reformat all indices to HEAT
df_stormData_raw$EVTYPE[indices] <- 'HEAT'

#-------------------------------------------------------------------------------------------

# remaining warm and hot keywords containing DRY will be assigned to drought
unique(grep('warm|hot', df_stormData_raw$EVTYPE, value = TRUE, ignore.case = TRUE, fixed = FALSE))

indices <- unique(grep('warm|hot', df_stormData_raw$EVTYPE, ignore.case = TRUE))

# reformat all indices to HEAT
df_stormData_raw$EVTYPE[indices] <- 'DROUGHT'


#-------------------------------------------------------------------------------------------

unique(agrep('blowing snow', df_stormData_raw$EVTYPE, value = TRUE, ignore.case = TRUE, fixed = FALSE))

indices <- unique(agrep('blowing snow', df_stormData_raw$EVTYPE, ignore.case = TRUE))

# reformat all indices to WINTER WEATHER
df_stormData_raw$EVTYPE[indices] <- 'WINTER WEATHER'

#-------------------------------------------------------------------------------------------

unique(grep('dry', df_stormData_raw$EVTYPE, value = TRUE, ignore.case = TRUE, fixed = FALSE))

indices <- unique(grep('dry', df_stormData_raw$EVTYPE, ignore.case = TRUE))

# reformat all indices to DROUGHT
df_stormData_raw$EVTYPE[indices] <- 'DROUGHT'

#-------------------------------------------------------------------------------------------

unique(grep('slide', df_stormData_raw$EVTYPE, value = TRUE, ignore.case = TRUE))

indices <- grep('slide', df_stormData_raw$EVTYPE, ignore.case = TRUE)

# reformat all indices to DEBRIS FLOW
df_stormData_raw$EVTYPE[indices] <- 'DEBRIS FLOW'

#-------------------------------------------------------------------------------------------

# search with fuzzy match
unique(agrep('snow', df_stormData_raw$EVTYPE, value = TRUE, ignore.case = TRUE))
unique(grep('(excessive)|(heavy)', df_stormData_raw$EVTYPE, value = TRUE, ignore.case = TRUE, fixed = FALSE))

# exclude from the above results, string that contain particular values
setdiff(unique(grep('snow', df_stormData_raw$EVTYPE,value = TRUE, ignore.case = TRUE)),
        unique(grep('(lake)|(excessive)|(heavy)|(thunder)|(storm)', df_stormData_raw$EVTYPE,value = TRUE, ignore.case = TRUE)))

# assign to indices 
indices <- setdiff(grep('snow', df_stormData_raw$EVTYPE,ignore.case = TRUE),
                   grep('(lake)|(excessive)|(heavy)|(thunder)|(storm)', df_stormData_raw$EVTYPE,ignore.case = TRUE))

# reformat all indices to WINTER WEATHER
df_stormData_raw$EVTYPE[indices] <- 'WINTER WEATHER'

#-------------------------------------------------------------------------------------------

#-------------------------------------------------------------------------------------------

# search with fuzzy match
unique(agrep('(excessive)|(heavy)', df_stormData_raw$EVTYPE, value = TRUE, ignore.case = TRUE, fixed = FALSE))

# exclude from the above results, string that contain particular values
setdiff(unique(grep('(excessive)|(heavy)', df_stormData_raw$EVTYPE,value = TRUE, ignore.case = TRUE, fixed = FALSE)),
        unique(grep('(^heavy rain$)|(heat)|(wet)|(lake)|(^excessive$)|(seas)|(cold)', df_stormData_raw$EVTYPE,value = TRUE, ignore.case = TRUE, fixed = FALSE)))

# assign to indices 
indices <- setdiff(grep('(excessive)|(heavy)', df_stormData_raw$EVTYPE,ignore.case = TRUE),
                   grep('(^heavy rain$)|(heat)|(wet)|(lake)|(^excessive$)|(seas)|(cold)', df_stormData_raw$EVTYPE,ignore.case = TRUE, fixed = FALSE))


# reformat all indices to HEAVY SNOW
df_stormData_raw$EVTYPE[indices] <- 'HEAVY SNOW'

#-------------------------------------------------------------------------------------------


#-------------------------------------------------------------------------------------------

# search with fuzzy match
unique(grep('extreme cold', df_stormData_raw$EVTYPE, value = TRUE, ignore.case = TRUE))
unique(grep('(extreme cold)|(extreme wind chill)', df_stormData_raw$EVTYPE, value = TRUE, ignore.case = TRUE, fixed = FALSE))

indices <- agrep('(extreme cold)|(extreme wind chill)', df_stormData_raw$EVTYPE, ignore.case = TRUE, fixed = FALSE)

# reformat all indices to extreme cold/wind chill
df_stormData_raw$EVTYPE[indices] <- 'EXTREME COLD/WIND CHILL'

#-------------------------------------------------------------------------------------------

#-------------------------------------------------------------------------------------------

# search with fuzzy match
unique(grep('cold|cool', df_stormData_raw$EVTYPE, value = TRUE, ignore.case = TRUE))

# exclude from the above results, string that contain particular values
setdiff(unique(grep('cold|cool', df_stormData_raw$EVTYPE,value = TRUE, ignore.case = TRUE)),
        unique(grep('extreme', df_stormData_raw$EVTYPE,value = TRUE, ignore.case = TRUE)))

# assign to indices 
indices <- setdiff(grep('cold|cool', df_stormData_raw$EVTYPE,ignore.case = TRUE, fixed = FALSE),
                   grep('extreme', df_stormData_raw$EVTYPE,ignore.case = TRUE, fixed = FALSE))

# reformat all indices to cold/wind chill
df_stormData_raw$EVTYPE[indices] <- 'COLD/WIND CHILL'

#-------------------------------------------------------------------------------------------

#-------------------------------------------------------------------------------------------

# search with fuzzy match
unique(grep('^dam|jam$', df_stormData_raw$EVTYPE, value = TRUE, ignore.case = TRUE))

indices <- grep('^dam|jam$', df_stormData_raw$EVTYPE, ignore.case = TRUE)

# reformat all indices to FLASH FLOOD
df_stormData_raw$EVTYPE[indices] <- 'FLASH FLOOD'

#-------------------------------------------------------------------------------------------

#-------------------------------------------------------------------------------------------

# search with fuzzy match
unique(grep('ice', df_stormData_raw$EVTYPE, value = TRUE, ignore.case = TRUE))

# exclude from the above results, string that contain particular values
setdiff(unique(grep('ice', df_stormData_raw$EVTYPE,value = TRUE, ignore.case = TRUE)),
        unique(grep('storm|fog', df_stormData_raw$EVTYPE,value = TRUE, ignore.case = TRUE)))

# assign to indices 
indices <- setdiff(grep('ice', df_stormData_raw$EVTYPE,ignore.case = TRUE, fixed = FALSE),
                   grep('storm|fog', df_stormData_raw$EVTYPE,ignore.case = TRUE, fixed = FALSE))

# reformat all indices to winter weather
df_stormData_raw$EVTYPE[indices] <- 'WINTER WEATHER'

#-------------------------------------------------------------------------------------------

#-------------------------------------------------------------------------------------------

# search with fuzzy match
unique(grep('apache', df_stormData_raw$EVTYPE, value = TRUE, ignore.case = TRUE))

indices <- grep('apache', df_stormData_raw$EVTYPE, ignore.case = TRUE)

# reformat all indices to THUNDERSTORM
df_stormData_raw$EVTYPE[indices] <- 'THUNDERSTORM'

#-------------------------------------------------------------------------------------------

#-------------------------------------------------------------------------------------------

# search with fuzzy match
unique(agrep('precip', df_stormData_raw$EVTYPE, value = TRUE, ignore.case = TRUE))

indices <- agrep('precip', df_stormData_raw$EVTYPE, ignore.case = TRUE)

# reformat all indices to PRECIPITATION
df_stormData_raw$EVTYPE[indices] <- 'PRECIPITATION'

#-------------------------------------------------------------------------------------------

#-------------------------------------------------------------------------------------------

# search with fuzzy match
unique(grep('burst|gust', df_stormData_raw$EVTYPE, value = TRUE, ignore.case = TRUE, fixed = FALSE))

indices <- grep('burst|gust', df_stormData_raw$EVTYPE, ignore.case = TRUE)

# reformat all indices to THUNDERSTORM WINDS
df_stormData_raw$EVTYPE[indices] <- 'THUNDERSTORM WINDS'

#-------------------------------------------------------------------------------------------

#-------------------------------------------------------------------------------------------

# search with fuzzy match
unique(agrep('whirlwind', df_stormData_raw$EVTYPE, value = TRUE, ignore.case = TRUE))

indices <- agrep('whirlwind', df_stormData_raw$EVTYPE, ignore.case = TRUE)

# reformat all indices to PRECIPITATION
df_stormData_raw$EVTYPE[indices] <- 'THUNDERSTORM WINDS'

#-------------------------------------------------------------------------------------------

#-------------------------------------------------------------------------------------------

# search with fuzzy match
unique(grep('wind', df_stormData_raw$EVTYPE, value = TRUE, ignore.case = TRUE))

# exclude from the above results, string that contain particular values
setdiff(unique(grep('wind', df_stormData_raw$EVTYPE,value = TRUE, ignore.case = TRUE, fixed = FALSE)),
        unique(grep('(thunderstorm)|(cold/wind chill)|(high wind)|(strong wind)|(marine)', df_stormData_raw$EVTYPE,value = TRUE, ignore.case = TRUE, fixed = FALSE)))

# assign to indices 
indices <- setdiff(grep('wind', df_stormData_raw$EVTYPE,ignore.case = TRUE, fixed = FALSE),
                   grep('(thunderstorm)|(cold/wind chill)|(high wind)|(strong wind)|(marine)', df_stormData_raw$EVTYPE,ignore.case = TRUE, fixed = FALSE))

# reformat all indices to PRECIPITATION
df_stormData_raw$EVTYPE[indices] <- 'STRONG WIND'

#-------------------------------------------------------------------------------------------

#-------------------------------------------------------------------------------------------

# search with fuzzy match
unique(grep('urban', df_stormData_raw$EVTYPE, value = TRUE, ignore.case = TRUE, fixed = FALSE))

indices <- grep('urban', df_stormData_raw$EVTYPE, ignore.case = TRUE)

# reformat all indices to THUNDERSTORM WINDS
df_stormData_raw$EVTYPE[indices] <- 'HEAVY RAIN'

#-------------------------------------------------------------------------------------------

#-------------------------------------------------------------------------------------------

# search with fuzzy match
unique(agrep('wintry', df_stormData_raw$EVTYPE, value = TRUE, ignore.case = TRUE, fixed = FALSE))

indices <- agrep('wintry', df_stormData_raw$EVTYPE, ignore.case = TRUE)

# reformat all indices to WINTER WEATHER
df_stormData_raw$EVTYPE[indices] <- 'WINTER WEATHER'

#-------------------------------------------------------------------------------------------

#-------------------------------------------------------------------------------------------

# search with fuzzy match
unique(grep('urban', df_stormData_raw$EVTYPE, value = TRUE, ignore.case = TRUE, fixed = FALSE))

indices <- grep('urban', df_stormData_raw$EVTYPE, ignore.case = TRUE)

# reformat all indices to THUNDERSTORM WINDS
df_stormData_raw$EVTYPE[indices] <- 'HEAVY RAIN'

#-------------------------------------------------------------------------------------------

#removing set of rows 
df_stormData_raw <- df_stormData_raw[-grep("record",df_stormData_raw$EVTYPE, ignore.case = TRUE),]

#-------------------------------------------------------------------------------------------

# search with fuzzy match
unique(grep('tide|surge', df_stormData_raw$EVTYPE, value = TRUE, ignore.case = TRUE))

# exclude from the above results, string that contain particular values
setdiff(unique(grep('tide|surge|sea', df_stormData_raw$EVTYPE,value = TRUE, ignore.case = TRUE, fixed = FALSE)),
        unique(grep('(astronomical low)|(unseasonal)', df_stormData_raw$EVTYPE,value = TRUE, ignore.case = TRUE, fixed = FALSE)))

# assign to indices 
indices <- setdiff(grep('tide|surge|sea', df_stormData_raw$EVTYPE,ignore.case = TRUE, fixed = FALSE),
                   grep('(astronomical low)|(unseasonal)', df_stormData_raw$EVTYPE,ignore.case = TRUE, fixed = FALSE))

# reformat all indices to PRECIPITATION
df_stormData_raw$EVTYPE[indices] <- 'STORM SURGE/TIDE'

#-------------------------------------------------------------------------------------------

#-------------------------------------------------------------------------------------------

# search with fuzzy match
unique(grep('glaze|icy', df_stormData_raw$EVTYPE, value = TRUE, ignore.case = TRUE, fixed = FALSE))

indices <- grep('glaze|icy', df_stormData_raw$EVTYPE, ignore.case = TRUE)

# reformat all indices to WINTER WEATHER
df_stormData_raw$EVTYPE[indices] <- 'WINTER WEATHER'

#-------------------------------------------------------------------------------------------

#-------------------------------------------------------------------------------------------

# search with fuzzy match
unique(grep('snow', df_stormData_raw$EVTYPE, value = TRUE, ignore.case = TRUE))

# exclude from the above results, string that contain particular values
setdiff(unique(grep('snow', df_stormData_raw$EVTYPE,value = TRUE, ignore.case = TRUE, fixed = FALSE)),
        unique(grep('lake', df_stormData_raw$EVTYPE,value = TRUE, ignore.case = TRUE, fixed = FALSE)))

# assign to indices 
indices <- setdiff(grep('snow', df_stormData_raw$EVTYPE,ignore.case = TRUE, fixed = FALSE),
                   grep('lake', df_stormData_raw$EVTYPE,ignore.case = TRUE, fixed = FALSE))

# reformat all indices to PRECIPITATION
df_stormData_raw$EVTYPE[indices] <- 'HEAVY SNOW'

#-------------------------------------------------------------------------------------------


# search with fuzzy match
unique(grep('wave|swell', df_stormData_raw$EVTYPE, value = TRUE, ignore.case = TRUE, fixed = FALSE))

indices <- grep('wave|swell', df_stormData_raw$EVTYPE, ignore.case = TRUE)

# reformat all indices to WINTER WEATHER
df_stormData_raw$EVTYPE[indices] <- 'STORM SURGE/TIDE'

#-------------------------------------------------------------------------------------------

# search with fuzzy match
unique(grep('heavy lake', df_stormData_raw$EVTYPE, value = TRUE, ignore.case = TRUE, fixed = FALSE))

indices <- grep('heavy lake', df_stormData_raw$EVTYPE, ignore.case = TRUE)

# reformat all indices to WINTER WEATHER
df_stormData_raw$EVTYPE[indices] <- 'LAKE-EFFECT SNOW'

#-------------------------------------------------------------------------------------------

#-------------------------------------------------------------------------------------------

# search with fuzzy match
unique(grep('high', df_stormData_raw$EVTYPE, value = TRUE, ignore.case = TRUE))

# exclude from the above results, string that contain particular values
setdiff(unique(grep('high', df_stormData_raw$EVTYPE,value = TRUE, ignore.case = TRUE, fixed = FALSE)),
        unique(grep('marine|wind', df_stormData_raw$EVTYPE,value = TRUE, ignore.case = TRUE, fixed = FALSE)))

# assign to indices 
indices <- setdiff(grep('high', df_stormData_raw$EVTYPE,ignore.case = TRUE, fixed = FALSE),
                   grep('marine|world', df_stormData_raw$EVTYPE,ignore.case = TRUE, fixed = FALSE))

# reformat all indices to PRECIPITATION
df_stormData_raw$EVTYPE[indices] <- 'HIGH SURF'

#-------------------------------------------------------------------------------------------

#-------------------------------------------------------------------------------------------

# search with fuzzy match
unique(grep('landspout', df_stormData_raw$EVTYPE, value = TRUE, ignore.case = TRUE, fixed = FALSE))

indices <- grep('landspout', df_stormData_raw$EVTYPE, ignore.case = TRUE)

# reformat all indices to WINTER WEATHER
df_stormData_raw$EVTYPE[indices] <- 'TORNADO'

#-------------------------------------------------------------------------------------------

#-------------------------------------------------------------------------------------------

# search with fuzzy match
unique(agrep('hypothermia', df_stormData_raw$EVTYPE, value = TRUE, ignore.case = TRUE, fixed = FALSE))

indices <- agrep('hypothermia', df_stormData_raw$EVTYPE, ignore.case = TRUE)

# reformat all indices to WINTER WEATHER
df_stormData_raw$EVTYPE[indices] <- 'COLD/WIND CHILL'

#-------------------------------------------------------------------------------------------

#-------------------------------------------------------------------------------------------

# search with fuzzy match
unique(grep('vog', df_stormData_raw$EVTYPE, value = TRUE, ignore.case = TRUE, fixed = FALSE))

indices <- grep('vog', df_stormData_raw$EVTYPE, ignore.case = TRUE)

# reformat all indices to WINTER WEATHER
df_stormData_raw$EVTYPE[indices] <- 'VOLCANIC ASH'

#-------------------------------------------------------------------------------------------

#-------------------------------------------------------------------------------------------

# search with fuzzy match
unique(grep('high', df_stormData_raw$EVTYPE, value = TRUE, ignore.case = TRUE))

# exclude from the above results, string that contain particular values
setdiff(unique(agrep('other|none', df_stormData_raw$EVTYPE,value = TRUE, ignore.case = TRUE, fixed = FALSE)),
        unique(grep('winter|funnel', df_stormData_raw$EVTYPE,value = TRUE, ignore.case = TRUE, fixed = FALSE)))

# assign to indices 
indices <- setdiff(agrep('other|none', df_stormData_raw$EVTYPE,ignore.case = TRUE, fixed = FALSE),
                   grep('winter|funnel', df_stormData_raw$EVTYPE,ignore.case = TRUE, fixed = FALSE))

# reformat all indices to PRECIPITATION
df_stormData_raw$EVTYPE[indices] <- 'OTHER'

#-------------------------------------------------------------------------------------------

#-------------------------------------------------------------------------------------------

# search with fuzzy match
unique(agrep('stream', df_stormData_raw$EVTYPE, value = TRUE, ignore.case = TRUE, fixed = FALSE))

indices <- grep('stream', df_stormData_raw$EVTYPE, ignore.case = TRUE)

# reformat all indices to WINTER WEATHER
df_stormData_raw$EVTYPE[indices] <- 'HEAVY RAIN'

#-------------------------------------------------------------------------------------------

#-------------------------------------------------------------------------------------------

# search with fuzzy match
unique(grep('water$', df_stormData_raw$EVTYPE, value = TRUE, ignore.case = TRUE, fixed = FALSE))

indices <- grep('water$', df_stormData_raw$EVTYPE, ignore.case = TRUE)

# reformat all indices to WINTER WEATHER
df_stormData_raw$EVTYPE[indices] <- 'STORM SURGE/TIDE'

#-------------------------------------------------------------------------------------------

#-------------------------------------------------------------------------------------------

# search with fuzzy match
unique(grep('(south)|(turbulence)|(floyd)|(flag)|(pattern)|(metro)|(landslump)|\\?', df_stormData_raw$EVTYPE, value = TRUE, ignore.case = TRUE, fixed = FALSE))

indices <- grep('(south)|(turbulence)|(floyd)|(flag)|(pattern)|(metro)|(landslump)|\\?', df_stormData_raw$EVTYPE, ignore.case = TRUE)

# reformat all indices to WINTER WEATHER
df_stormData_raw$EVTYPE[indices] <- 'OTHER'

#-------------------------------------------------------------------------------------------

df_stormData_raw$EVTYPE <- as.factor(df_stormData_raw$EVTYPE)
