*FILENAME REFFILE 'C:/Users/Pablo/Desktop/DS 6372 Applied Statistics - Inference and Modeling/Project 2/allKobeData_30692startsPreds.csv';
FILENAME REFFILE './desktop/kg6372/modelingKobeData.csv';
PROC IMPORT DATAFILE=REFFILE
DBMS=CSV
OUT=dfkobe;
GETNAMES=YES;
run;
proc sort data = dfkobe;
by shot_made_flag;
run;
proc print data = dfkobe(obs=10);
run;
/* we will need to have indicator variables for wins and losses; a win column with 1 or 0 and a loss column with 1 or 0. mutually exclusive */
/* check assumptions; break these up into multiple variable plots */
proc sgscatter data=dfkobe;
by shot_made_flag;
matrix season seconds_remaining shot_distance team_id game_date shot_id attendance arena_temp avgnoisedb;
run;
proc sgscatter data=dfkobe;
by shot_made_flag;
matrix season seconds_remaining shot_distance team_id game_date shot_id attendance arena_temp avgnoisedb recId game_event_id game_id ;
run;
proc sgscatter data=dfkobe;
by shot_made_flag;
matrix lat loc_x loc_y lon minutes_remaining period playoffs;
run;
proc sgscatter data=dfkobe;
by shot_made_flag;
matrix recId game_event_id game_id lat loc_x loc_y lon minutes_remaining period playoffs season seconds_remaining shot_distance team_id game_date shot_id attendance arena_temp avgnoisedb;
run;
/* check out Wilks' Lambda to see if there is a difference between at least one mean in the response */
proc glm data=dfkobe;
class shot_made_flag;
model recId game_event_id game_id lat loc_x loc_y lon minutes_remaining period playoffs season seconds_remaining shot_distance team_id game_date shot_id attendance arena_temp avgnoisedb = shot_made_flag;
manova H = shot_made_flag/printe printh summary;
run;

title 'Linear Discriminant Analysis - Pool Covariance Matrices Across Levels in the Class Variable (pool=yes)';
data dfmade dfpredict; set dfkobe;
if recId >= 30692 then output dfpredict;
if obs < 30692 then output dfmade;
run;
/* LDA and proc discrim requires continuous data only. shot_type, shot_zone_area, shot_zone_basic, and shot_zone_range are all explained by 
spatial data (lat, loc_x, loc_y, and lon) so will be dropped from the predictors here. Also, since Kobe only ever played for the Lakers, 
team_name will be dropped as well. action_type, combined_shot_type, matchup, and opponent will be dropped as well. Season is dropped for game_date */
/* pool=test to show Bartlett's Chi-Square test result for homogeneity to determine if linear or quadratic analsyis would be better
   for this classification. Currently, the p-value for that test is significant, indicating that Quadratic discriminate analysis is better. */
proc discrim data=dfmade pool=test crossvalidate
			 testData=dfpredict testout=discrimOut;
class shot_made_flag;
var recId game_event_id game_id lat loc_x loc_y lon minutes_remaining period playoffs season seconds_remaining shot_distance team_id game_date shot_id attendance arena_temp avgnoisedb;
priors "0" = 0.552302266 "1" = 0.447697734;
run;

/* This is the LDA portion. Because Bartlett's indicated Quadratic Discriminate Analysis would be best, "pool=no" to achieve this: */
proc discrim data=dfmade pool=no crossvalidate
			 testData=dfpredict testout=discrimOut;
class shot_made_flag;
var recId game_event_id game_id lat loc_x loc_y lon minutes_remaining period playoffs season seconds_remaining shot_distance team_id game_date shot_id attendance arena_temp avgnoisedb;
priors "0" = 0.552302266 "1" = 0.447697734;
run;

proc print data=discrimOut;
run;
