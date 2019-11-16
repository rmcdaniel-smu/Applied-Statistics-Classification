/*Read in the data*/
/**********************************************************************************/
FILENAME REFFILE '/home/u38493344/Applied Stats/Project 2/modelingKobeData.csv';

PROC IMPORT DATAFILE=REFFILE
	DBMS=CSV
	OUT=kobeModel;
	getnames = yes;
RUN;
proc print data = kobeModel(obs=10);
run;

/*Create factors for variables*/
/**********************************************************************************/


/*Model with all variables -  no variable selection - want to take a look at initial findings*/
/**********************************************************************************/
proc logistic data = kobeModel descending;
	class action_type combined_shot_type shot_type shot_zone_area shot_zone_range opponent/ param=ref;
	model shot_made_flag(event='1') =  game_event_id lat loc_x loc_y lon minutes_remaining period playoffs season seconds_remaining
	shot_distance game_date shot_id attendance arena_temp avgnoisedb /ctable pprob = 0.5;
	output out = logisticOut predprobs=I p=predprob resdev=resdev reschi=pearres;
run;

proc freq data = logisticOut; 
tables shot_made_flag*_into_/nocol nopercent;
run;

/*Print out model residulas*/
/**********************************************************************************/
data logisticOutGraphs;
set logisticOut; 
obsno = _n_;
run;

proc plot data = logisticOutGraphs;
plot resdev*obsno;
plot pearres*obsno;
run;
quit;

/*Get model odds ratios*/
/**********************************************************************************/
data logisticOutExample;
set logisticOut;
drop _level_;
run;

title "Output probabilities for observations form proc logistic";
proc print data = logisticOutExample;
run;

/*Model Selection*/
/**********************************************************************************/
proc logistic data = kobeModel descending;
	class action_type combined_shot_type shot_type shot_zone_area shot_zone_range opponent/ param=ref;
	model shot_made_flag(event='1') =  game_event_id lat loc_x loc_y lon minutes_remaining period playoffs season seconds_remaining
	shot_distance game_date shot_id attendance arena_temp avgnoisedb /selection = stepwise;
	output out = logisticOut predprobs=I p=predprob resdev=resdev reschi=pearres;
run;
