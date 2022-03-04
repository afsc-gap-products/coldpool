/* Query to retrive bottom temperature data from AFSC/RACE/GAP bottom trawl surveys of the eastern Bering Sea continental shelf from RACEBASE. Includes index hauls with good performance. This subset of hauls is used for cold pool area calculations. 
Contact: Sean Rohan (sean.rohan@noaa.gov), AFSC/RACE/GAP
Updated: February 28, 2022 */
select e.gear_temperature,
e.surface_temperature,
e.start_latitude,
e.start_longitude,
e.end_latitude,
e.end_longitude,
e.stationid,
e.start_time,
e.stratum,
e.haul_type,
e.performance,
e.cruise, 
e.bottom_depth,
a.survey_definition_id
from racebase.haul e,
race_data.survey_definitions a, 
    race_data.surveys b,
    race_data.cruises c
where
e.region = 'BS'
and b.survey_definition_id = 98
and (e.stratum in (10,20,31,32,41,42,43,50,61,62,82,90)) 
and  e.cruise > 198200 
and e.cruise = c.cruise 
and a.survey_definition_id = b.survey_definition_id
and b.survey_id = c.survey_id
and e.performance >= 0 
and e.bottom_depth < 201
and c.vessel_id = e.vessel
and (e.haul_type in (3, 13))