/* Query to retrive preliminary temperature data from AFSC/RACE/GAP bottom trawl surveys of the eastern Bering Sea continental shelf from RACEBASE. Includes index hauls with good performance. This subset of hauls is used for cold pool area calculations. 
Contact: Sean Rohan (sean.rohan@noaa.gov), AFSC/RACE/GAP
Updated: September 7, 2022 */
select e.edit_gear_temperature gear_temperature,
e.edit_surface_temperature surface_temperature,
f.edit_latitude latitude_dms,
f.edit_longitude longitude_dms,
e.station stationid,
e.stratum,
e.haul_type,
e.performance,
e.cruise_id,
e.edit_bottom_depth bottom_depth,
c.cruise,
f.event_type_id,
a.survey_definition_id
from race_data.edit_hauls e,
race_data.survey_definitions a, 
race_data.surveys b,
race_data.cruises c,
race_data.edit_events f
where b.survey_definition_id in (78, 98, 143)
and (e.stratum in (10,20,31,32,41,42,43,50,61,62,82,90, 70,71,81)) 
and c.cruise > 202200
and e.cruise_id = c.cruise_id 
and a.survey_definition_id = b.survey_definition_id
and b.survey_id = c.survey_id
and e.performance >= 0 
and e.edit_bottom_depth < 201
and (e.haul_type in (3, 13))
and f.haul_id = e.haul_id
and f.event_type_id = 9