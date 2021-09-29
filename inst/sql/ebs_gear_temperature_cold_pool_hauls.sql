/* Query to retrive bottom temperature data from AFSC/RACE/GAP bottom trawl surveys of the eastern Bering Sea continental shelf from RACEBASE. Includes index hauls with good performance. This subset of hauls is used for cold pool area calculations. 
Contact: Sean Rohan (sean.rohan@noaa.gov), AFSC/RACE/GAP
Updated: September 28, 2021 */
select gear_temperature,
surface_temperature,
start_latitude, 
start_longitude, 
end_latitude, 
end_longitude, 
stationid, 
stratum, 
haul_type, 
performance, 
cruise, 
bottom_depth 
from racebase.haul 
where region = 'BS' 
and (stratum in (10,20,31,32,41,42,43,50,61,62,82,90)) 
and  cruise > 198200 
and performance >= 0 
and bottom_depth < 201 
and haul_type = 3