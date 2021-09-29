/* Query to retrive bottom temperature data from AFSC/RACE/GAP bottom trawl surveys of the eastern Bering Sea continental shelf from RACEBASE. Includes all haul and gear types (normal index stations, special project stations, and red king crab resample) and hauls with good and bad performance. This subset of hauls is not used for cold pool area calculations. 
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
start_time,
cruise, 
bottom_depth
from racebase.haul where
region = 'BS' and
(stratum in (10,20,31,32,41,42,43,50,61,62,82,90)) and 
cruise > 198200 and
bottom_depth < 201