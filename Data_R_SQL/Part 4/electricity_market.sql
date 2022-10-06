/*
 ISDS 570 Data Transformation for Business - Electricity Market Case
 Dr. Kalczynski

 https://www.postgresql.org/docs/current/static/sql-expressions.html
 https://www.w3schools.com/SQL/

*/

---------------------------------------------------------------------------
-- First, create the electricitymarket database and open this file --------
---------------------------------------------------------------------------

-----------------------------------------------------------
-- Let us analyze the csv files and create a table --------
-----------------------------------------------------------

-- Make sure to carefully select the PK

/*
-- LIFELINE
-- DROP TABLE public.load;

CREATE TABLE public.load
(
    time_stamp timestamp without time zone NOT NULL,
    time_zone character varying(3) COLLATE pg_catalog."default" NOT NULL,
    node_name character varying(255) COLLATE pg_catalog."default",
    ptid bigint NOT NULL,
    load real,
    CONSTRAINT load_pkey PRIMARY KEY (time_stamp, time_zone, ptid)
)
WITH (
    OIDS = FALSE
)
TABLESPACE pg_default;

ALTER TABLE public.load
    OWNER to postgres;
*/

-------------------------------------------
-- Create a role for the database  --------
-------------------------------------------
-- rolename: electricitymarketwriter
-- password: write123

/*
-- LIFELINE:
-- REVOKE ALL PRIVILEGES ON ALL TABLES IN SCHEMA public FROM electricitymarketwriter;
-- DROP USER electricitymarketwriter;

CREATE USER electricitymarketwriter WITH
	LOGIN
	NOSUPERUSER
	NOCREATEDB
	NOCREATEROLE
	INHERIT
	NOREPLICATION
	CONNECTION LIMIT -1
	PASSWORD 'write123';
*/

-- Grant all rights (on existing tables and views)
GRANT ALL ON ALL TABLES IN SCHEMA public TO electricitymarketwriter;

-- Grant all rights (for future tables and views)
ALTER DEFAULT PRIVILEGES IN SCHEMA public
   GRANT ALL ON TABLES TO electricitymarketwriter;

/*
******** RESTORE POINT electricitymarket1
In order to continue learning, you need to have all the previous steps completed
If you would like to review later (and you need to know how to complete these steps).
However, in the interest of time, I have created a backup of the current progress and 
will now demonstrate how to restore it.

IMPORTANT: remove the current database and recreate it to restore from backup
DO NOT RESTORE THE BACKUP TO THE postgres database!
*/


----------------------------------------------------------
-- Build an ETL for deltas (outside PostgreSQL)  --------
----------------------------------------------------------

-- Build a tool to extract data (new data added every 5 minutes)

-- Check 
-- TRUNCATE TABLE load
SELECT * FROM load LIMIT 10;
SELECT DISTINCT node_name FROM load;
SELECT node_name, COUNT(*) FROM load GROUP BY node_name;

-- Total load for Mar. 17th, 2022 for the entire market
SELECT time_stamp, time_zone, SUM(load) as total_load
FROM load 
WHERE time_stamp BETWEEN '2022-03-17 00:00:00' AND '2022-03-17 23:59:59'
GROUP BY time_stamp, time_zone
ORDER BY time_stamp


SELECT COUNT(*) FROM load;

-- Average hourly load for the entire market for all dates
-- https://www.postgresql.org/docs/10/static/functions-datetime.html

SELECT date_trunc('hour',time_stamp) as ymdh, AVG(total_load) as avg_load
FROM 
(SELECT time_stamp, time_zone, SUM(load) as total_load
FROM load 
WHERE time_stamp BETWEEN '2022-02-01 00:00:00' AND '2022-02-28 23:59:59'
GROUP BY time_stamp, time_zone) TL
GROUP BY date_trunc('hour',time_stamp)
ORDER BY ymdh;


