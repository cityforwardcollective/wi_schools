Report Cards Table Data Dictionary
================
Spencer Schien
Last updated 2020-04-07

## Description

The `report_cards` table contains data made available through DPI’s
[School Report Card Data Download
Files](https://apps2.dpi.wi.gov/reportcards/). A few manipulations have
been performed on the data to facilitate analysis:

  - Variable names have been cleaned so they are shorter, they include
    only lower case characters, and they have no spaces
  - District and school codes have been replaced by the `dpi_true_id`
    variable, which standardizes the codes across the years (even if
    codes didn’t change, the padding of codes was not consistent in
    DPI’s reports, where a code might be 400 one year and 0400
    another)
  - Many variables in DPI’s report have been removed because they are
    superfluous (e.g. state comparison scores)
  - Two columns have been added to facilitate filters – these are the
    `cg_redacted` (i.e. Closing Gaps is redacted in 2019-20) and
    `has_2_rc` (i.e. Choice school has two report cards for the given
    year)

## Dictionary

The table below contains the data dictionary for the `report_cards`
table. This dictionary is also provided in CSV form in this folder ([raw
here](https://raw.githubusercontent.com/cityforwardcollective/wi_schools/master/data_dictionaries/report_cards/report_cards_data_dictionary.csv)).

| variable             | description                                                                                                                              | data\_transformation                                                                                                |
| :------------------- | :--------------------------------------------------------------------------------------------------------------------------------------- | :------------------------------------------------------------------------------------------------------------------ |
| school\_year         | School year.                                                                                                                             | None                                                                                                                |
| dpi\_true\_id        | Unique school identifier                                                                                                                 | Concatenation of the District and School ID designated by DPI. Private Schools are given the District ID of ‘0000’. |
| overall\_score       | School Report Card Overall Score                                                                                                         | None                                                                                                                |
| overall\_rating      | School Report Card Overall Rating                                                                                                        | None                                                                                                                |
| lowest\_grade        | Lowest grade served by the school                                                                                                        | None                                                                                                                |
| highest\_grade       | Highest grade served by the school                                                                                                       | None                                                                                                                |
| grade\_band          | Grade band for comparison schools                                                                                                        | None                                                                                                                |
| school\_type         | School type (e.g. Elementary, High School, etc.)                                                                                         | None                                                                                                                |
| school\_enrollment   | School enrollment                                                                                                                        | None                                                                                                                |
| district\_enrollment | District enrollment                                                                                                                      | None                                                                                                                |
| per\_am\_in          | Percent American Indian or Alaskan Native students                                                                                       | None                                                                                                                |
| per\_asian           | Percent Asian students                                                                                                                   | None                                                                                                                |
| per\_b\_aa           | Percent black or African American students                                                                                               | None                                                                                                                |
| per\_hisp\_lat       | Percent Hispanic or Latino students                                                                                                      | None                                                                                                                |
| per\_nh\_opi         | Percent native Hawaiian or other Pacific Islander students                                                                               | None                                                                                                                |
| per\_white           | Percent White not Hispanic students                                                                                                      | None                                                                                                                |
| per\_tom             | Percent two or more races                                                                                                                | None                                                                                                                |
| per\_swd             | Percent students with disabilities                                                                                                       | None                                                                                                                |
| per\_ed              | Percent economically disadvantaged students                                                                                              | None                                                                                                                |
| per\_lep             | Percent limited English proficient students                                                                                              | None                                                                                                                |
| per\_choice          | Percent students attending under the choice program                                                                                      | None                                                                                                                |
| per\_open            | Percent students attending under open enrollment                                                                                         | None                                                                                                                |
| sch\_ach             | School Student Achievement Score                                                                                                         | None                                                                                                                |
| sch\_ela\_ach        | School Student Achievement ELA Component Score                                                                                           | None                                                                                                                |
| sch\_math\_ach       | School Student Achievement Math Component Score                                                                                          | None                                                                                                                |
| sch\_growth          | School Student Growth Score                                                                                                              | None                                                                                                                |
| sch\_ela\_growth     | School Student Growth ELA Component Score                                                                                                | None                                                                                                                |
| sch\_math\_growth    | School Student Growth Math Component Score                                                                                               | None                                                                                                                |
| sch\_cg              | School Closing Gaps Score                                                                                                                | None                                                                                                                |
| sch\_ela\_cg         | School Closing Gaps ELA Component Score                                                                                                  | None                                                                                                                |
| sch\_math\_cg        | School Closing Gaps Math Conponent Score                                                                                                 | None                                                                                                                |
| sch\_4y\_grad\_gap   | School 4 Year Graduation Gap Score                                                                                                       | None                                                                                                                |
| sch\_6y\_grad\_gap   | School 6 Year Graduation Gap Score                                                                                                       | None                                                                                                                |
| sch\_grad\_gap       | School Graduation Gap Score                                                                                                              | None                                                                                                                |
| sch\_ot              | School On-Track and Postsecondary Readiness Score                                                                                        | None                                                                                                                |
| sch\_grad\_rate      | School Graduation Rate Score                                                                                                             | None                                                                                                                |
| sch\_att\_rate       | School Attendance Rate Score                                                                                                             | None                                                                                                                |
| sch\_3rd\_ela        | School Third-Grade ELA Achievement Score                                                                                                 | None                                                                                                                |
| sch\_8th\_math       | School Eighth-Grade Math Achievement Score                                                                                               | None                                                                                                                |
| ach\_weight          | Score weighting Achievement Priority Area                                                                                                | None                                                                                                                |
| growth\_weight       | Score weighting Growth Priotity Area                                                                                                     | None                                                                                                                |
| cg\_weight           | Score weighting Closing Gaps Priority Area                                                                                               | None                                                                                                                |
| ot\_weight           | Score weighting On-Track Priority Area                                                                                                   | None                                                                                                                |
| report\_card\_type   | Specifies between Public - All Students, Private - Choice Students, or Private - All Students                                            | None                                                                                                                |
| cg\_redacted         | Designates whether the School Closing Gaps score was removed in 2018-19 due to fluctuation from previous year greater than twenty points | Boolean determined by presence of ’\*’ in School Closing Gaps Score                                                 |
| has\_2\_rc           | Designates whether private school has only a Choice Students Report Card or also has an All Students Report Card                         | Boolean determined by presence of more than one Report Card for a school in a year                                  |
