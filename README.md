# Wisconsin Schools Database

The purpose of this database is to provide easy access to school data across all sectors of schools in Wisconsin.  The current method by which the Wisconsin Department of Public Instruction makes available school data is not aligned across all school agency types -- most notably, private school data is reported through a separate process, and the format in which it is made available requires a significant amount of cleaning and reformatting to make meaningful comparisons with public schools.

This database automates the cleaning and reformatting process, which saves time and increases the integrity of analyses from year to year since the analyst can be certain the same data manipulation is being done from year to year.

The resulting data tables are exported to the [wisconsink12](https://github.com/cityforwardcollective/wisconsink12) R package.

## Data Sources

* Enrollment
    - [Public Schools](https://dpi.wi.gov/wisedash/download-files/type?field_wisedash_upload_type_value=Enrollment)
    - [Private Schools](https://dpi.wi.gov/wisedash/download-files/type?field_wisedash_upload_type_value=Enrollment-Private-School)
* Choice Counts
    - [All Programs 2016-17 SY to Present](https://dpi.wi.gov/sms/choice-programs/data)
    - [Earlier MPCP Counts](https://dpi.wi.gov/parental-education-options/choice-programs/data/mpcp-historical)
* [Report Cards](https://apps2.dpi.wi.gov/reportcards/)
* High School Completion
    - [Public School](https://dpi.wi.gov/wisedash/download-files/type?field_wisedash_upload_type_value=hs-completion)
    - [Private School](https://dpi.wi.gov/wisedash/download-files/type?field_wisedash_upload_type_value=hs-completion-private-school)
* Wisconsin School Assessment System
    - [Public Schools Forward Exam](https://dpi.wi.gov/wisedash/download-files/type?field_wisedash_upload_type_value=Forward)
    - [Public Schools ACT](https://dpi.wi.gov/wisedash/download-files/type?field_wisedash_upload_type_value=ACT11)
    - [Private Schools Forward Exam and ACT](https://dpi.wi.gov/assessment/parental-choice-program/data)
* [Open Enrollment](https://dpi.wi.gov/open-enrollment/data/aid-adjustments)
* [Chapter 220](https://dpi.wi.gov/sfs/aid/general/integration-220/overview)
* [Attendance](https://dpi.wi.gov/wisedash/download-files/type?field_wisedash_upload_type_value=Attendance)
* [Suspension](https://dpi.wi.gov/wisedash/download-files/type?field_wisedash_upload_type_value=Discipline)

