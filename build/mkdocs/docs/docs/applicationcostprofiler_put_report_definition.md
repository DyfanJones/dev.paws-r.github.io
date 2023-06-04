<table style="width: 100%;">
<tbody>
<tr class="odd">
<td>applicationcostprofiler_put_report_definition</td>
<td style="text-align: right;">R Documentation</td>
</tr>
</tbody>
</table>

## Creates the report definition for a report in Application Cost Profiler

### Description

Creates the report definition for a report in Application Cost Profiler.

### Usage

    applicationcostprofiler_put_report_definition(reportId,
      reportDescription, reportFrequency, format, destinationS3Location)

### Arguments

<table>
<colgroup>
<col style="width: 35%" />
<col style="width: 65%" />
</colgroup>
<tbody>
<tr class="odd">
<td><code
id="applicationcostprofiler_put_report_definition_:_reportId">reportId</code></td>
<td><p>[required] Required. ID of the report. You can choose any valid
string matching the pattern for the ID.</p></td>
</tr>
<tr class="even">
<td><code
id="applicationcostprofiler_put_report_definition_:_reportDescription">reportDescription</code></td>
<td><p>[required] Required. Description of the report.</p></td>
</tr>
<tr class="odd">
<td><code
id="applicationcostprofiler_put_report_definition_:_reportFrequency">reportFrequency</code></td>
<td><p>[required] Required. The cadence to generate the report.</p></td>
</tr>
<tr class="even">
<td><code
id="applicationcostprofiler_put_report_definition_:_format">format</code></td>
<td><p>[required] Required. The format to use for the generated
report.</p></td>
</tr>
<tr class="odd">
<td><code
id="applicationcostprofiler_put_report_definition_:_destinationS3Location">destinationS3Location</code></td>
<td><p>[required] Required. Amazon Simple Storage Service (Amazon S3)
location where Application Cost Profiler uploads the report.</p></td>
</tr>
</tbody>
</table>

### Value

A list with the following syntax:

    list(
      reportId = "string"
    )

### Request syntax

    svc$put_report_definition(
      reportId = "string",
      reportDescription = "string",
      reportFrequency = "MONTHLY"|"DAILY"|"ALL",
      format = "CSV"|"PARQUET",
      destinationS3Location = list(
        bucket = "string",
        prefix = "string"
      )
    )