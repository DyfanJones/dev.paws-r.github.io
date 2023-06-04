<table style="width: 100%;">
<tbody>
<tr class="odd">
<td>comprehend_describe_events_detection_job</td>
<td style="text-align: right;">R Documentation</td>
</tr>
</tbody>
</table>

## Gets the status and details of an events detection job

### Description

Gets the status and details of an events detection job.

### Usage

    comprehend_describe_events_detection_job(JobId)

### Arguments

<table>
<colgroup>
<col style="width: 35%" />
<col style="width: 65%" />
</colgroup>
<tbody>
<tr class="odd">
<td><code
id="comprehend_describe_events_detection_job_:_JobId">JobId</code></td>
<td><p>[required] The identifier of the events detection job.</p></td>
</tr>
</tbody>
</table>

### Value

A list with the following syntax:

    list(
      EventsDetectionJobProperties = list(
        JobId = "string",
        JobArn = "string",
        JobName = "string",
        JobStatus = "SUBMITTED"|"IN_PROGRESS"|"COMPLETED"|"FAILED"|"STOP_REQUESTED"|"STOPPED",
        Message = "string",
        SubmitTime = as.POSIXct(
          "2015-01-01"
        ),
        EndTime = as.POSIXct(
          "2015-01-01"
        ),
        InputDataConfig = list(
          S3Uri = "string",
          InputFormat = "ONE_DOC_PER_FILE"|"ONE_DOC_PER_LINE",
          DocumentReaderConfig = list(
            DocumentReadAction = "TEXTRACT_DETECT_DOCUMENT_TEXT"|"TEXTRACT_ANALYZE_DOCUMENT",
            DocumentReadMode = "SERVICE_DEFAULT"|"FORCE_DOCUMENT_READ_ACTION",
            FeatureTypes = list(
              "TABLES"|"FORMS"
            )
          )
        ),
        OutputDataConfig = list(
          S3Uri = "string",
          KmsKeyId = "string"
        ),
        LanguageCode = "en"|"es"|"fr"|"de"|"it"|"pt"|"ar"|"hi"|"ja"|"ko"|"zh"|"zh-TW",
        DataAccessRoleArn = "string",
        TargetEventTypes = list(
          "string"
        )
      )
    )

### Request syntax

    svc$describe_events_detection_job(
      JobId = "string"
    )