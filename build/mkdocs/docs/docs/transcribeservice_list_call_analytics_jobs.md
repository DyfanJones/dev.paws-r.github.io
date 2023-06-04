<table style="width: 100%;">
<tbody>
<tr class="odd">
<td>transcribeservice_list_call_analytics_jobs</td>
<td style="text-align: right;">R Documentation</td>
</tr>
</tbody>
</table>

## Provides a list of Call Analytics jobs that match the specified criteria

### Description

Provides a list of Call Analytics jobs that match the specified
criteria. If no criteria are specified, all Call Analytics jobs are
returned.

To get detailed information about a specific Call Analytics job, use the
operation.

### Usage

    transcribeservice_list_call_analytics_jobs(Status, JobNameContains,
      NextToken, MaxResults)

### Arguments

<table>
<colgroup>
<col style="width: 35%" />
<col style="width: 65%" />
</colgroup>
<tbody>
<tr class="odd">
<td><code
id="transcribeservice_list_call_analytics_jobs_:_Status">Status</code></td>
<td><p>Returns only Call Analytics jobs with the specified status. Jobs
are ordered by creation date, with the newest job first. If you don't
include <code>Status</code>, all Call Analytics jobs are
returned.</p></td>
</tr>
<tr class="even">
<td><code
id="transcribeservice_list_call_analytics_jobs_:_JobNameContains">JobNameContains</code></td>
<td><p>Returns only the Call Analytics jobs that contain the specified
string. The search is not case sensitive.</p></td>
</tr>
<tr class="odd">
<td><code
id="transcribeservice_list_call_analytics_jobs_:_NextToken">NextToken</code></td>
<td><p>If your <code>list_call_analytics_jobs</code> request returns
more results than can be displayed, <code>NextToken</code> is displayed
in the response with an associated string. To get the next page of
results, copy this string and repeat your request, including
<code>NextToken</code> with the value of the copied string. Repeat as
needed to view all your results.</p></td>
</tr>
<tr class="even">
<td><code
id="transcribeservice_list_call_analytics_jobs_:_MaxResults">MaxResults</code></td>
<td><p>The maximum number of Call Analytics jobs to return in each page
of results. If there are fewer results than the value that you specify,
only the actual results are returned. If you don't specify a value, a
default of 5 is used.</p></td>
</tr>
</tbody>
</table>

### Value

A list with the following syntax:

    list(
      Status = "QUEUED"|"IN_PROGRESS"|"FAILED"|"COMPLETED",
      NextToken = "string",
      CallAnalyticsJobSummaries = list(
        list(
          CallAnalyticsJobName = "string",
          CreationTime = as.POSIXct(
            "2015-01-01"
          ),
          StartTime = as.POSIXct(
            "2015-01-01"
          ),
          CompletionTime = as.POSIXct(
            "2015-01-01"
          ),
          LanguageCode = "af-ZA"|"ar-AE"|"ar-SA"|"da-DK"|"de-CH"|"de-DE"|"en-AB"|"en-AU"|"en-GB"|"en-IE"|"en-IN"|"en-US"|"en-WL"|"es-ES"|"es-US"|"fa-IR"|"fr-CA"|"fr-FR"|"he-IL"|"hi-IN"|"id-ID"|"it-IT"|"ja-JP"|"ko-KR"|"ms-MY"|"nl-NL"|"pt-BR"|"pt-PT"|"ru-RU"|"ta-IN"|"te-IN"|"tr-TR"|"zh-CN"|"zh-TW"|"th-TH"|"en-ZA"|"en-NZ"|"vi-VN"|"sv-SE",
          CallAnalyticsJobStatus = "QUEUED"|"IN_PROGRESS"|"FAILED"|"COMPLETED",
          FailureReason = "string"
        )
      )
    )

### Request syntax

    svc$list_call_analytics_jobs(
      Status = "QUEUED"|"IN_PROGRESS"|"FAILED"|"COMPLETED",
      JobNameContains = "string",
      NextToken = "string",
      MaxResults = 123
    )