<table style="width: 100%;">
<tbody>
<tr class="odd">
<td>comprehendmedical_stop_snomedct_inference_job</td>
<td style="text-align: right;">R Documentation</td>
</tr>
</tbody>
</table>

## Stops an InferSNOMEDCT inference job in progress

### Description

Stops an InferSNOMEDCT inference job in progress.

### Usage

    comprehendmedical_stop_snomedct_inference_job(JobId)

### Arguments

<table>
<colgroup>
<col style="width: 35%" />
<col style="width: 65%" />
</colgroup>
<tbody>
<tr class="odd">
<td><code
id="comprehendmedical_stop_snomedct_inference_job_:_JobId">JobId</code></td>
<td><p>[required] The job id of the asynchronous InferSNOMEDCT job to be
stopped.</p></td>
</tr>
</tbody>
</table>

### Value

A list with the following syntax:

    list(
      JobId = "string"
    )

### Request syntax

    svc$stop_snomedct_inference_job(
      JobId = "string"
    )