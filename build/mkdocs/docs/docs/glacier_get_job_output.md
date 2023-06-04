<table style="width: 100%;">
<tbody>
<tr class="odd">
<td>glacier_get_job_output</td>
<td style="text-align: right;">R Documentation</td>
</tr>
</tbody>
</table>

## This operation downloads the output of the job you initiated using InitiateJob

### Description

This operation downloads the output of the job you initiated using
`initiate_job`. Depending on the job type you specified when you
initiated the job, the output will be either the content of an archive
or a vault inventory.

You can download all the job output or download a portion of the output
by specifying a byte range. In the case of an archive retrieval job,
depending on the byte range you specify, Amazon S3 Glacier (Glacier)
returns the checksum for the portion of the data. You can compute the
checksum on the client and verify that the values match to ensure the
portion you downloaded is the correct data.

A job ID will not expire for at least 24 hours after Glacier completes
the job. That a byte range. For both archive and inventory retrieval
jobs, you should verify the downloaded size against the size returned in
the headers from the **Get Job Output** response.

For archive retrieval jobs, you should also verify that the size is what
you expected. If you download a portion of the output, the expected size
is based on the range of bytes you specified. For example, if you
specify a range of `bytes=0-1048575`, you should verify your download
size is 1,048,576 bytes. If you download an entire archive, the expected
size is the size of the archive when you uploaded it to Amazon S3
Glacier The expected size is also returned in the headers from the **Get
Job Output** response.

In the case of an archive retrieval job, depending on the byte range you
specify, Glacier returns the checksum for the portion of the data. To
ensure the portion you downloaded is the correct data, compute the
checksum on the client, verify that the values match, and verify that
the size is what you expected.

A job ID does not expire for at least 24 hours after Glacier completes
the job. That is, you can download the job output within the 24 hours
period after Amazon Glacier completes the job.

An AWS account has full permission to perform all operations (actions).
However, AWS Identity and Access Management (IAM) users don't have any
permissions by default. You must grant them explicit permission to
perform specific actions. For more information, see [Access Control
Using AWS Identity and Access Management
(IAM)](https://docs.aws.amazon.com/amazonglacier/latest/dev/security-iam.html).

For conceptual information and the underlying REST API, see [Downloading
a Vault
Inventory](https://docs.aws.amazon.com/amazonglacier/latest/dev/vault-inventory.html),
[Downloading an
Archive](https://docs.aws.amazon.com/amazonglacier/latest/dev/downloading-an-archive.html),
and [Get Job
Output](https://docs.aws.amazon.com/amazonglacier/latest/dev/api-job-output-get.html)

### Usage

    glacier_get_job_output(accountId, vaultName, jobId, range)

### Arguments

<table>
<colgroup>
<col style="width: 35%" />
<col style="width: 65%" />
</colgroup>
<tbody>
<tr class="odd">
<td><code id="glacier_get_job_output_:_accountId">accountId</code></td>
<td><p>[required] The <code>AccountId</code> value is the AWS account ID
of the account that owns the vault. You can either specify an AWS
account ID or optionally a single '<code>-</code>' (hyphen), in which
case Amazon S3 Glacier uses the AWS account ID associated with the
credentials used to sign the request. If you use an account ID, do not
include any hyphens ('-') in the ID.</p></td>
</tr>
<tr class="even">
<td><code id="glacier_get_job_output_:_vaultName">vaultName</code></td>
<td><p>[required] The name of the vault.</p></td>
</tr>
<tr class="odd">
<td><code id="glacier_get_job_output_:_jobId">jobId</code></td>
<td><p>[required] The job ID whose data is downloaded.</p></td>
</tr>
<tr class="even">
<td><code id="glacier_get_job_output_:_range">range</code></td>
<td><p>The range of bytes to retrieve from the output. For example, if
you want to download the first 1,048,576 bytes, specify the range as
<code>bytes=0-1048575</code>. By default, this operation downloads the
entire output.</p>
<p>If the job output is large, then you can use a range to retrieve a
portion of the output. This allows you to download the entire output in
smaller chunks of bytes. For example, suppose you have 1 GB of job
output you want to download and you decide to download 128 MB chunks of
data at a time, which is a total of eight Get Job Output requests. You
use the following process to download the job output:</p>
<ol>
<li><p>Download a 128 MB chunk of output by specifying the appropriate
byte range. Verify that all 128 MB of data was received.</p></li>
<li><p>Along with the data, the response includes a SHA256 tree hash of
the payload. You compute the checksum of the payload on the client and
compare it with the checksum you received in the response to ensure you
received all the expected data.</p></li>
<li><p>Repeat steps 1 and 2 for all the eight 128 MB chunks of output
data, each time specifying the appropriate byte range.</p></li>
<li><p>After downloading all the parts of the job output, you have a
list of eight checksum values. Compute the tree hash of these values to
find the checksum of the entire output. Using the
<code>describe_job</code> API, obtain job information of the job that
provided you the output. The response includes the checksum of the
entire archive stored in Amazon S3 Glacier. You compare this value with
the checksum you computed to ensure you have downloaded the entire
archive content with no errors.</p></li>
</ol></td>
</tr>
</tbody>
</table>

### Value

A list with the following syntax:

    list(
      body = raw,
      checksum = "string",
      status = 123,
      contentRange = "string",
      acceptRanges = "string",
      contentType = "string",
      archiveDescription = "string"
    )

### Request syntax

    svc$get_job_output(
      accountId = "string",
      vaultName = "string",
      jobId = "string",
      range = "string"
    )

### Examples

    ## Not run: 
    # The example downloads the output of a previously initiated inventory
    # retrieval job that is identified by the job ID.
    svc$get_job_output(
      accountId = "-",
      jobId = "zbxcm3Z_3z5UkoroF7SuZKrxgGoDc3RloGduS7Eg-RO47Yc6FxsdGBgf_Q2DK5Ej...",
      range = "",
      vaultName = "my-vaul"
    )

    ## End(Not run)