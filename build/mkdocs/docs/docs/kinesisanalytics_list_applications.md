<table style="width: 100%;">
<tbody>
<tr class="odd">
<td>kinesisanalytics_list_applications</td>
<td style="text-align: right;">R Documentation</td>
</tr>
</tbody>
</table>

## This documentation is for version 1 of the Amazon Kinesis Data Analytics API, which only supports SQL applications

### Description

This documentation is for version 1 of the Amazon Kinesis Data Analytics
API, which only supports SQL applications. Version 2 of the API supports
SQL and Java applications. For more information about version 2, see
Amazon Kinesis Data Analytics API V2 Documentation.

Returns a list of Amazon Kinesis Analytics applications in your account.
For each application, the response includes the application name, Amazon
Resource Name (ARN), and status. If the response returns the
`HasMoreApplications` value as true, you can send another request by
adding the `ExclusiveStartApplicationName` in the request body, and set
the value of this to the last application name from the previous
response.

If you want detailed information about a specific application, use
`describe_application`.

This operation requires permissions to perform the
`kinesisanalytics:ListApplications` action.

### Usage

    kinesisanalytics_list_applications(Limit, ExclusiveStartApplicationName)

### Arguments

<table>
<colgroup>
<col style="width: 35%" />
<col style="width: 65%" />
</colgroup>
<tbody>
<tr class="odd">
<td><code
id="kinesisanalytics_list_applications_:_Limit">Limit</code></td>
<td><p>Maximum number of applications to list.</p></td>
</tr>
<tr class="even">
<td><code
id="kinesisanalytics_list_applications_:_ExclusiveStartApplicationName">ExclusiveStartApplicationName</code></td>
<td><p>Name of the application to start the list with. When using
pagination to retrieve the list, you don't need to specify this
parameter in the first request. However, in subsequent requests, you add
the last application name from the previous response to get the next
page of applications.</p></td>
</tr>
</tbody>
</table>

### Value

A list with the following syntax:

    list(
      ApplicationSummaries = list(
        list(
          ApplicationName = "string",
          ApplicationARN = "string",
          ApplicationStatus = "DELETING"|"STARTING"|"STOPPING"|"READY"|"RUNNING"|"UPDATING"
        )
      ),
      HasMoreApplications = TRUE|FALSE
    )

### Request syntax

    svc$list_applications(
      Limit = 123,
      ExclusiveStartApplicationName = "string"
    )