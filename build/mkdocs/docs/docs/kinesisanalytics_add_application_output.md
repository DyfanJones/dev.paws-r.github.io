<table style="width: 100%;">
<tbody>
<tr class="odd">
<td>kinesisanalytics_add_application_output</td>
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

Adds an external destination to your Amazon Kinesis Analytics
application.

If you want Amazon Kinesis Analytics to deliver data from an
in-application stream within your application to an external destination
(such as an Amazon Kinesis stream, an Amazon Kinesis Firehose delivery
stream, or an AWS Lambda function), you add the relevant configuration
to your application using this operation. You can configure one or more
outputs for your application. Each output configuration maps an
in-application stream and an external destination.

You can use one of the output configurations to deliver data from your
in-application error stream to an external destination so that you can
analyze the errors. For more information, see [Understanding Application
Output
(Destination)](https://docs.aws.amazon.com/kinesisanalytics/latest/dev/how-it-works-output.html).

Any configuration update, including adding a streaming source using this
operation, results in a new version of the application. You can use the
`describe_application` operation to find the current application
version.

For the limits on the number of application inputs and outputs you can
configure, see
[Limits](https://docs.aws.amazon.com/kinesisanalytics/latest/dev/limits.html).

This operation requires permissions to perform the
`kinesisanalytics:AddApplicationOutput` action.

### Usage

    kinesisanalytics_add_application_output(ApplicationName,
      CurrentApplicationVersionId, Output)

### Arguments

<table>
<colgroup>
<col style="width: 35%" />
<col style="width: 65%" />
</colgroup>
<tbody>
<tr class="odd">
<td><code
id="kinesisanalytics_add_application_output_:_ApplicationName">ApplicationName</code></td>
<td><p>[required] Name of the application to which you want to add the
output configuration.</p></td>
</tr>
<tr class="even">
<td><code
id="kinesisanalytics_add_application_output_:_CurrentApplicationVersionId">CurrentApplicationVersionId</code></td>
<td><p>[required] Version of the application to which you want to add
the output configuration. You can use the
<code>describe_application</code> operation to get the current
application version. If the version specified is not the current
version, the <code>ConcurrentModificationException</code> is
returned.</p></td>
</tr>
<tr class="odd">
<td><code
id="kinesisanalytics_add_application_output_:_Output">Output</code></td>
<td><p>[required] An array of objects, each describing one output
configuration. In the output configuration, you specify the name of an
in-application stream, a destination (that is, an Amazon Kinesis stream,
an Amazon Kinesis Firehose delivery stream, or an AWS Lambda function),
and record the formation to use when writing to the
destination.</p></td>
</tr>
</tbody>
</table>

### Value

An empty list.

### Request syntax

    svc$add_application_output(
      ApplicationName = "string",
      CurrentApplicationVersionId = 123,
      Output = list(
        Name = "string",
        KinesisStreamsOutput = list(
          ResourceARN = "string",
          RoleARN = "string"
        ),
        KinesisFirehoseOutput = list(
          ResourceARN = "string",
          RoleARN = "string"
        ),
        LambdaOutput = list(
          ResourceARN = "string",
          RoleARN = "string"
        ),
        DestinationSchema = list(
          RecordFormatType = "JSON"|"CSV"
        )
      )
    )