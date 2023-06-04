<table style="width: 100%;">
<tbody>
<tr class="odd">
<td>cloudwatchlogs_tag_resource</td>
<td style="text-align: right;">R Documentation</td>
</tr>
</tbody>
</table>

## Assigns one or more tags (key-value pairs) to the specified CloudWatch Logs resource

### Description

Assigns one or more tags (key-value pairs) to the specified CloudWatch
Logs resource. Currently, the only CloudWatch Logs resources that can be
tagged are log groups and destinations.

Tags can help you organize and categorize your resources. You can also
use them to scope user permissions by granting a user permission to
access or change only resources with certain tag values.

Tags don't have any semantic meaning to Amazon Web Services and are
interpreted strictly as strings of characters.

You can use the `tag_resource` action with a resource that already has
tags. If you specify a new tag key for the alarm, this tag is appended
to the list of tags associated with the alarm. If you specify a tag key
that is already associated with the alarm, the new tag value that you
specify replaces the previous value for that tag.

You can associate as many as 50 tags with a CloudWatch Logs resource.

### Usage

    cloudwatchlogs_tag_resource(resourceArn, tags)

### Arguments

<table>
<colgroup>
<col style="width: 35%" />
<col style="width: 65%" />
</colgroup>
<tbody>
<tr class="odd">
<td><code
id="cloudwatchlogs_tag_resource_:_resourceArn">resourceArn</code></td>
<td><p>[required] The ARN of the resource that you're adding tags
to.</p>
<p>The ARN format of a log group is
<code>arn:aws:logs:Region:account-id:log-group:log-group-name </code></p>
<p>The ARN format of a destination is
<code>arn:aws:logs:Region:account-id:destination:destination-name </code></p>
<p>For more information about ARN format, see <a
href="https://docs.aws.amazon.com/AmazonCloudWatch/latest/logs/iam-access-control-overview-cwl.html">CloudWatch
Logs resources and operations</a>.</p></td>
</tr>
<tr class="even">
<td><code id="cloudwatchlogs_tag_resource_:_tags">tags</code></td>
<td><p>[required] The list of key-value pairs to associate with the
resource.</p></td>
</tr>
</tbody>
</table>

### Value

An empty list.

### Request syntax

    svc$tag_resource(
      resourceArn = "string",
      tags = list(
        "string"
      )
    )