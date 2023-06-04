<table style="width: 100%;">
<tbody>
<tr class="odd">
<td>connect_create_prompt</td>
<td style="text-align: right;">R Documentation</td>
</tr>
</tbody>
</table>

## Creates a prompt

### Description

Creates a prompt. For more information about prompts, such as supported
file types and maximum length, see [Create
prompts](https://docs.aws.amazon.com/connect/latest/adminguide/prompts.html)
in the *Amazon Connect Administrator's Guide*.

### Usage

    connect_create_prompt(InstanceId, Name, Description, S3Uri, Tags)

### Arguments

<table>
<colgroup>
<col style="width: 35%" />
<col style="width: 65%" />
</colgroup>
<tbody>
<tr class="odd">
<td><code id="connect_create_prompt_:_InstanceId">InstanceId</code></td>
<td><p>[required] The identifier of the Amazon Connect instance. You can
<a
href="https://docs.aws.amazon.com/connect/latest/adminguide/find-instance-arn.html">find
the instance ID</a> in the Amazon Resource Name (ARN) of the
instance.</p></td>
</tr>
<tr class="even">
<td><code id="connect_create_prompt_:_Name">Name</code></td>
<td><p>[required] The name of the prompt.</p></td>
</tr>
<tr class="odd">
<td><code
id="connect_create_prompt_:_Description">Description</code></td>
<td><p>The description of the prompt.</p></td>
</tr>
<tr class="even">
<td><code id="connect_create_prompt_:_S3Uri">S3Uri</code></td>
<td><p>[required] The URI for the S3 bucket where the prompt is
stored.</p></td>
</tr>
<tr class="odd">
<td><code id="connect_create_prompt_:_Tags">Tags</code></td>
<td><p>The tags used to organize, track, or control access for this
resource. For example, { "tags": {"key1":"value1", "key2":"value2"}
}.</p></td>
</tr>
</tbody>
</table>

### Value

A list with the following syntax:

    list(
      PromptARN = "string",
      PromptId = "string"
    )

### Request syntax

    svc$create_prompt(
      InstanceId = "string",
      Name = "string",
      Description = "string",
      S3Uri = "string",
      Tags = list(
        "string"
      )
    )