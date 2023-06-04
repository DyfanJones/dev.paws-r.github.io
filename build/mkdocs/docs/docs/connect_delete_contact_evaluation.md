<table style="width: 100%;">
<tbody>
<tr class="odd">
<td>connect_delete_contact_evaluation</td>
<td style="text-align: right;">R Documentation</td>
</tr>
</tbody>
</table>

## Deletes a contact evaluation in the specified Amazon Connect instance

### Description

Deletes a contact evaluation in the specified Amazon Connect instance.

### Usage

    connect_delete_contact_evaluation(InstanceId, EvaluationId)

### Arguments

<table>
<colgroup>
<col style="width: 35%" />
<col style="width: 65%" />
</colgroup>
<tbody>
<tr class="odd">
<td><code
id="connect_delete_contact_evaluation_:_InstanceId">InstanceId</code></td>
<td><p>[required] The identifier of the Amazon Connect instance. You can
<a
href="https://docs.aws.amazon.com/connect/latest/adminguide/find-instance-arn.html">find
the instance ID</a> in the Amazon Resource Name (ARN) of the
instance.</p></td>
</tr>
<tr class="even">
<td><code
id="connect_delete_contact_evaluation_:_EvaluationId">EvaluationId</code></td>
<td><p>[required] A unique identifier for the contact
evaluation.</p></td>
</tr>
</tbody>
</table>

### Value

An empty list.

### Request syntax

    svc$delete_contact_evaluation(
      InstanceId = "string",
      EvaluationId = "string"
    )