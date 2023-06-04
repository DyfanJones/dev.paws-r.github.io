<table style="width: 100%;">
<tbody>
<tr class="odd">
<td>cognitoidentity_tag_resource</td>
<td style="text-align: right;">R Documentation</td>
</tr>
</tbody>
</table>

## Assigns a set of tags to the specified Amazon Cognito identity pool

### Description

Assigns a set of tags to the specified Amazon Cognito identity pool. A
tag is a label that you can use to categorize and manage identity pools
in different ways, such as by purpose, owner, environment, or other
criteria.

Each tag consists of a key and value, both of which you define. A key is
a general category for more specific values. For example, if you have
two versions of an identity pool, one for testing and another for
production, you might assign an `Environment` tag key to both identity
pools. The value of this key might be `Test` for one identity pool and
`Production` for the other.

Tags are useful for cost tracking and access control. You can activate
your tags so that they appear on the Billing and Cost Management
console, where you can track the costs associated with your identity
pools. In an IAM policy, you can constrain permissions for identity
pools based on specific tags or tag values.

You can use this action up to 5 times per second, per account. An
identity pool can have as many as 50 tags.

### Usage

    cognitoidentity_tag_resource(ResourceArn, Tags)

### Arguments

<table>
<colgroup>
<col style="width: 35%" />
<col style="width: 65%" />
</colgroup>
<tbody>
<tr class="odd">
<td><code
id="cognitoidentity_tag_resource_:_ResourceArn">ResourceArn</code></td>
<td><p>[required] The Amazon Resource Name (ARN) of the identity
pool.</p></td>
</tr>
<tr class="even">
<td><code id="cognitoidentity_tag_resource_:_Tags">Tags</code></td>
<td><p>[required] The tags to assign to the identity pool.</p></td>
</tr>
</tbody>
</table>

### Value

An empty list.

### Request syntax

    svc$tag_resource(
      ResourceArn = "string",
      Tags = list(
        "string"
      )
    )