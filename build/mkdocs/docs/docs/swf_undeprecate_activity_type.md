<table style="width: 100%;">
<tbody>
<tr class="odd">
<td>swf_undeprecate_activity_type</td>
<td style="text-align: right;">R Documentation</td>
</tr>
</tbody>
</table>

## Undeprecates a previously deprecated activity type

### Description

Undeprecates a previously deprecated *activity type*. After an activity
type has been undeprecated, you can create new tasks of that activity
type.

This operation is eventually consistent. The results are best effort and
may not exactly reflect recent updates and changes.

**Access Control**

You can use IAM policies to control this action's access to Amazon SWF
resources as follows:

-   Use a `Resource` element with the domain name to limit the action to
    only specified domains.

-   Use an `Action` element to allow or deny permission to call this
    action.

-   Constrain the following parameters by using a `Condition` element
    with the appropriate keys.

    -   `activityType.name`: String constraint. The key is
        `swf:activityType.name`.

    -   `activityType.version`: String constraint. The key is
        `swf:activityType.version`.

If the caller doesn't have sufficient permissions to invoke the action,
or the parameter values fall outside the specified constraints, the
action fails. The associated event attribute's `cause` parameter is set
to `OPERATION_NOT_PERMITTED`. For details and example IAM policies, see
[Using IAM to Manage Access to Amazon SWF
Workflows](https://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html)
in the *Amazon SWF Developer Guide*.

### Usage

    swf_undeprecate_activity_type(domain, activityType)

### Arguments

<table>
<colgroup>
<col style="width: 35%" />
<col style="width: 65%" />
</colgroup>
<tbody>
<tr class="odd">
<td><code id="swf_undeprecate_activity_type_:_domain">domain</code></td>
<td><p>[required] The name of the domain of the deprecated activity
type.</p></td>
</tr>
<tr class="even">
<td><code
id="swf_undeprecate_activity_type_:_activityType">activityType</code></td>
<td><p>[required] The activity type to undeprecate.</p></td>
</tr>
</tbody>
</table>

### Value

An empty list.

### Request syntax

    svc$undeprecate_activity_type(
      domain = "string",
      activityType = list(
        name = "string",
        version = "string"
      )
    )