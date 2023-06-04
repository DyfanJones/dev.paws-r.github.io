<table style="width: 100%;">
<tbody>
<tr class="odd">
<td>resourcegroups_list_groups</td>
<td style="text-align: right;">R Documentation</td>
</tr>
</tbody>
</table>

## Returns a list of existing Resource Groups in your account

### Description

Returns a list of existing Resource Groups in your account.

**Minimum permissions**

To run this command, you must have the following permissions:

-   `resource-groups:ListGroups`

### Usage

    resourcegroups_list_groups(Filters, MaxResults, NextToken)

### Arguments

<table>
<colgroup>
<col style="width: 35%" />
<col style="width: 65%" />
</colgroup>
<tbody>
<tr class="odd">
<td><code id="resourcegroups_list_groups_:_Filters">Filters</code></td>
<td><p>Filters, formatted as GroupFilter objects, that you want to apply
to a <code>list_groups</code> operation.</p>
<ul>
<li><p><code>resource-type</code> - Filter the results to include only
those of the specified resource types. Specify up to five resource types
in the format <code
style="white-space: pre;">⁠AWS::ServiceCode::ResourceType ⁠</code>. For
example, <code style="white-space: pre;">⁠AWS::EC2::Instance⁠</code>, or
<code style="white-space: pre;">⁠AWS::S3::Bucket⁠</code>.</p></li>
<li><p><code>configuration-type</code> - Filter the results to include
only those groups that have the specified configuration types attached.
The current supported values are:</p>
<ul>
<li><p><code
style="white-space: pre;">⁠AWS::EC2::CapacityReservationPool⁠</code></p></li>
<li><p><code
style="white-space: pre;">⁠AWS::EC2::HostManagement⁠</code></p></li>
</ul></li>
</ul></td>
</tr>
<tr class="even">
<td><code
id="resourcegroups_list_groups_:_MaxResults">MaxResults</code></td>
<td><p>The total number of results that you want included on each page
of the response. If you do not include this parameter, it defaults to a
value that is specific to the operation. If additional items exist
beyond the maximum you specify, the <code>NextToken</code> response
element is present and has a value (is not null). Include that value as
the <code>NextToken</code> request parameter in the next call to the
operation to get the next part of the results. Note that the service
might return fewer results than the maximum even when there are more
results available. You should check <code>NextToken</code> after every
operation to ensure that you receive all of the results.</p></td>
</tr>
<tr class="odd">
<td><code
id="resourcegroups_list_groups_:_NextToken">NextToken</code></td>
<td><p>The parameter for receiving additional results if you receive a
<code>NextToken</code> response in a previous request. A
<code>NextToken</code> response indicates that more output is available.
Set this parameter to the value provided by a previous call's
<code>NextToken</code> response to indicate where the output should
continue from.</p></td>
</tr>
</tbody>
</table>

### Value

A list with the following syntax:

    list(
      GroupIdentifiers = list(
        list(
          GroupName = "string",
          GroupArn = "string"
        )
      ),
      Groups = list(
        list(
          GroupArn = "string",
          Name = "string",
          Description = "string"
        )
      ),
      NextToken = "string"
    )

### Request syntax

    svc$list_groups(
      Filters = list(
        list(
          Name = "resource-type"|"configuration-type",
          Values = list(
            "string"
          )
        )
      ),
      MaxResults = 123,
      NextToken = "string"
    )