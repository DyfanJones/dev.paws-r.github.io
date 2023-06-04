<table style="width: 100%;">
<tbody>
<tr class="odd">
<td>organizations_list_accounts_for_parent</td>
<td style="text-align: right;">R Documentation</td>
</tr>
</tbody>
</table>

## Lists the accounts in an organization that are contained by the specified target root or organizational unit (OU)

### Description

Lists the accounts in an organization that are contained by the
specified target root or organizational unit (OU). If you specify the
root, you get a list of all the accounts that aren't in any OU. If you
specify an OU, you get a list of all the accounts in only that OU and
not in any child OUs. To get a list of all accounts in the organization,
use the `list_accounts` operation.

Always check the `NextToken` response parameter for a `null` value when
calling a `⁠List*⁠` operation. These operations can occasionally return an
empty set of results even when there are more results available. The
`NextToken` response parameter value is `null` *only* when there are no
more results to display.

This operation can be called only from the organization's management
account or by a member account that is a delegated administrator for an
Amazon Web Services service.

### Usage

    organizations_list_accounts_for_parent(ParentId, NextToken, MaxResults)

### Arguments

<table>
<colgroup>
<col style="width: 35%" />
<col style="width: 65%" />
</colgroup>
<tbody>
<tr class="odd">
<td><code
id="organizations_list_accounts_for_parent_:_ParentId">ParentId</code></td>
<td><p>[required] The unique identifier (ID) for the parent root or
organization unit (OU) whose accounts you want to list.</p></td>
</tr>
<tr class="even">
<td><code
id="organizations_list_accounts_for_parent_:_NextToken">NextToken</code></td>
<td><p>The parameter for receiving additional results if you receive a
<code>NextToken</code> response in a previous request. A
<code>NextToken</code> response indicates that more output is available.
Set this parameter to the value of the previous call's
<code>NextToken</code> response to indicate where the output should
continue from.</p></td>
</tr>
<tr class="odd">
<td><code
id="organizations_list_accounts_for_parent_:_MaxResults">MaxResults</code></td>
<td><p>The total number of results that you want included on each page
of the response. If you do not include this parameter, it defaults to a
value that is specific to the operation. If additional items exist
beyond the maximum you specify, the <code>NextToken</code> response
element is present and has a value (is not null). Include that value as
the <code>NextToken</code> request parameter in the next call to the
operation to get the next part of the results. Note that Organizations
might return fewer results than the maximum even when there are more
results available. You should check <code>NextToken</code> after every
operation to ensure that you receive all of the results.</p></td>
</tr>
</tbody>
</table>

### Value

A list with the following syntax:

    list(
      Accounts = list(
        list(
          Id = "string",
          Arn = "string",
          Email = "string",
          Name = "string",
          Status = "ACTIVE"|"SUSPENDED"|"PENDING_CLOSURE",
          JoinedMethod = "INVITED"|"CREATED",
          JoinedTimestamp = as.POSIXct(
            "2015-01-01"
          )
        )
      ),
      NextToken = "string"
    )

### Request syntax

    svc$list_accounts_for_parent(
      ParentId = "string",
      NextToken = "string",
      MaxResults = 123
    )

### Examples

    ## Not run: 
    # The following example shows how to request a list of the accounts in an
    # OU:/n/n
    svc$list_accounts_for_parent(
      ParentId = "ou-examplerootid111-exampleouid111"
    )

    ## End(Not run)