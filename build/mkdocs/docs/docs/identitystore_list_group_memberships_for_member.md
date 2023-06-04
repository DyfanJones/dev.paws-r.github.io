<table style="width: 100%;">
<tbody>
<tr class="odd">
<td>identitystore_list_group_memberships_for_member</td>
<td style="text-align: right;">R Documentation</td>
</tr>
</tbody>
</table>

## For the specified member in the specified identity store, returns the list of all GroupMembership objects and returns results in paginated form

### Description

For the specified member in the specified identity store, returns the
list of all `GroupMembership` objects and returns results in paginated
form.

### Usage

    identitystore_list_group_memberships_for_member(IdentityStoreId,
      MemberId, MaxResults, NextToken)

### Arguments

<table>
<colgroup>
<col style="width: 35%" />
<col style="width: 65%" />
</colgroup>
<tbody>
<tr class="odd">
<td><code
id="identitystore_list_group_memberships_for_member_:_IdentityStoreId">IdentityStoreId</code></td>
<td><p>[required] The globally unique identifier for the identity
store.</p></td>
</tr>
<tr class="even">
<td><code
id="identitystore_list_group_memberships_for_member_:_MemberId">MemberId</code></td>
<td><p>[required] An object that contains the identifier of a group
member. Setting the <code>UserID</code> field to the specific identifier
for a user indicates that the user is a member of the group.</p></td>
</tr>
<tr class="odd">
<td><code
id="identitystore_list_group_memberships_for_member_:_MaxResults">MaxResults</code></td>
<td><p>The maximum number of results to be returned per request. This
parameter is used in the <code>list_users</code> and
<code>list_groups</code> requests to specify how many results to return
in one page. The length limit is 50 characters.</p></td>
</tr>
<tr class="even">
<td><code
id="identitystore_list_group_memberships_for_member_:_NextToken">NextToken</code></td>
<td><p>The pagination token used for the <code>list_users</code>,
<code>list_groups</code>, and <code>list_group_memberships</code> API
operations. This value is generated by the identity store service. It is
returned in the API response if the total results are more than the size
of one page. This token is also returned when it is used in the API
request to search for the next page.</p></td>
</tr>
</tbody>
</table>

### Value

A list with the following syntax:

    list(
      GroupMemberships = list(
        list(
          IdentityStoreId = "string",
          MembershipId = "string",
          GroupId = "string",
          MemberId = list(
            UserId = "string"
          )
        )
      ),
      NextToken = "string"
    )

### Request syntax

    svc$list_group_memberships_for_member(
      IdentityStoreId = "string",
      MemberId = list(
        UserId = "string"
      ),
      MaxResults = 123,
      NextToken = "string"
    )