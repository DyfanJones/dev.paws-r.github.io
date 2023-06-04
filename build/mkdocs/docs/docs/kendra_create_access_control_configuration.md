<table style="width: 100%;">
<tbody>
<tr class="odd">
<td>kendra_create_access_control_configuration</td>
<td style="text-align: right;">R Documentation</td>
</tr>
</tbody>
</table>

## Creates an access configuration for your documents

### Description

Creates an access configuration for your documents. This includes user
and group access information for your documents. This is useful for user
context filtering, where search results are filtered based on the user
or their group access to documents.

You can use this to re-configure your existing document level access
control without indexing all of your documents again. For example, your
index contains top-secret company documents that only certain employees
or users should access. One of these users leaves the company or
switches to a team that should be blocked from accessing top-secret
documents. The user still has access to top-secret documents because the
user had access when your documents were previously indexed. You can
create a specific access control configuration for the user with deny
access. You can later update the access control configuration to allow
access if the user returns to the company and re-joins the 'top-secret'
team. You can re-configure access control for your documents as
circumstances change.

To apply your access control configuration to certain documents, you
call the `batch_put_document` API with the
`AccessControlConfigurationId` included in the
[Document](https://docs.aws.amazon.com/kendra/latest/APIReference/API_Document.html)
object. If you use an S3 bucket as a data source, you update the
`.metadata.json` with the `AccessControlConfigurationId` and synchronize
your data source. Amazon Kendra currently only supports access control
configuration for S3 data sources and documents indexed using the
`batch_put_document` API.

### Usage

    kendra_create_access_control_configuration(IndexId, Name, Description,
      AccessControlList, HierarchicalAccessControlList, ClientToken)

### Arguments

<table>
<colgroup>
<col style="width: 35%" />
<col style="width: 65%" />
</colgroup>
<tbody>
<tr class="odd">
<td><code
id="kendra_create_access_control_configuration_:_IndexId">IndexId</code></td>
<td><p>[required] The identifier of the index to create an access
control configuration for your documents.</p></td>
</tr>
<tr class="even">
<td><code
id="kendra_create_access_control_configuration_:_Name">Name</code></td>
<td><p>[required] A name for the access control configuration.</p></td>
</tr>
<tr class="odd">
<td><code
id="kendra_create_access_control_configuration_:_Description">Description</code></td>
<td><p>A description for the access control configuration.</p></td>
</tr>
<tr class="even">
<td><code
id="kendra_create_access_control_configuration_:_AccessControlList">AccessControlList</code></td>
<td><p>Information on principals (users and/or groups) and which
documents they should have access to. This is useful for user context
filtering, where search results are filtered based on the user or their
group access to documents.</p></td>
</tr>
<tr class="odd">
<td><code
id="kendra_create_access_control_configuration_:_HierarchicalAccessControlList">HierarchicalAccessControlList</code></td>
<td><p>The list of <a
href="https://docs.aws.amazon.com/kendra/latest/APIReference/API_Principal.html">principal</a>
lists that define the hierarchy for which documents users should have
access to.</p></td>
</tr>
<tr class="even">
<td><code
id="kendra_create_access_control_configuration_:_ClientToken">ClientToken</code></td>
<td><p>A token that you provide to identify the request to create an
access control configuration. Multiple calls to the
<code>create_access_control_configuration</code> API with the same
client token will create only one access control configuration.</p></td>
</tr>
</tbody>
</table>

### Value

A list with the following syntax:

    list(
      Id = "string"
    )

### Request syntax

    svc$create_access_control_configuration(
      IndexId = "string",
      Name = "string",
      Description = "string",
      AccessControlList = list(
        list(
          Name = "string",
          Type = "USER"|"GROUP",
          Access = "ALLOW"|"DENY",
          DataSourceId = "string"
        )
      ),
      HierarchicalAccessControlList = list(
        list(
          PrincipalList = list(
            list(
              Name = "string",
              Type = "USER"|"GROUP",
              Access = "ALLOW"|"DENY",
              DataSourceId = "string"
            )
          )
        )
      ),
      ClientToken = "string"
    )