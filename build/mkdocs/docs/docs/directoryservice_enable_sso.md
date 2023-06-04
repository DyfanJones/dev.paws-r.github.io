<table style="width: 100%;">
<tbody>
<tr class="odd">
<td>directoryservice_enable_sso</td>
<td style="text-align: right;">R Documentation</td>
</tr>
</tbody>
</table>

## Enables single sign-on for a directory

### Description

Enables single sign-on for a directory. Single sign-on allows users in
your directory to access certain Amazon Web Services services from a
computer joined to the directory without having to enter their
credentials separately.

### Usage

    directoryservice_enable_sso(DirectoryId, UserName, Password)

### Arguments

<table>
<colgroup>
<col style="width: 35%" />
<col style="width: 65%" />
</colgroup>
<tbody>
<tr class="odd">
<td><code
id="directoryservice_enable_sso_:_DirectoryId">DirectoryId</code></td>
<td><p>[required] The identifier of the directory for which to enable
single-sign on.</p></td>
</tr>
<tr class="even">
<td><code
id="directoryservice_enable_sso_:_UserName">UserName</code></td>
<td><p>The username of an alternate account to use to enable single-sign
on. This is only used for AD Connector directories. This account must
have privileges to add a service principal name.</p>
<p>If the AD Connector service account does not have privileges to add a
service principal name, you can specify an alternate account with the
<em>UserName</em> and <em>Password</em> parameters. These credentials
are only used to enable single sign-on and are not stored by the
service. The AD Connector service account is not changed.</p></td>
</tr>
<tr class="odd">
<td><code
id="directoryservice_enable_sso_:_Password">Password</code></td>
<td><p>The password of an alternate account to use to enable single-sign
on. This is only used for AD Connector directories. For more
information, see the <em>UserName</em> parameter.</p></td>
</tr>
</tbody>
</table>

### Value

An empty list.

### Request syntax

    svc$enable_sso(
      DirectoryId = "string",
      UserName = "string",
      Password = "string"
    )