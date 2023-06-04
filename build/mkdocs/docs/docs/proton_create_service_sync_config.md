<table style="width: 100%;">
<tbody>
<tr class="odd">
<td>proton_create_service_sync_config</td>
<td style="text-align: right;">R Documentation</td>
</tr>
</tbody>
</table>

## Create the Proton Ops configuration file

### Description

Create the Proton Ops configuration file.

### Usage

    proton_create_service_sync_config(branch, filePath, repositoryName,
      repositoryProvider, serviceName)

### Arguments

<table>
<colgroup>
<col style="width: 35%" />
<col style="width: 65%" />
</colgroup>
<tbody>
<tr class="odd">
<td><code
id="proton_create_service_sync_config_:_branch">branch</code></td>
<td><p>[required] The repository branch for your Proton Ops
file.</p></td>
</tr>
<tr class="even">
<td><code
id="proton_create_service_sync_config_:_filePath">filePath</code></td>
<td><p>[required] The path to the Proton Ops file.</p></td>
</tr>
<tr class="odd">
<td><code
id="proton_create_service_sync_config_:_repositoryName">repositoryName</code></td>
<td><p>[required] The repository name.</p></td>
</tr>
<tr class="even">
<td><code
id="proton_create_service_sync_config_:_repositoryProvider">repositoryProvider</code></td>
<td><p>[required] The provider type for your repository.</p></td>
</tr>
<tr class="odd">
<td><code
id="proton_create_service_sync_config_:_serviceName">serviceName</code></td>
<td><p>[required] The name of the service the Proton Ops file is
for.</p></td>
</tr>
</tbody>
</table>

### Value

A list with the following syntax:

    list(
      serviceSyncConfig = list(
        branch = "string",
        filePath = "string",
        repositoryName = "string",
        repositoryProvider = "GITHUB"|"GITHUB_ENTERPRISE"|"BITBUCKET",
        serviceName = "string"
      )
    )

### Request syntax

    svc$create_service_sync_config(
      branch = "string",
      filePath = "string",
      repositoryName = "string",
      repositoryProvider = "GITHUB"|"GITHUB_ENTERPRISE"|"BITBUCKET",
      serviceName = "string"
    )