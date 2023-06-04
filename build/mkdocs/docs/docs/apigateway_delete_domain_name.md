<table style="width: 100%;">
<tbody>
<tr class="odd">
<td>apigateway_delete_domain_name</td>
<td style="text-align: right;">R Documentation</td>
</tr>
</tbody>
</table>

## Deletes the DomainName resource

### Description

Deletes the DomainName resource.

### Usage

    apigateway_delete_domain_name(domainName)

### Arguments

<table>
<colgroup>
<col style="width: 35%" />
<col style="width: 65%" />
</colgroup>
<tbody>
<tr class="odd">
<td><code
id="apigateway_delete_domain_name_:_domainName">domainName</code></td>
<td><p>[required] The name of the DomainName resource to be
deleted.</p></td>
</tr>
</tbody>
</table>

### Value

An empty list.

### Request syntax

    svc$delete_domain_name(
      domainName = "string"
    )