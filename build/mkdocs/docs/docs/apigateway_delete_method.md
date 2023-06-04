<table style="width: 100%;">
<tbody>
<tr class="odd">
<td>apigateway_delete_method</td>
<td style="text-align: right;">R Documentation</td>
</tr>
</tbody>
</table>

## Deletes an existing Method resource

### Description

Deletes an existing Method resource.

### Usage

    apigateway_delete_method(restApiId, resourceId, httpMethod)

### Arguments

<table>
<colgroup>
<col style="width: 35%" />
<col style="width: 65%" />
</colgroup>
<tbody>
<tr class="odd">
<td><code
id="apigateway_delete_method_:_restApiId">restApiId</code></td>
<td><p>[required] The string identifier of the associated
RestApi.</p></td>
</tr>
<tr class="even">
<td><code
id="apigateway_delete_method_:_resourceId">resourceId</code></td>
<td><p>[required] The Resource identifier for the Method
resource.</p></td>
</tr>
<tr class="odd">
<td><code
id="apigateway_delete_method_:_httpMethod">httpMethod</code></td>
<td><p>[required] The HTTP verb of the Method resource.</p></td>
</tr>
</tbody>
</table>

### Value

An empty list.

### Request syntax

    svc$delete_method(
      restApiId = "string",
      resourceId = "string",
      httpMethod = "string"
    )