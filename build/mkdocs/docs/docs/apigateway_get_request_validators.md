<table style="width: 100%;">
<tbody>
<tr class="odd">
<td>apigateway_get_request_validators</td>
<td style="text-align: right;">R Documentation</td>
</tr>
</tbody>
</table>

## Gets the RequestValidators collection of a given RestApi

### Description

Gets the RequestValidators collection of a given RestApi.

### Usage

    apigateway_get_request_validators(restApiId, position, limit)

### Arguments

<table>
<colgroup>
<col style="width: 35%" />
<col style="width: 65%" />
</colgroup>
<tbody>
<tr class="odd">
<td><code
id="apigateway_get_request_validators_:_restApiId">restApiId</code></td>
<td><p>[required] The string identifier of the associated
RestApi.</p></td>
</tr>
<tr class="even">
<td><code
id="apigateway_get_request_validators_:_position">position</code></td>
<td><p>The current pagination position in the paged result set.</p></td>
</tr>
<tr class="odd">
<td><code
id="apigateway_get_request_validators_:_limit">limit</code></td>
<td><p>The maximum number of returned results per page. The default
value is 25 and the maximum value is 500.</p></td>
</tr>
</tbody>
</table>

### Value

A list with the following syntax:

    list(
      position = "string",
      items = list(
        list(
          id = "string",
          name = "string",
          validateRequestBody = TRUE|FALSE,
          validateRequestParameters = TRUE|FALSE
        )
      )
    )

### Request syntax

    svc$get_request_validators(
      restApiId = "string",
      position = "string",
      limit = 123
    )