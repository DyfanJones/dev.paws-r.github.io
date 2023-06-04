<table style="width: 100%;">
<tbody>
<tr class="odd">
<td>glue_list_statements</td>
<td style="text-align: right;">R Documentation</td>
</tr>
</tbody>
</table>

## Lists statements for the session

### Description

Lists statements for the session.

### Usage

    glue_list_statements(SessionId, RequestOrigin, NextToken)

### Arguments

<table>
<colgroup>
<col style="width: 35%" />
<col style="width: 65%" />
</colgroup>
<tbody>
<tr class="odd">
<td><code id="glue_list_statements_:_SessionId">SessionId</code></td>
<td><p>[required] The Session ID of the statements.</p></td>
</tr>
<tr class="even">
<td><code
id="glue_list_statements_:_RequestOrigin">RequestOrigin</code></td>
<td><p>The origin of the request to list statements.</p></td>
</tr>
<tr class="odd">
<td><code id="glue_list_statements_:_NextToken">NextToken</code></td>
<td><p>A continuation token, if this is a continuation call.</p></td>
</tr>
</tbody>
</table>

### Value

A list with the following syntax:

    list(
      Statements = list(
        list(
          Id = 123,
          Code = "string",
          State = "WAITING"|"RUNNING"|"AVAILABLE"|"CANCELLING"|"CANCELLED"|"ERROR",
          Output = list(
            Data = list(
              TextPlain = "string"
            ),
            ExecutionCount = 123,
            Status = "WAITING"|"RUNNING"|"AVAILABLE"|"CANCELLING"|"CANCELLED"|"ERROR",
            ErrorName = "string",
            ErrorValue = "string",
            Traceback = list(
              "string"
            )
          ),
          Progress = 123.0,
          StartedOn = 123,
          CompletedOn = 123
        )
      ),
      NextToken = "string"
    )

### Request syntax

    svc$list_statements(
      SessionId = "string",
      RequestOrigin = "string",
      NextToken = "string"
    )