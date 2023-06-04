<table style="width: 100%;">
<tbody>
<tr class="odd">
<td>mq_list_configuration_revisions</td>
<td style="text-align: right;">R Documentation</td>
</tr>
</tbody>
</table>

## Returns a list of all revisions for the specified configuration

### Description

Returns a list of all revisions for the specified configuration.

### Usage

    mq_list_configuration_revisions(ConfigurationId, MaxResults, NextToken)

### Arguments

<table>
<colgroup>
<col style="width: 35%" />
<col style="width: 65%" />
</colgroup>
<tbody>
<tr class="odd">
<td><code
id="mq_list_configuration_revisions_:_ConfigurationId">ConfigurationId</code></td>
<td><p>[required] The unique ID that Amazon MQ generates for the
configuration.</p></td>
</tr>
<tr class="even">
<td><code
id="mq_list_configuration_revisions_:_MaxResults">MaxResults</code></td>
<td><p>The maximum number of brokers that Amazon MQ can return per page
(20 by default). This value must be an integer from 5 to 100.</p></td>
</tr>
<tr class="odd">
<td><code
id="mq_list_configuration_revisions_:_NextToken">NextToken</code></td>
<td><p>The token that specifies the next page of results Amazon MQ
should return. To request the first page, leave nextToken
empty.</p></td>
</tr>
</tbody>
</table>

### Value

A list with the following syntax:

    list(
      ConfigurationId = "string",
      MaxResults = 123,
      NextToken = "string",
      Revisions = list(
        list(
          Created = as.POSIXct(
            "2015-01-01"
          ),
          Description = "string",
          Revision = 123
        )
      )
    )

### Request syntax

    svc$list_configuration_revisions(
      ConfigurationId = "string",
      MaxResults = 123,
      NextToken = "string"
    )