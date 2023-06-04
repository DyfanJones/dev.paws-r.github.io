<table style="width: 100%;">
<tbody>
<tr class="odd">
<td>inspector_list_findings</td>
<td style="text-align: right;">R Documentation</td>
</tr>
</tbody>
</table>

## Lists findings that are generated by the assessment runs that are specified by the ARNs of the assessment runs

### Description

Lists findings that are generated by the assessment runs that are
specified by the ARNs of the assessment runs.

### Usage

    inspector_list_findings(assessmentRunArns, filter, nextToken,
      maxResults)

### Arguments

<table>
<colgroup>
<col style="width: 35%" />
<col style="width: 65%" />
</colgroup>
<tbody>
<tr class="odd">
<td><code
id="inspector_list_findings_:_assessmentRunArns">assessmentRunArns</code></td>
<td><p>The ARNs of the assessment runs that generate the findings that
you want to list.</p></td>
</tr>
<tr class="even">
<td><code id="inspector_list_findings_:_filter">filter</code></td>
<td><p>You can use this parameter to specify a subset of data to be
included in the action's response.</p>
<p>For a record to match a filter, all specified filter attributes must
match. When multiple values are specified for a filter attribute, any of
the values can match.</p></td>
</tr>
<tr class="odd">
<td><code id="inspector_list_findings_:_nextToken">nextToken</code></td>
<td><p>You can use this parameter when paginating results. Set the value
of this parameter to null on your first call to the
<strong>ListFindings</strong> action. Subsequent calls to the action
fill <strong>nextToken</strong> in the request with the value of
<strong>NextToken</strong> from the previous response to continue
listing data.</p></td>
</tr>
<tr class="even">
<td><code
id="inspector_list_findings_:_maxResults">maxResults</code></td>
<td><p>You can use this parameter to indicate the maximum number of
items you want in the response. The default value is 10. The maximum
value is 500.</p></td>
</tr>
</tbody>
</table>

### Value

A list with the following syntax:

    list(
      findingArns = list(
        "string"
      ),
      nextToken = "string"
    )

### Request syntax

    svc$list_findings(
      assessmentRunArns = list(
        "string"
      ),
      filter = list(
        agentIds = list(
          "string"
        ),
        autoScalingGroups = list(
          "string"
        ),
        ruleNames = list(
          "string"
        ),
        severities = list(
          "Low"|"Medium"|"High"|"Informational"|"Undefined"
        ),
        rulesPackageArns = list(
          "string"
        ),
        attributes = list(
          list(
            key = "string",
            value = "string"
          )
        ),
        userAttributes = list(
          list(
            key = "string",
            value = "string"
          )
        ),
        creationTimeRange = list(
          beginDate = as.POSIXct(
            "2015-01-01"
          ),
          endDate = as.POSIXct(
            "2015-01-01"
          )
        )
      ),
      nextToken = "string",
      maxResults = 123
    )

### Examples

    ## Not run: 
    # Lists findings that are generated by the assessment runs that are
    # specified by the ARNs of the assessment runs.
    svc$list_findings(
      assessmentRunArns = list(
        "arn:aws:inspector:us-west-2:123456789012:target/0-0kFIPusq/template/0-..."
      ),
      maxResults = 123L
    )

    ## End(Not run)