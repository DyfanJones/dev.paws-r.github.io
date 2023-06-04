<table style="width: 100%;">
<tbody>
<tr class="odd">
<td>memorydb_describe_reserved_nodes_offerings</td>
<td style="text-align: right;">R Documentation</td>
</tr>
</tbody>
</table>

## Lists available reserved node offerings

### Description

Lists available reserved node offerings.

### Usage

    memorydb_describe_reserved_nodes_offerings(ReservedNodesOfferingId,
      NodeType, Duration, OfferingType, MaxResults, NextToken)

### Arguments

<table>
<colgroup>
<col style="width: 35%" />
<col style="width: 65%" />
</colgroup>
<tbody>
<tr class="odd">
<td><code
id="memorydb_describe_reserved_nodes_offerings_:_ReservedNodesOfferingId">ReservedNodesOfferingId</code></td>
<td><p>The offering identifier filter value. Use this parameter to show
only the available offering that matches the specified reservation
identifier.</p></td>
</tr>
<tr class="even">
<td><code
id="memorydb_describe_reserved_nodes_offerings_:_NodeType">NodeType</code></td>
<td><p>The node type for the reserved nodes. For more information, see
<a
href="https://docs.aws.amazon.com/memorydb/latest/devguide/#reserved-nodes-supported">Supported
node types</a>.</p></td>
</tr>
<tr class="odd">
<td><code
id="memorydb_describe_reserved_nodes_offerings_:_Duration">Duration</code></td>
<td><p>Duration filter value, specified in years or seconds. Use this
parameter to show only reservations for a given duration.</p></td>
</tr>
<tr class="even">
<td><code
id="memorydb_describe_reserved_nodes_offerings_:_OfferingType">OfferingType</code></td>
<td><p>The offering type filter value. Use this parameter to show only
the available offerings matching the specified offering type. Valid
values: "All Upfront"|"Partial Upfront"| "No Upfront"</p></td>
</tr>
<tr class="odd">
<td><code
id="memorydb_describe_reserved_nodes_offerings_:_MaxResults">MaxResults</code></td>
<td><p>The maximum number of records to include in the response. If more
records exist than the specified MaxRecords value, a marker is included
in the response so that the remaining results can be retrieved.</p></td>
</tr>
<tr class="even">
<td><code
id="memorydb_describe_reserved_nodes_offerings_:_NextToken">NextToken</code></td>
<td><p>An optional marker returned from a prior request. Use this marker
for pagination of results from this operation. If this parameter is
specified, the response includes only records beyond the marker, up to
the value specified by MaxRecords.</p></td>
</tr>
</tbody>
</table>

### Value

A list with the following syntax:

    list(
      NextToken = "string",
      ReservedNodesOfferings = list(
        list(
          ReservedNodesOfferingId = "string",
          NodeType = "string",
          Duration = 123,
          FixedPrice = 123.0,
          OfferingType = "string",
          RecurringCharges = list(
            list(
              RecurringChargeAmount = 123.0,
              RecurringChargeFrequency = "string"
            )
          )
        )
      )
    )

### Request syntax

    svc$describe_reserved_nodes_offerings(
      ReservedNodesOfferingId = "string",
      NodeType = "string",
      Duration = "string",
      OfferingType = "string",
      MaxResults = 123,
      NextToken = "string"
    )