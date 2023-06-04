<table style="width: 100%;">
<tbody>
<tr class="odd">
<td>rds_describe_db_parameters</td>
<td style="text-align: right;">R Documentation</td>
</tr>
</tbody>
</table>

## Returns the detailed parameter list for a particular DB parameter group

### Description

Returns the detailed parameter list for a particular DB parameter group.

### Usage

    rds_describe_db_parameters(DBParameterGroupName, Source, Filters,
      MaxRecords, Marker)

### Arguments

<table>
<colgroup>
<col style="width: 35%" />
<col style="width: 65%" />
</colgroup>
<tbody>
<tr class="odd">
<td><code
id="rds_describe_db_parameters_:_DBParameterGroupName">DBParameterGroupName</code></td>
<td><p>[required] The name of a specific DB parameter group to return
details for.</p>
<p>Constraints:</p>
<ul>
<li><p>If supplied, must match the name of an existing
DBParameterGroup.</p></li>
</ul></td>
</tr>
<tr class="even">
<td><code id="rds_describe_db_parameters_:_Source">Source</code></td>
<td><p>The parameter types to return.</p>
<p>Default: All parameter types returned</p>
<p>Valid Values: <code>user | system | engine-default</code></p></td>
</tr>
<tr class="odd">
<td><code id="rds_describe_db_parameters_:_Filters">Filters</code></td>
<td><p>This parameter isn't currently supported.</p></td>
</tr>
<tr class="even">
<td><code
id="rds_describe_db_parameters_:_MaxRecords">MaxRecords</code></td>
<td><p>The maximum number of records to include in the response. If more
records exist than the specified <code>MaxRecords</code> value, a
pagination token called a marker is included in the response so that you
can retrieve the remaining results.</p>
<p>Default: 100</p>
<p>Constraints: Minimum 20, maximum 100.</p></td>
</tr>
<tr class="odd">
<td><code id="rds_describe_db_parameters_:_Marker">Marker</code></td>
<td><p>An optional pagination token provided by a previous
<code>describe_db_parameters</code> request. If this parameter is
specified, the response includes only records beyond the marker, up to
the value specified by <code>MaxRecords</code>.</p></td>
</tr>
</tbody>
</table>

### Value

A list with the following syntax:

    list(
      Parameters = list(
        list(
          ParameterName = "string",
          ParameterValue = "string",
          Description = "string",
          Source = "string",
          ApplyType = "string",
          DataType = "string",
          AllowedValues = "string",
          IsModifiable = TRUE|FALSE,
          MinimumEngineVersion = "string",
          ApplyMethod = "immediate"|"pending-reboot",
          SupportedEngineModes = list(
            "string"
          )
        )
      ),
      Marker = "string"
    )

### Request syntax

    svc$describe_db_parameters(
      DBParameterGroupName = "string",
      Source = "string",
      Filters = list(
        list(
          Name = "string",
          Values = list(
            "string"
          )
        )
      ),
      MaxRecords = 123,
      Marker = "string"
    )