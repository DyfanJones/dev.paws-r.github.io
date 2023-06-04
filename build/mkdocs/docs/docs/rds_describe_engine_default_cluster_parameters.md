<table style="width: 100%;">
<tbody>
<tr class="odd">
<td>rds_describe_engine_default_cluster_parameters</td>
<td style="text-align: right;">R Documentation</td>
</tr>
</tbody>
</table>

## Returns the default engine and system parameter information for the cluster database engine

### Description

Returns the default engine and system parameter information for the
cluster database engine.

For more information on Amazon Aurora, see [What is Amazon
Aurora?](https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/CHAP_AuroraOverview.html)
in the *Amazon Aurora User Guide*.

### Usage

    rds_describe_engine_default_cluster_parameters(DBParameterGroupFamily,
      Filters, MaxRecords, Marker)

### Arguments

<table>
<colgroup>
<col style="width: 35%" />
<col style="width: 65%" />
</colgroup>
<tbody>
<tr class="odd">
<td><code
id="rds_describe_engine_default_cluster_parameters_:_DBParameterGroupFamily">DBParameterGroupFamily</code></td>
<td><p>[required] The name of the DB cluster parameter group family to
return engine parameter information for.</p></td>
</tr>
<tr class="even">
<td><code
id="rds_describe_engine_default_cluster_parameters_:_Filters">Filters</code></td>
<td><p>This parameter isn't currently supported.</p></td>
</tr>
<tr class="odd">
<td><code
id="rds_describe_engine_default_cluster_parameters_:_MaxRecords">MaxRecords</code></td>
<td><p>The maximum number of records to include in the response. If more
records exist than the specified <code>MaxRecords</code> value, a
pagination token called a marker is included in the response so you can
retrieve the remaining results.</p>
<p>Default: 100</p>
<p>Constraints: Minimum 20, maximum 100.</p></td>
</tr>
<tr class="even">
<td><code
id="rds_describe_engine_default_cluster_parameters_:_Marker">Marker</code></td>
<td><p>An optional pagination token provided by a previous
<code>describe_engine_default_cluster_parameters</code> request. If this
parameter is specified, the response includes only records beyond the
marker, up to the value specified by <code>MaxRecords</code>.</p></td>
</tr>
</tbody>
</table>

### Value

A list with the following syntax:

    list(
      EngineDefaults = list(
        DBParameterGroupFamily = "string",
        Marker = "string",
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
        )
      )
    )

### Request syntax

    svc$describe_engine_default_cluster_parameters(
      DBParameterGroupFamily = "string",
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