<table style="width: 100%;">
<tbody>
<tr class="odd">
<td>opensearchservice_describe_instance_type_limits</td>
<td style="text-align: right;">R Documentation</td>
</tr>
</tbody>
</table>

## Describes the instance count, storage, and master node limits for a given OpenSearch or Elasticsearch version and instance type

### Description

Describes the instance count, storage, and master node limits for a
given OpenSearch or Elasticsearch version and instance type.

### Usage

    opensearchservice_describe_instance_type_limits(DomainName,
      InstanceType, EngineVersion)

### Arguments

<table>
<colgroup>
<col style="width: 35%" />
<col style="width: 65%" />
</colgroup>
<tbody>
<tr class="odd">
<td><code
id="opensearchservice_describe_instance_type_limits_:_DomainName">DomainName</code></td>
<td><p>The name of the domain. Only specify if you need the limits for
an existing domain.</p></td>
</tr>
<tr class="even">
<td><code
id="opensearchservice_describe_instance_type_limits_:_InstanceType">InstanceType</code></td>
<td><p>[required] The OpenSearch Service instance type for which you
need limit information.</p></td>
</tr>
<tr class="odd">
<td><code
id="opensearchservice_describe_instance_type_limits_:_EngineVersion">EngineVersion</code></td>
<td><p>[required] Version of OpenSearch or Elasticsearch, in the format
Elasticsearch_X.Y or OpenSearch_X.Y. Defaults to the latest version of
OpenSearch.</p></td>
</tr>
</tbody>
</table>

### Value

A list with the following syntax:

    list(
      LimitsByRole = list(
        list(
          StorageTypes = list(
            list(
              StorageTypeName = "string",
              StorageSubTypeName = "string",
              StorageTypeLimits = list(
                list(
                  LimitName = "string",
                  LimitValues = list(
                    "string"
                  )
                )
              )
            )
          ),
          InstanceLimits = list(
            InstanceCountLimits = list(
              MinimumInstanceCount = 123,
              MaximumInstanceCount = 123
            )
          ),
          AdditionalLimits = list(
            list(
              LimitName = "string",
              LimitValues = list(
                "string"
              )
            )
          )
        )
      )
    )

### Request syntax

    svc$describe_instance_type_limits(
      DomainName = "string",
      InstanceType = "m3.medium.search"|"m3.large.search"|"m3.xlarge.search"|"m3.2xlarge.search"|"m4.large.search"|"m4.xlarge.search"|"m4.2xlarge.search"|"m4.4xlarge.search"|"m4.10xlarge.search"|"m5.large.search"|"m5.xlarge.search"|"m5.2xlarge.search"|"m5.4xlarge.search"|"m5.12xlarge.search"|"m5.24xlarge.search"|"r5.large.search"|"r5.xlarge.search"|"r5.2xlarge.search"|"r5.4xlarge.search"|"r5.12xlarge.search"|"r5.24xlarge.search"|"c5.large.search"|"c5.xlarge.search"|"c5.2xlarge.search"|"c5.4xlarge.search"|"c5.9xlarge.search"|"c5.18xlarge.search"|"t3.nano.search"|"t3.micro.search"|"t3.small.search"|"t3.medium.search"|"t3.large.search"|"t3.xlarge.search"|"t3.2xlarge.search"|"ultrawarm1.medium.search"|"ultrawarm1.large.search"|"ultrawarm1.xlarge.search"|"t2.micro.search"|"t2.small.search"|"t2.medium.search"|"r3.large.search"|"r3.xlarge.search"|"r3.2xlarge.search"|"r3.4xlarge.search"|"r3.8xlarge.search"|"i2.xlarge.search"|"i2.2xlarge.search"|"d2.xlarge.search"|"d2.2xlarge.search"|"d2.4xlarge.search"|"d2.8xlarge.search"|"c4.large.search"|"c4.xlarge.search"|"c4.2xlarge.search"|"c4.4xlarge.search"|"c4.8xlarge.search"|"r4.large.search"|"r4.xlarge.search"|"r4.2xlarge.search"|"r4.4xlarge.search"|"r4.8xlarge.search"|"r4.16xlarge.search"|"i3.large.search"|"i3.xlarge.search"|"i3.2xlarge.search"|"i3.4xlarge.search"|"i3.8xlarge.search"|"i3.16xlarge.search"|"r6g.large.search"|"r6g.xlarge.search"|"r6g.2xlarge.search"|"r6g.4xlarge.search"|"r6g.8xlarge.search"|"r6g.12xlarge.search"|"m6g.large.search"|"m6g.xlarge.search"|"m6g.2xlarge.search"|"m6g.4xlarge.search"|"m6g.8xlarge.search"|"m6g.12xlarge.search"|"c6g.large.search"|"c6g.xlarge.search"|"c6g.2xlarge.search"|"c6g.4xlarge.search"|"c6g.8xlarge.search"|"c6g.12xlarge.search"|"r6gd.large.search"|"r6gd.xlarge.search"|"r6gd.2xlarge.search"|"r6gd.4xlarge.search"|"r6gd.8xlarge.search"|"r6gd.12xlarge.search"|"r6gd.16xlarge.search"|"t4g.small.search"|"t4g.medium.search",
      EngineVersion = "string"
    )