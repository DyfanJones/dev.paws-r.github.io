<table style="width: 100%;">
<tbody>
<tr class="odd">
<td>lightsail_get_key_pairs</td>
<td style="text-align: right;">R Documentation</td>
</tr>
</tbody>
</table>

## Returns information about all key pairs in the user's account

### Description

Returns information about all key pairs in the user's account.

### Usage

    lightsail_get_key_pairs(pageToken, includeDefaultKeyPair)

### Arguments

<table>
<colgroup>
<col style="width: 35%" />
<col style="width: 65%" />
</colgroup>
<tbody>
<tr class="odd">
<td><code id="lightsail_get_key_pairs_:_pageToken">pageToken</code></td>
<td><p>The token to advance to the next page of results from your
request.</p>
<p>To get a page token, perform an initial <code>get_key_pairs</code>
request. If your results are paginated, the response will return a next
page token that you can specify as the page token in a subsequent
request.</p></td>
</tr>
<tr class="even">
<td><code
id="lightsail_get_key_pairs_:_includeDefaultKeyPair">includeDefaultKeyPair</code></td>
<td><p>A Boolean value that indicates whether to include the default key
pair in the response of your request.</p></td>
</tr>
</tbody>
</table>

### Value

A list with the following syntax:

    list(
      keyPairs = list(
        list(
          name = "string",
          arn = "string",
          supportCode = "string",
          createdAt = as.POSIXct(
            "2015-01-01"
          ),
          location = list(
            availabilityZone = "string",
            regionName = "us-east-1"|"us-east-2"|"us-west-1"|"us-west-2"|"eu-west-1"|"eu-west-2"|"eu-west-3"|"eu-central-1"|"ca-central-1"|"ap-south-1"|"ap-southeast-1"|"ap-southeast-2"|"ap-northeast-1"|"ap-northeast-2"|"eu-north-1"
          ),
          resourceType = "ContainerService"|"Instance"|"StaticIp"|"KeyPair"|"InstanceSnapshot"|"Domain"|"PeeredVpc"|"LoadBalancer"|"LoadBalancerTlsCertificate"|"Disk"|"DiskSnapshot"|"RelationalDatabase"|"RelationalDatabaseSnapshot"|"ExportSnapshotRecord"|"CloudFormationStackRecord"|"Alarm"|"ContactMethod"|"Distribution"|"Certificate"|"Bucket",
          tags = list(
            list(
              key = "string",
              value = "string"
            )
          ),
          fingerprint = "string"
        )
      ),
      nextPageToken = "string"
    )

### Request syntax

    svc$get_key_pairs(
      pageToken = "string",
      includeDefaultKeyPair = TRUE|FALSE
    )