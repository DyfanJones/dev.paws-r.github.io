<table style="width: 100%;">
<tbody>
<tr class="odd">
<td>route53_change_resource_record_sets</td>
<td style="text-align: right;">R Documentation</td>
</tr>
</tbody>
</table>

## Creates, changes, or deletes a resource record set, which contains authoritative DNS information for a specified domain name or subdomain name

### Description

Creates, changes, or deletes a resource record set, which contains
authoritative DNS information for a specified domain name or subdomain
name. For example, you can use `change_resource_record_sets` to create a
resource record set that routes traffic for test.example.com to a web
server that has an IP address of 192.0.2.44.

**Deleting Resource Record Sets**

To delete a resource record set, you must specify all the same values
that you specified when you created it.

**Change Batches and Transactional Changes**

The request body must include a document with a
`ChangeResourceRecordSetsRequest` element. The request body contains a
list of change items, known as a change batch. Change batches are
considered transactional changes. Route 53 validates the changes in the
request and then either makes all or none of the changes in the change
batch request. This ensures that DNS routing isn't adversely affected by
partial changes to the resource record sets in a hosted zone.

For example, suppose a change batch request contains two changes: it
deletes the `CNAME` resource record set for www.example.com and creates
an alias resource record set for www.example.com. If validation for both
records succeeds, Route 53 deletes the first resource record set and
creates the second resource record set in a single operation. If
validation for either the `DELETE` or the `CREATE` action fails, then
the request is canceled, and the original `CNAME` record continues to
exist.

If you try to delete the same resource record set more than once in a
single change batch, Route 53 returns an `InvalidChangeBatch` error.

**Traffic Flow**

To create resource record sets for complex routing configurations, use
either the traffic flow visual editor in the Route 53 console or the API
actions for traffic policies and traffic policy instances. Save the
configuration as a traffic policy, then associate the traffic policy
with one or more domain names (such as example.com) or subdomain names
(such as www.example.com), in the same hosted zone or in multiple hosted
zones. You can roll back the updates if the new configuration isn't
performing as expected. For more information, see [Using Traffic Flow to
Route DNS
Traffic](https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/traffic-flow.html)
in the *Amazon Route 53 Developer Guide*.

**Create, Delete, and Upsert**

Use `ChangeResourceRecordsSetsRequest` to perform the following actions:

-   `CREATE`: Creates a resource record set that has the specified
    values.

-   `DELETE`: Deletes an existing resource record set that has the
    specified values.

-   `UPSERT`: If a resource set exists Route 53 updates it with the
    values in the request.

**Syntaxes for Creating, Updating, and Deleting Resource Record Sets**

The syntax for a request depends on the type of resource record set that
you want to create, delete, or update, such as weighted, alias, or
failover. The XML elements in your request must appear in the order
listed in the syntax.

For an example for each type of resource record set, see "Examples."

Don't refer to the syntax in the "Parameter Syntax" section, which
includes all of the elements for every kind of resource record set that
you can create, delete, or update by using
`change_resource_record_sets`.

**Change Propagation to Route 53 DNS Servers**

When you submit a `change_resource_record_sets` request, Route 53
propagates your changes to all of the Route 53 authoritative DNS
servers. While your changes are propagating, `get_change` returns a
status of `PENDING`. When propagation is complete, `get_change` returns
a status of `INSYNC`. Changes generally propagate to all Route 53 name
servers within 60 seconds. For more information, see `get_change`.

**Limits on ChangeResourceRecordSets Requests**

For information about the limits on a `change_resource_record_sets`
request, see
[Limits](https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/DNSLimitations.html)
in the *Amazon Route 53 Developer Guide*.

### Usage

    route53_change_resource_record_sets(HostedZoneId, ChangeBatch)

### Arguments

<table>
<colgroup>
<col style="width: 35%" />
<col style="width: 65%" />
</colgroup>
<tbody>
<tr class="odd">
<td><code
id="route53_change_resource_record_sets_:_HostedZoneId">HostedZoneId</code></td>
<td><p>[required] The ID of the hosted zone that contains the resource
record sets that you want to change.</p></td>
</tr>
<tr class="even">
<td><code
id="route53_change_resource_record_sets_:_ChangeBatch">ChangeBatch</code></td>
<td><p>[required] A complex type that contains an optional comment and
the <code>Changes</code> element.</p></td>
</tr>
</tbody>
</table>

### Value

A list with the following syntax:

    list(
      ChangeInfo = list(
        Id = "string",
        Status = "PENDING"|"INSYNC",
        SubmittedAt = as.POSIXct(
          "2015-01-01"
        ),
        Comment = "string"
      )
    )

### Request syntax

    svc$change_resource_record_sets(
      HostedZoneId = "string",
      ChangeBatch = list(
        Comment = "string",
        Changes = list(
          list(
            Action = "CREATE"|"DELETE"|"UPSERT",
            ResourceRecordSet = list(
              Name = "string",
              Type = "SOA"|"A"|"TXT"|"NS"|"CNAME"|"MX"|"NAPTR"|"PTR"|"SRV"|"SPF"|"AAAA"|"CAA"|"DS",
              SetIdentifier = "string",
              Weight = 123,
              Region = "us-east-1"|"us-east-2"|"us-west-1"|"us-west-2"|"ca-central-1"|"eu-west-1"|"eu-west-2"|"eu-west-3"|"eu-central-1"|"eu-central-2"|"ap-southeast-1"|"ap-southeast-2"|"ap-southeast-3"|"ap-northeast-1"|"ap-northeast-2"|"ap-northeast-3"|"eu-north-1"|"sa-east-1"|"cn-north-1"|"cn-northwest-1"|"ap-east-1"|"me-south-1"|"me-central-1"|"ap-south-1"|"ap-south-2"|"af-south-1"|"eu-south-1"|"eu-south-2"|"ap-southeast-4",
              GeoLocation = list(
                ContinentCode = "string",
                CountryCode = "string",
                SubdivisionCode = "string"
              ),
              Failover = "PRIMARY"|"SECONDARY",
              MultiValueAnswer = TRUE|FALSE,
              TTL = 123,
              ResourceRecords = list(
                list(
                  Value = "string"
                )
              ),
              AliasTarget = list(
                HostedZoneId = "string",
                DNSName = "string",
                EvaluateTargetHealth = TRUE|FALSE
              ),
              HealthCheckId = "string",
              TrafficPolicyInstanceId = "string",
              CidrRoutingConfig = list(
                CollectionId = "string",
                LocationName = "string"
              )
            )
          )
        )
      )
    )

### Examples

    ## Not run: 
    # The following example creates a resource record set that routes Internet
    # traffic to a resource with an IP address of 192.0.2.44.
    svc$change_resource_record_sets(
      ChangeBatch = list(
        Changes = list(
          list(
            Action = "CREATE",
            ResourceRecordSet = list(
              Name = "example.com",
              ResourceRecords = list(
                list(
                  Value = "192.0.2.44"
                )
              ),
              TTL = 60L,
              Type = "A"
            )
          )
        ),
        Comment = "Web server for example.com"
      ),
      HostedZoneId = "Z3M3LMPEXAMPLE"
    )

    # The following example creates two weighted resource record sets. The
    # resource with a Weight of 100 will get 1/3rd of traffic (100/100+200),
    # and the other resource will get the rest of the traffic for example.com.
    svc$change_resource_record_sets(
      ChangeBatch = list(
        Changes = list(
          list(
            Action = "CREATE",
            ResourceRecordSet = list(
              HealthCheckId = "abcdef11-2222-3333-4444-555555fedcba",
              Name = "example.com",
              ResourceRecords = list(
                list(
                  Value = "192.0.2.44"
                )
              ),
              SetIdentifier = "Seattle data center",
              TTL = 60L,
              Type = "A",
              Weight = 100L
            )
          ),
          list(
            Action = "CREATE",
            ResourceRecordSet = list(
              HealthCheckId = "abcdef66-7777-8888-9999-000000fedcba",
              Name = "example.com",
              ResourceRecords = list(
                list(
                  Value = "192.0.2.45"
                )
              ),
              SetIdentifier = "Portland data center",
              TTL = 60L,
              Type = "A",
              Weight = 200L
            )
          )
        ),
        Comment = "Web servers for example.com"
      ),
      HostedZoneId = "Z3M3LMPEXAMPLE"
    )

    # The following example creates an alias resource record set that routes
    # traffic to a CloudFront distribution.
    svc$change_resource_record_sets(
      ChangeBatch = list(
        Changes = list(
          list(
            Action = "CREATE",
            ResourceRecordSet = list(
              AliasTarget = list(
                DNSName = "d123rk29d0stfj.cloudfront.net",
                EvaluateTargetHealth = FALSE,
                HostedZoneId = "Z2FDTNDATAQYW2"
              ),
              Name = "example.com",
              Type = "A"
            )
          )
        ),
        Comment = "CloudFront distribution for example.com"
      ),
      HostedZoneId = "Z3M3LMPEXAMPLE"
    )

    # The following example creates two weighted alias resource record sets
    # that route traffic to ELB load balancers. The resource with a Weight of
    # 100 will get 1/3rd of traffic (100/100+200), and the other resource will
    # get the rest of the traffic for example.com.
    svc$change_resource_record_sets(
      ChangeBatch = list(
        Changes = list(
          list(
            Action = "CREATE",
            ResourceRecordSet = list(
              AliasTarget = list(
                DNSName = "example-com-123456789.us-east-2.elb.amazonaws.com ",
                EvaluateTargetHealth = TRUE,
                HostedZoneId = "Z3AADJGX6KTTL2"
              ),
              Name = "example.com",
              SetIdentifier = "Ohio region",
              Type = "A",
              Weight = 100L
            )
          ),
          list(
            Action = "CREATE",
            ResourceRecordSet = list(
              AliasTarget = list(
                DNSName = "example-com-987654321.us-west-2.elb.amazonaws.com ",
                EvaluateTargetHealth = TRUE,
                HostedZoneId = "Z1H1FL5HABSF5"
              ),
              Name = "example.com",
              SetIdentifier = "Oregon region",
              Type = "A",
              Weight = 200L
            )
          )
        ),
        Comment = "ELB load balancers for example.com"
      ),
      HostedZoneId = "Z3M3LMPEXAMPLE"
    )

    # The following example creates two latency resource record sets that
    # route traffic to EC2 instances. Traffic for example.com is routed either
    # to the Ohio region or the Oregon region, depending on the latency
    # between the user and those regions.
    svc$change_resource_record_sets(
      ChangeBatch = list(
        Changes = list(
          list(
            Action = "CREATE",
            ResourceRecordSet = list(
              HealthCheckId = "abcdef11-2222-3333-4444-555555fedcba",
              Name = "example.com",
              Region = "us-east-2",
              ResourceRecords = list(
                list(
                  Value = "192.0.2.44"
                )
              ),
              SetIdentifier = "Ohio region",
              TTL = 60L,
              Type = "A"
            )
          ),
          list(
            Action = "CREATE",
            ResourceRecordSet = list(
              HealthCheckId = "abcdef66-7777-8888-9999-000000fedcba",
              Name = "example.com",
              Region = "us-west-2",
              ResourceRecords = list(
                list(
                  Value = "192.0.2.45"
                )
              ),
              SetIdentifier = "Oregon region",
              TTL = 60L,
              Type = "A"
            )
          )
        ),
        Comment = "EC2 instances for example.com"
      ),
      HostedZoneId = "Z3M3LMPEXAMPLE"
    )

    # The following example creates two latency alias resource record sets
    # that route traffic for example.com to ELB load balancers. Requests are
    # routed either to the Ohio region or the Oregon region, depending on the
    # latency between the user and those regions.
    svc$change_resource_record_sets(
      ChangeBatch = list(
        Changes = list(
          list(
            Action = "CREATE",
            ResourceRecordSet = list(
              AliasTarget = list(
                DNSName = "example-com-123456789.us-east-2.elb.amazonaws.com ",
                EvaluateTargetHealth = TRUE,
                HostedZoneId = "Z3AADJGX6KTTL2"
              ),
              Name = "example.com",
              Region = "us-east-2",
              SetIdentifier = "Ohio region",
              Type = "A"
            )
          ),
          list(
            Action = "CREATE",
            ResourceRecordSet = list(
              AliasTarget = list(
                DNSName = "example-com-987654321.us-west-2.elb.amazonaws.com ",
                EvaluateTargetHealth = TRUE,
                HostedZoneId = "Z1H1FL5HABSF5"
              ),
              Name = "example.com",
              Region = "us-west-2",
              SetIdentifier = "Oregon region",
              Type = "A"
            )
          )
        ),
        Comment = "ELB load balancers for example.com"
      ),
      HostedZoneId = "Z3M3LMPEXAMPLE"
    )

    # The following example creates primary and secondary failover resource
    # record sets that route traffic to EC2 instances. Traffic is generally
    # routed to the primary resource, in the Ohio region. If that resource is
    # unavailable, traffic is routed to the secondary resource, in the Oregon
    # region.
    svc$change_resource_record_sets(
      ChangeBatch = list(
        Changes = list(
          list(
            Action = "CREATE",
            ResourceRecordSet = list(
              Failover = "PRIMARY",
              HealthCheckId = "abcdef11-2222-3333-4444-555555fedcba",
              Name = "example.com",
              ResourceRecords = list(
                list(
                  Value = "192.0.2.44"
                )
              ),
              SetIdentifier = "Ohio region",
              TTL = 60L,
              Type = "A"
            )
          ),
          list(
            Action = "CREATE",
            ResourceRecordSet = list(
              Failover = "SECONDARY",
              HealthCheckId = "abcdef66-7777-8888-9999-000000fedcba",
              Name = "example.com",
              ResourceRecords = list(
                list(
                  Value = "192.0.2.45"
                )
              ),
              SetIdentifier = "Oregon region",
              TTL = 60L,
              Type = "A"
            )
          )
        ),
        Comment = "Failover configuration for example.com"
      ),
      HostedZoneId = "Z3M3LMPEXAMPLE"
    )

    # The following example creates primary and secondary failover alias
    # resource record sets that route traffic to ELB load balancers. Traffic
    # is generally routed to the primary resource, in the Ohio region. If that
    # resource is unavailable, traffic is routed to the secondary resource, in
    # the Oregon region.
    svc$change_resource_record_sets(
      ChangeBatch = list(
        Changes = list(
          list(
            Action = "CREATE",
            ResourceRecordSet = list(
              AliasTarget = list(
                DNSName = "example-com-123456789.us-east-2.elb.amazonaws.com ",
                EvaluateTargetHealth = TRUE,
                HostedZoneId = "Z3AADJGX6KTTL2"
              ),
              Failover = "PRIMARY",
              Name = "example.com",
              SetIdentifier = "Ohio region",
              Type = "A"
            )
          ),
          list(
            Action = "CREATE",
            ResourceRecordSet = list(
              AliasTarget = list(
                DNSName = "example-com-987654321.us-west-2.elb.amazonaws.com ",
                EvaluateTargetHealth = TRUE,
                HostedZoneId = "Z1H1FL5HABSF5"
              ),
              Failover = "SECONDARY",
              Name = "example.com",
              SetIdentifier = "Oregon region",
              Type = "A"
            )
          )
        ),
        Comment = "Failover alias configuration for example.com"
      ),
      HostedZoneId = "Z3M3LMPEXAMPLE"
    )

    # The following example creates four geolocation resource record sets that
    # use IPv4 addresses to route traffic to resources such as web servers
    # running on EC2 instances. Traffic is routed to one of four IP addresses,
    # for North America (NA), for South America (SA), for Europe (EU), and for
    # all other locations (*).
    svc$change_resource_record_sets(
      ChangeBatch = list(
        Changes = list(
          list(
            Action = "CREATE",
            ResourceRecordSet = list(
              GeoLocation = list(
                ContinentCode = "NA"
              ),
              Name = "example.com",
              ResourceRecords = list(
                list(
                  Value = "192.0.2.44"
                )
              ),
              SetIdentifier = "North America",
              TTL = 60L,
              Type = "A"
            )
          ),
          list(
            Action = "CREATE",
            ResourceRecordSet = list(
              GeoLocation = list(
                ContinentCode = "SA"
              ),
              Name = "example.com",
              ResourceRecords = list(
                list(
                  Value = "192.0.2.45"
                )
              ),
              SetIdentifier = "South America",
              TTL = 60L,
              Type = "A"
            )
          ),
          list(
            Action = "CREATE",
            ResourceRecordSet = list(
              GeoLocation = list(
                ContinentCode = "EU"
              ),
              Name = "example.com",
              ResourceRecords = list(
                list(
                  Value = "192.0.2.46"
                )
              ),
              SetIdentifier = "Europe",
              TTL = 60L,
              Type = "A"
            )
          ),
          list(
            Action = "CREATE",
            ResourceRecordSet = list(
              GeoLocation = list(
                CountryCode = "*"
              ),
              Name = "example.com",
              ResourceRecords = list(
                list(
                  Value = "192.0.2.47"
                )
              ),
              SetIdentifier = "Other locations",
              TTL = 60L,
              Type = "A"
            )
          )
        ),
        Comment = "Geolocation configuration for example.com"
      ),
      HostedZoneId = "Z3M3LMPEXAMPLE"
    )

    # The following example creates four geolocation alias resource record
    # sets that route traffic to ELB load balancers. Traffic is routed to one
    # of four IP addresses, for North America (NA), for South America (SA),
    # for Europe (EU), and for all other locations (*).
    svc$change_resource_record_sets(
      ChangeBatch = list(
        Changes = list(
          list(
            Action = "CREATE",
            ResourceRecordSet = list(
              AliasTarget = list(
                DNSName = "example-com-123456789.us-east-2.elb.amazonaws.com ",
                EvaluateTargetHealth = TRUE,
                HostedZoneId = "Z3AADJGX6KTTL2"
              ),
              GeoLocation = list(
                ContinentCode = "NA"
              ),
              Name = "example.com",
              SetIdentifier = "North America",
              Type = "A"
            )
          ),
          list(
            Action = "CREATE",
            ResourceRecordSet = list(
              AliasTarget = list(
                DNSName = "example-com-234567890.sa-east-1.elb.amazonaws.com ",
                EvaluateTargetHealth = TRUE,
                HostedZoneId = "Z2P70J7HTTTPLU"
              ),
              GeoLocation = list(
                ContinentCode = "SA"
              ),
              Name = "example.com",
              SetIdentifier = "South America",
              Type = "A"
            )
          ),
          list(
            Action = "CREATE",
            ResourceRecordSet = list(
              AliasTarget = list(
                DNSName = "example-com-234567890.eu-central-1.elb.amazonaws.com ",
                EvaluateTargetHealth = TRUE,
                HostedZoneId = "Z215JYRZR1TBD5"
              ),
              GeoLocation = list(
                ContinentCode = "EU"
              ),
              Name = "example.com",
              SetIdentifier = "Europe",
              Type = "A"
            )
          ),
          list(
            Action = "CREATE",
            ResourceRecordSet = list(
              AliasTarget = list(
                DNSName = "example-com-234567890.ap-southeast-1.elb.amazonaws.com ",
                EvaluateTargetHealth = TRUE,
                HostedZoneId = "Z1LMS91P8CMLE5"
              ),
              GeoLocation = list(
                CountryCode = "*"
              ),
              Name = "example.com",
              SetIdentifier = "Other locations",
              Type = "A"
            )
          )
        ),
        Comment = "Geolocation alias configuration for example.com"
      ),
      HostedZoneId = "Z3M3LMPEXAMPLE"
    )

    ## End(Not run)