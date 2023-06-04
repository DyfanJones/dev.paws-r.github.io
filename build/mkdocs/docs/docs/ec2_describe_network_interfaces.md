<table style="width: 100%;">
<tbody>
<tr class="odd">
<td>ec2_describe_network_interfaces</td>
<td style="text-align: right;">R Documentation</td>
</tr>
</tbody>
</table>

## Describes one or more of your network interfaces

### Description

Describes one or more of your network interfaces.

### Usage

    ec2_describe_network_interfaces(Filters, DryRun, NetworkInterfaceIds,
      NextToken, MaxResults)

### Arguments

<table>
<colgroup>
<col style="width: 35%" />
<col style="width: 65%" />
</colgroup>
<tbody>
<tr class="odd">
<td><code
id="ec2_describe_network_interfaces_:_Filters">Filters</code></td>
<td><p>One or more filters.</p>
<ul>
<li><p><code>addresses.private-ip-address</code> - The private IPv4
addresses associated with the network interface.</p></li>
<li><p><code>addresses.primary</code> - Whether the private IPv4 address
is the primary IP address associated with the network
interface.</p></li>
<li><p><code>addresses.association.public-ip</code> - The association ID
returned when the network interface was associated with the Elastic IP
address (IPv4).</p></li>
<li><p><code>addresses.association.owner-id</code> - The owner ID of the
addresses associated with the network interface.</p></li>
<li><p><code>association.association-id</code> - The association ID
returned when the network interface was associated with an IPv4
address.</p></li>
<li><p><code>association.allocation-id</code> - The allocation ID
returned when you allocated the Elastic IP address (IPv4) for your
network interface.</p></li>
<li><p><code>association.ip-owner-id</code> - The owner of the Elastic
IP address (IPv4) associated with the network interface.</p></li>
<li><p><code>association.public-ip</code> - The address of the Elastic
IP address (IPv4) bound to the network interface.</p></li>
<li><p><code>association.public-dns-name</code> - The public DNS name
for the network interface (IPv4).</p></li>
<li><p><code>attachment.attachment-id</code> - The ID of the interface
attachment.</p></li>
<li><p><code>attachment.attach-time</code> - The time that the network
interface was attached to an instance.</p></li>
<li><p><code>attachment.delete-on-termination</code> - Indicates whether
the attachment is deleted when an instance is terminated.</p></li>
<li><p><code>attachment.device-index</code> - The device index to which
the network interface is attached.</p></li>
<li><p><code>attachment.instance-id</code> - The ID of the instance to
which the network interface is attached.</p></li>
<li><p><code>attachment.instance-owner-id</code> - The owner ID of the
instance to which the network interface is attached.</p></li>
<li><p><code>attachment.status</code> - The status of the attachment
(<code>attaching</code> | <code>attached</code> | <code>detaching</code>
| <code>detached</code>).</p></li>
<li><p><code>availability-zone</code> - The Availability Zone of the
network interface.</p></li>
<li><p><code>description</code> - The description of the network
interface.</p></li>
<li><p><code>group-id</code> - The ID of a security group associated
with the network interface.</p></li>
<li><p><code>group-name</code> - The name of a security group associated
with the network interface.</p></li>
<li><p><code>ipv6-addresses.ipv6-address</code> - An IPv6 address
associated with the network interface.</p></li>
<li><p><code>interface-type</code> - The type of network interface
(<code>api_gateway_managed</code> |
<code>aws_codestar_connections_managed</code> | <code>branch</code> |
<code>efa</code> | <code>gateway_load_balancer</code> |
<code>gateway_load_balancer_endpoint</code> |
<code>global_accelerator_managed</code> | <code>interface</code> |
<code>iot_rules_managed</code> | <code>lambda</code> |
<code>load_balancer</code> | <code>nat_gateway</code> |
<code>network_load_balancer</code> | <code>quicksight</code> |
<code>transit_gateway</code> | <code>trunk</code> |
<code>vpc_endpoint</code>).</p></li>
<li><p><code>mac-address</code> - The MAC address of the network
interface.</p></li>
<li><p><code>network-interface-id</code> - The ID of the network
interface.</p></li>
<li><p><code>owner-id</code> - The Amazon Web Services account ID of the
network interface owner.</p></li>
<li><p><code>private-ip-address</code> - The private IPv4 address or
addresses of the network interface.</p></li>
<li><p><code>private-dns-name</code> - The private DNS name of the
network interface (IPv4).</p></li>
<li><p><code>requester-id</code> - The alias or Amazon Web Services
account ID of the principal or service that created the network
interface.</p></li>
<li><p><code>requester-managed</code> - Indicates whether the network
interface is being managed by an Amazon Web Service (for example, Amazon
Web Services Management Console, Auto Scaling, and so on).</p></li>
<li><p><code>source-dest-check</code> - Indicates whether the network
interface performs source/destination checking. A value of
<code>true</code> means checking is enabled, and <code>false</code>
means checking is disabled. The value must be <code>false</code> for the
network interface to perform network address translation (NAT) in your
VPC.</p></li>
<li><p><code>status</code> - The status of the network interface. If the
network interface is not attached to an instance, the status is
<code>available</code>; if a network interface is attached to an
instance the status is <code
style="white-space: pre;">⁠in-use⁠</code>.</p></li>
<li><p><code>subnet-id</code> - The ID of the subnet for the network
interface.</p></li>
<li><p><code>tag</code>:\&lt;key\&gt; - The key/value combination of a
tag assigned to the resource. Use the tag key in the filter name and the
tag value as the filter value. For example, to find all resources that
have a tag with the key <code>Owner</code> and the value
<code>TeamA</code>, specify <code>tag:Owner</code> for the filter name
and <code>TeamA</code> for the filter value.</p></li>
<li><p><code>tag-key</code> - The key of a tag assigned to the resource.
Use this filter to find all resources assigned a tag with a specific
key, regardless of the tag value.</p></li>
<li><p><code>vpc-id</code> - The ID of the VPC for the network
interface.</p></li>
</ul></td>
</tr>
<tr class="even">
<td><code
id="ec2_describe_network_interfaces_:_DryRun">DryRun</code></td>
<td><p>Checks whether you have the required permissions for the action,
without actually making the request, and provides an error response. If
you have the required permissions, the error response is
<code>DryRunOperation</code>. Otherwise, it is
<code>UnauthorizedOperation</code>.</p></td>
</tr>
<tr class="odd">
<td><code
id="ec2_describe_network_interfaces_:_NetworkInterfaceIds">NetworkInterfaceIds</code></td>
<td><p>The network interface IDs.</p>
<p>Default: Describes all your network interfaces.</p></td>
</tr>
<tr class="even">
<td><code
id="ec2_describe_network_interfaces_:_NextToken">NextToken</code></td>
<td><p>The token returned from a previous paginated request. Pagination
continues from the end of the items returned by the previous
request.</p></td>
</tr>
<tr class="odd">
<td><code
id="ec2_describe_network_interfaces_:_MaxResults">MaxResults</code></td>
<td><p>The maximum number of items to return for this request. To get
the next page of items, make another request with the token returned in
the output. You cannot specify this parameter and the network interface
IDs parameter in the same request. For more information, see <a
href="https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Query-Requests.html#api-pagination">Pagination</a>.</p></td>
</tr>
</tbody>
</table>

### Value

A list with the following syntax:

    list(
      NetworkInterfaces = list(
        list(
          Association = list(
            AllocationId = "string",
            AssociationId = "string",
            IpOwnerId = "string",
            PublicDnsName = "string",
            PublicIp = "string",
            CustomerOwnedIp = "string",
            CarrierIp = "string"
          ),
          Attachment = list(
            AttachTime = as.POSIXct(
              "2015-01-01"
            ),
            AttachmentId = "string",
            DeleteOnTermination = TRUE|FALSE,
            DeviceIndex = 123,
            NetworkCardIndex = 123,
            InstanceId = "string",
            InstanceOwnerId = "string",
            Status = "attaching"|"attached"|"detaching"|"detached",
            EnaSrdSpecification = list(
              EnaSrdEnabled = TRUE|FALSE,
              EnaSrdUdpSpecification = list(
                EnaSrdUdpEnabled = TRUE|FALSE
              )
            )
          ),
          AvailabilityZone = "string",
          Description = "string",
          Groups = list(
            list(
              GroupName = "string",
              GroupId = "string"
            )
          ),
          InterfaceType = "interface"|"natGateway"|"efa"|"trunk"|"load_balancer"|"network_load_balancer"|"vpc_endpoint"|"branch"|"transit_gateway"|"lambda"|"quicksight"|"global_accelerator_managed"|"api_gateway_managed"|"gateway_load_balancer"|"gateway_load_balancer_endpoint"|"iot_rules_managed"|"aws_codestar_connections_managed",
          Ipv6Addresses = list(
            list(
              Ipv6Address = "string"
            )
          ),
          MacAddress = "string",
          NetworkInterfaceId = "string",
          OutpostArn = "string",
          OwnerId = "string",
          PrivateDnsName = "string",
          PrivateIpAddress = "string",
          PrivateIpAddresses = list(
            list(
              Association = list(
                AllocationId = "string",
                AssociationId = "string",
                IpOwnerId = "string",
                PublicDnsName = "string",
                PublicIp = "string",
                CustomerOwnedIp = "string",
                CarrierIp = "string"
              ),
              Primary = TRUE|FALSE,
              PrivateDnsName = "string",
              PrivateIpAddress = "string"
            )
          ),
          Ipv4Prefixes = list(
            list(
              Ipv4Prefix = "string"
            )
          ),
          Ipv6Prefixes = list(
            list(
              Ipv6Prefix = "string"
            )
          ),
          RequesterId = "string",
          RequesterManaged = TRUE|FALSE,
          SourceDestCheck = TRUE|FALSE,
          Status = "available"|"associated"|"attaching"|"in-use"|"detaching",
          SubnetId = "string",
          TagSet = list(
            list(
              Key = "string",
              Value = "string"
            )
          ),
          VpcId = "string",
          DenyAllIgwTraffic = TRUE|FALSE,
          Ipv6Native = TRUE|FALSE,
          Ipv6Address = "string"
        )
      ),
      NextToken = "string"
    )

### Request syntax

    svc$describe_network_interfaces(
      Filters = list(
        list(
          Name = "string",
          Values = list(
            "string"
          )
        )
      ),
      DryRun = TRUE|FALSE,
      NetworkInterfaceIds = list(
        "string"
      ),
      NextToken = "string",
      MaxResults = 123
    )

### Examples

    ## Not run: 
    # 
    svc$describe_network_interfaces(
      NetworkInterfaceIds = list(
        "eni-e5aa89a3"
      )
    )

    ## End(Not run)