<table style="width: 100%;">
<tbody>
<tr class="odd">
<td>ec2_associate_address</td>
<td style="text-align: right;">R Documentation</td>
</tr>
</tbody>
</table>

## Associates an Elastic IP address, or carrier IP address (for instances that are in subnets in Wavelength Zones) with an instance or a network interface

### Description

Associates an Elastic IP address, or carrier IP address (for instances
that are in subnets in Wavelength Zones) with an instance or a network
interface. Before you can use an Elastic IP address, you must allocate
it to your account.

An Elastic IP address is for use in either the EC2-Classic platform or
in a VPC. For more information, see [Elastic IP
Addresses](https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/elastic-ip-addresses-eip.html)
in the *Amazon Elastic Compute Cloud User Guide*.

\[EC2-Classic, VPC in an EC2-VPC-only account\] If the Elastic IP
address is already associated with a different instance, it is
disassociated from that instance and associated with the specified
instance. If you associate an Elastic IP address with an instance that
has an existing Elastic IP address, the existing address is
disassociated from the instance, but remains allocated to your account.

\[VPC in an EC2-Classic account\] If you don't specify a private IP
address, the Elastic IP address is associated with the primary IP
address. If the Elastic IP address is already associated with a
different instance or a network interface, you get an error unless you
allow reassociation. You cannot associate an Elastic IP address with an
instance or network interface that has an existing Elastic IP address.

\[Subnets in Wavelength Zones\] You can associate an IP address from the
telecommunication carrier to the instance or network interface.

You cannot associate an Elastic IP address with an interface in a
different network border group.

This is an idempotent operation. If you perform the operation more than
once, Amazon EC2 doesn't return an error, and you may be charged for
each time the Elastic IP address is remapped to the same instance. For
more information, see the *Elastic IP Addresses* section of [Amazon EC2
Pricing](https://aws.amazon.com/ec2/pricing/).

We are retiring EC2-Classic. We recommend that you migrate from
EC2-Classic to a VPC. For more information, see [Migrate from
EC2-Classic to a
VPC](https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/vpc-migrate.html)
in the *Amazon Elastic Compute Cloud User Guide*.

### Usage

    ec2_associate_address(AllocationId, InstanceId, PublicIp,
      AllowReassociation, DryRun, NetworkInterfaceId, PrivateIpAddress)

### Arguments

<table>
<colgroup>
<col style="width: 35%" />
<col style="width: 65%" />
</colgroup>
<tbody>
<tr class="odd">
<td><code
id="ec2_associate_address_:_AllocationId">AllocationId</code></td>
<td><p>[EC2-VPC] The allocation ID. This is required for
EC2-VPC.</p></td>
</tr>
<tr class="even">
<td><code id="ec2_associate_address_:_InstanceId">InstanceId</code></td>
<td><p>The ID of the instance. The instance must have exactly one
attached network interface. For EC2-VPC, you can specify either the
instance ID or the network interface ID, but not both. For EC2-Classic,
you must specify an instance ID and the instance must be in the running
state.</p></td>
</tr>
<tr class="odd">
<td><code id="ec2_associate_address_:_PublicIp">PublicIp</code></td>
<td><p>[EC2-Classic] The Elastic IP address to associate with the
instance. This is required for EC2-Classic.</p></td>
</tr>
<tr class="even">
<td><code
id="ec2_associate_address_:_AllowReassociation">AllowReassociation</code></td>
<td><p>[EC2-VPC] For a VPC in an EC2-Classic account, specify true to
allow an Elastic IP address that is already associated with an instance
or network interface to be reassociated with the specified instance or
network interface. Otherwise, the operation fails. In a VPC in an
EC2-VPC-only account, reassociation is automatic, therefore you can
specify false to ensure the operation fails if the Elastic IP address is
already associated with another resource.</p></td>
</tr>
<tr class="odd">
<td><code id="ec2_associate_address_:_DryRun">DryRun</code></td>
<td><p>Checks whether you have the required permissions for the action,
without actually making the request, and provides an error response. If
you have the required permissions, the error response is
<code>DryRunOperation</code>. Otherwise, it is
<code>UnauthorizedOperation</code>.</p></td>
</tr>
<tr class="even">
<td><code
id="ec2_associate_address_:_NetworkInterfaceId">NetworkInterfaceId</code></td>
<td><p>[EC2-VPC] The ID of the network interface. If the instance has
more than one network interface, you must specify a network interface
ID.</p>
<p>For EC2-VPC, you can specify either the instance ID or the network
interface ID, but not both.</p></td>
</tr>
<tr class="odd">
<td><code
id="ec2_associate_address_:_PrivateIpAddress">PrivateIpAddress</code></td>
<td><p>[EC2-VPC] The primary or secondary private IP address to
associate with the Elastic IP address. If no private IP address is
specified, the Elastic IP address is associated with the primary private
IP address.</p></td>
</tr>
</tbody>
</table>

### Value

A list with the following syntax:

    list(
      AssociationId = "string"
    )

### Request syntax

    svc$associate_address(
      AllocationId = "string",
      InstanceId = "string",
      PublicIp = "string",
      AllowReassociation = TRUE|FALSE,
      DryRun = TRUE|FALSE,
      NetworkInterfaceId = "string",
      PrivateIpAddress = "string"
    )

### Examples

    ## Not run: 
    # This example associates the specified Elastic IP address with the
    # specified instance in a VPC.
    svc$associate_address(
      AllocationId = "eipalloc-64d5890a",
      InstanceId = "i-0b263919b6498b123"
    )

    # This example associates the specified Elastic IP address with the
    # specified network interface.
    svc$associate_address(
      AllocationId = "eipalloc-64d5890a",
      NetworkInterfaceId = "eni-1a2b3c4d"
    )

    # This example associates an Elastic IP address with an instance in
    # EC2-Classic.
    svc$associate_address(
      InstanceId = "i-07ffe74c7330ebf53",
      PublicIp = "198.51.100.0"
    )

    ## End(Not run)