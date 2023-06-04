<table style="width: 100%;">
<tbody>
<tr class="odd">
<td>lightsail_attach_instances_to_load_balancer</td>
<td style="text-align: right;">R Documentation</td>
</tr>
</tbody>
</table>

## Attaches one or more Lightsail instances to a load balancer

### Description

Attaches one or more Lightsail instances to a load balancer.

After some time, the instances are attached to the load balancer and the
health check status is available.

The `⁠attach instances to load balancer⁠` operation supports tag-based
access control via resource tags applied to the resource identified by
`⁠load balancer name⁠`. For more information, see the Lightsail Developer
Guide.

### Usage

    lightsail_attach_instances_to_load_balancer(loadBalancerName,
      instanceNames)

### Arguments

<table>
<colgroup>
<col style="width: 35%" />
<col style="width: 65%" />
</colgroup>
<tbody>
<tr class="odd">
<td><code
id="lightsail_attach_instances_to_load_balancer_:_loadBalancerName">loadBalancerName</code></td>
<td><p>[required] The name of the load balancer.</p></td>
</tr>
<tr class="even">
<td><code
id="lightsail_attach_instances_to_load_balancer_:_instanceNames">instanceNames</code></td>
<td><p>[required] An array of strings representing the instance name(s)
you want to attach to your load balancer.</p>
<p>An instance must be <code>running</code> before you can attach it to
your load balancer.</p>
<p>There are no additional limits on the number of instances you can
attach to your load balancer, aside from the limit of Lightsail
instances you can create in your account (20).</p></td>
</tr>
</tbody>
</table>

### Value

A list with the following syntax:

    list(
      operations = list(
        list(
          id = "string",
          resourceName = "string",
          resourceType = "ContainerService"|"Instance"|"StaticIp"|"KeyPair"|"InstanceSnapshot"|"Domain"|"PeeredVpc"|"LoadBalancer"|"LoadBalancerTlsCertificate"|"Disk"|"DiskSnapshot"|"RelationalDatabase"|"RelationalDatabaseSnapshot"|"ExportSnapshotRecord"|"CloudFormationStackRecord"|"Alarm"|"ContactMethod"|"Distribution"|"Certificate"|"Bucket",
          createdAt = as.POSIXct(
            "2015-01-01"
          ),
          location = list(
            availabilityZone = "string",
            regionName = "us-east-1"|"us-east-2"|"us-west-1"|"us-west-2"|"eu-west-1"|"eu-west-2"|"eu-west-3"|"eu-central-1"|"ca-central-1"|"ap-south-1"|"ap-southeast-1"|"ap-southeast-2"|"ap-northeast-1"|"ap-northeast-2"|"eu-north-1"
          ),
          isTerminal = TRUE|FALSE,
          operationDetails = "string",
          operationType = "DeleteKnownHostKeys"|"DeleteInstance"|"CreateInstance"|"StopInstance"|"StartInstance"|"RebootInstance"|"OpenInstancePublicPorts"|"PutInstancePublicPorts"|"CloseInstancePublicPorts"|"AllocateStaticIp"|"ReleaseStaticIp"|"AttachStaticIp"|"DetachStaticIp"|"UpdateDomainEntry"|"DeleteDomainEntry"|"CreateDomain"|"DeleteDomain"|"CreateInstanceSnapshot"|"DeleteInstanceSnapshot"|"CreateInstancesFromSnapshot"|"CreateLoadBalancer"|"DeleteLoadBalancer"|"AttachInstancesToLoadBalancer"|"DetachInstancesFromLoadBalancer"|"UpdateLoadBalancerAttribute"|"CreateLoadBalancerTlsCertificate"|"DeleteLoadBalancerTlsCertificate"|"AttachLoadBalancerTlsCertificate"|"CreateDisk"|"DeleteDisk"|"AttachDisk"|"DetachDisk"|"CreateDiskSnapshot"|"DeleteDiskSnapshot"|"CreateDiskFromSnapshot"|"CreateRelationalDatabase"|"UpdateRelationalDatabase"|"DeleteRelationalDatabase"|"CreateRelationalDatabaseFromSnapshot"|"CreateRelationalDatabaseSnapshot"|"DeleteRelationalDatabaseSnapshot"|"UpdateRelationalDatabaseParameters"|"StartRelationalDatabase"|"RebootRelationalDatabase"|"StopRelationalDatabase"|"EnableAddOn"|"DisableAddOn"|"PutAlarm"|"GetAlarms"|"DeleteAlarm"|"TestAlarm"|"CreateContactMethod"|"GetContactMethods"|"SendContactMethodVerification"|"DeleteContactMethod"|"CreateDistribution"|"UpdateDistribution"|"DeleteDistribution"|"ResetDistributionCache"|"AttachCertificateToDistribution"|"DetachCertificateFromDistribution"|"UpdateDistributionBundle"|"SetIpAddressType"|"CreateCertificate"|"DeleteCertificate"|"CreateContainerService"|"UpdateContainerService"|"DeleteContainerService"|"CreateContainerServiceDeployment"|"CreateContainerServiceRegistryLogin"|"RegisterContainerImage"|"DeleteContainerImage"|"CreateBucket"|"DeleteBucket"|"CreateBucketAccessKey"|"DeleteBucketAccessKey"|"UpdateBucketBundle"|"UpdateBucket"|"SetResourceAccessForBucket"|"UpdateInstanceMetadataOptions"|"StartGUISession"|"StopGUISession",
          status = "NotStarted"|"Started"|"Failed"|"Completed"|"Succeeded",
          statusChangedAt = as.POSIXct(
            "2015-01-01"
          ),
          errorCode = "string",
          errorDetails = "string"
        )
      )
    )

### Request syntax

    svc$attach_instances_to_load_balancer(
      loadBalancerName = "string",
      instanceNames = list(
        "string"
      )
    )