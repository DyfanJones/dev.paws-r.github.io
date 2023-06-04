<table style="width: 100%;">
<tbody>
<tr class="odd">
<td>applicationautoscaling_register_scalable_target</td>
<td style="text-align: right;">R Documentation</td>
</tr>
</tbody>
</table>

## Registers or updates a scalable target, which is the resource that you want to scale

### Description

Registers or updates a scalable target, which is the resource that you
want to scale.

Scalable targets are uniquely identified by the combination of resource
ID, scalable dimension, and namespace, which represents some capacity
dimension of the underlying service.

When you register a new scalable target, you must specify values for the
minimum and maximum capacity. If the specified resource is not active in
the target service, this operation does not change the resource's
current capacity. Otherwise, it changes the resource's current capacity
to a value that is inside of this range.

If you add a scaling policy, current capacity is adjustable within the
specified range when scaling starts. Application Auto Scaling scaling
policies will not scale capacity to values that are outside of the
minimum and maximum range.

After you register a scalable target, you do not need to register it
again to use other Application Auto Scaling operations. To see which
resources have been registered, use `describe_scalable_targets`. You can
also view the scaling policies for a service namespace by using
`describe_scalable_targets`. If you no longer need a scalable target,
you can deregister it by using `deregister_scalable_target`.

To update a scalable target, specify the parameters that you want to
change. Include the parameters that identify the scalable target:
resource ID, scalable dimension, and namespace. Any parameters that you
don't specify are not changed by this update request.

If you call the `register_scalable_target` API operation to create a
scalable target, there might be a brief delay until the operation
achieves [eventual
consistency](https://en.wikipedia.org/wiki/Eventual_consistency). You
might become aware of this brief delay if you get unexpected errors when
performing sequential operations. The typical strategy is to retry the
request, and some Amazon Web Services SDKs include automatic backoff and
retry logic.

If you call the `register_scalable_target` API operation to update an
existing scalable target, Application Auto Scaling retrieves the current
capacity of the resource. If it's below the minimum capacity or above
the maximum capacity, Application Auto Scaling adjusts the capacity of
the scalable target to place it within these bounds, even if you don't
include the `MinCapacity` or `MaxCapacity` request parameters.

### Usage

    applicationautoscaling_register_scalable_target(ServiceNamespace,
      ResourceId, ScalableDimension, MinCapacity, MaxCapacity, RoleARN,
      SuspendedState, Tags)

### Arguments

<table>
<colgroup>
<col style="width: 35%" />
<col style="width: 65%" />
</colgroup>
<tbody>
<tr class="odd">
<td><code
id="applicationautoscaling_register_scalable_target_:_ServiceNamespace">ServiceNamespace</code></td>
<td><p>[required] The namespace of the Amazon Web Services service that
provides the resource. For a resource provided by your own application
or service, use <code>custom-resource</code> instead.</p></td>
</tr>
<tr class="even">
<td><code
id="applicationautoscaling_register_scalable_target_:_ResourceId">ResourceId</code></td>
<td><p>[required] The identifier of the resource that is associated with
the scalable target. This string consists of the resource type and
unique identifier.</p>
<ul>
<li><p>ECS service - The resource type is <code>service</code> and the
unique identifier is the cluster name and service name. Example:
<code>service/default/sample-webapp</code>.</p></li>
<li><p>Spot Fleet - The resource type is <code>spot-fleet-request</code>
and the unique identifier is the Spot Fleet request ID. Example: <code
style="white-space: pre;">⁠spot-fleet-request/sfr-73fbd2ce-aa30-494c-8788-1cee4EXAMPLE⁠</code>.</p></li>
<li><p>EMR cluster - The resource type is <code>instancegroup</code> and
the unique identifier is the cluster ID and instance group ID. Example:
<code
style="white-space: pre;">⁠instancegroup/j-2EEZNYKUA1NTV/ig-1791Y4E1L8YI0⁠</code>.</p></li>
<li><p>AppStream 2.0 fleet - The resource type is <code>fleet</code> and
the unique identifier is the fleet name. Example:
<code>fleet/sample-fleet</code>.</p></li>
<li><p>DynamoDB table - The resource type is <code>table</code> and the
unique identifier is the table name. Example:
<code>table/my-table</code>.</p></li>
<li><p>DynamoDB global secondary index - The resource type is
<code>index</code> and the unique identifier is the index name. Example:
<code>table/my-table/index/my-table-index</code>.</p></li>
<li><p>Aurora DB cluster - The resource type is <code>cluster</code> and
the unique identifier is the cluster name. Example:
<code>cluster:my-db-cluster</code>.</p></li>
<li><p>SageMaker endpoint variant - The resource type is
<code>variant</code> and the unique identifier is the resource ID.
Example:
<code>endpoint/my-end-point/variant/KMeansClustering</code>.</p></li>
<li><p>Custom resources are not supported with a resource type. This
parameter must specify the <code>OutputValue</code> from the
CloudFormation template stack used to access the resources. The unique
identifier is defined by the service provider. More information is
available in our <a
href="https://github.com/aws/aws-auto-scaling-custom-resource">GitHub
repository</a>.</p></li>
<li><p>Amazon Comprehend document classification endpoint - The resource
type and unique identifier are specified using the endpoint ARN.
Example:
<code>arn:aws:comprehend:us-west-2:123456789012:document-classifier-endpoint/EXAMPLE</code>.</p></li>
<li><p>Amazon Comprehend entity recognizer endpoint - The resource type
and unique identifier are specified using the endpoint ARN. Example:
<code>arn:aws:comprehend:us-west-2:123456789012:entity-recognizer-endpoint/EXAMPLE</code>.</p></li>
<li><p>Lambda provisioned concurrency - The resource type is
<code>function</code> and the unique identifier is the function name
with a function version or alias name suffix that is not <code
style="white-space: pre;">⁠$LATEST⁠</code>. Example: <code
style="white-space: pre;">⁠function:my-function:prod⁠</code> or <code
style="white-space: pre;">⁠function:my-function:1⁠</code>.</p></li>
<li><p>Amazon Keyspaces table - The resource type is <code>table</code>
and the unique identifier is the table name. Example:
<code>keyspace/mykeyspace/table/mytable</code>.</p></li>
<li><p>Amazon MSK cluster - The resource type and unique identifier are
specified using the cluster ARN. Example: <code
style="white-space: pre;">⁠arn:aws:kafka:us-east-1:123456789012:cluster/demo-cluster-1/6357e0b2-0e6a-4b86-a0b4-70df934c2e31-5⁠</code>.</p></li>
<li><p>Amazon ElastiCache replication group - The resource type is
<code>replication-group</code> and the unique identifier is the
replication group name. Example:
<code>replication-group/mycluster</code>.</p></li>
<li><p>Neptune cluster - The resource type is <code>cluster</code> and
the unique identifier is the cluster name. Example:
<code>cluster:mycluster</code>.</p></li>
<li><p>SageMaker Serverless endpoint - The resource type is
<code>variant</code> and the unique identifier is the resource ID.
Example:
<code>endpoint/my-end-point/variant/KMeansClustering</code>.</p></li>
</ul></td>
</tr>
<tr class="odd">
<td><code
id="applicationautoscaling_register_scalable_target_:_ScalableDimension">ScalableDimension</code></td>
<td><p>[required] The scalable dimension associated with the scalable
target. This string consists of the service namespace, resource type,
and scaling property.</p>
<ul>
<li><p><code>ecs:service:DesiredCount</code> - The desired task count of
an ECS service.</p></li>
<li><p><code>elasticmapreduce:instancegroup:InstanceCount</code> - The
instance count of an EMR Instance Group.</p></li>
<li><p><code>ec2:spot-fleet-request:TargetCapacity</code> - The target
capacity of a Spot Fleet.</p></li>
<li><p><code>appstream:fleet:DesiredCapacity</code> - The desired
capacity of an AppStream 2.0 fleet.</p></li>
<li><p><code>dynamodb:table:ReadCapacityUnits</code> - The provisioned
read capacity for a DynamoDB table.</p></li>
<li><p><code>dynamodb:table:WriteCapacityUnits</code> - The provisioned
write capacity for a DynamoDB table.</p></li>
<li><p><code>dynamodb:index:ReadCapacityUnits</code> - The provisioned
read capacity for a DynamoDB global secondary index.</p></li>
<li><p><code>dynamodb:index:WriteCapacityUnits</code> - The provisioned
write capacity for a DynamoDB global secondary index.</p></li>
<li><p><code>rds:cluster:ReadReplicaCount</code> - The count of Aurora
Replicas in an Aurora DB cluster. Available for Aurora MySQL-compatible
edition and Aurora PostgreSQL-compatible edition.</p></li>
<li><p><code>sagemaker:variant:DesiredInstanceCount</code> - The number
of EC2 instances for a SageMaker model endpoint variant.</p></li>
<li><p><code>custom-resource:ResourceType:Property</code> - The scalable
dimension for a custom resource provided by your own application or
service.</p></li>
<li><p><code>comprehend:document-classifier-endpoint:DesiredInferenceUnits</code>
- The number of inference units for an Amazon Comprehend document
classification endpoint.</p></li>
<li><p><code>comprehend:entity-recognizer-endpoint:DesiredInferenceUnits</code>
- The number of inference units for an Amazon Comprehend entity
recognizer endpoint.</p></li>
<li><p><code
style="white-space: pre;">⁠lambda:function:ProvisionedConcurrency⁠</code>
- The provisioned concurrency for a Lambda function.</p></li>
<li><p><code>cassandra:table:ReadCapacityUnits</code> - The provisioned
read capacity for an Amazon Keyspaces table.</p></li>
<li><p><code>cassandra:table:WriteCapacityUnits</code> - The provisioned
write capacity for an Amazon Keyspaces table.</p></li>
<li><p><code>kafka:broker-storage:VolumeSize</code> - The provisioned
volume size (in GiB) for brokers in an Amazon MSK cluster.</p></li>
<li><p><code>elasticache:replication-group:NodeGroups</code> - The
number of node groups for an Amazon ElastiCache replication
group.</p></li>
<li><p><code>elasticache:replication-group:Replicas</code> - The number
of replicas per node group for an Amazon ElastiCache replication
group.</p></li>
<li><p><code>neptune:cluster:ReadReplicaCount</code> - The count of read
replicas in an Amazon Neptune DB cluster.</p></li>
<li><p><code>sagemaker:variant:DesiredProvisionedConcurrency</code> -
The provisioned concurrency for a SageMaker Serverless
endpoint.</p></li>
</ul></td>
</tr>
<tr class="even">
<td><code
id="applicationautoscaling_register_scalable_target_:_MinCapacity">MinCapacity</code></td>
<td><p>The minimum value that you plan to scale in to. When a scaling
policy is in effect, Application Auto Scaling can scale in (contract) as
needed to the minimum capacity limit in response to changing demand.
This property is required when registering a new scalable target.</p>
<p>For the following resources, the minimum value allowed is 0.</p>
<ul>
<li><p>AppStream 2.0 fleets</p></li>
<li><p>Aurora DB clusters</p></li>
<li><p>ECS services</p></li>
<li><p>EMR clusters</p></li>
<li><p>Lambda provisioned concurrency</p></li>
<li><p>SageMaker endpoint variants</p></li>
<li><p>SageMaker Serverless endpoint provisioned concurrency</p></li>
<li><p>Spot Fleets</p></li>
<li><p>custom resources</p></li>
</ul>
<p>It's strongly recommended that you specify a value greater than 0. A
value greater than 0 means that data points are continuously reported to
CloudWatch that scaling policies can use to scale on a metric like
average CPU utilization.</p>
<p>For all other resources, the minimum allowed value depends on the
type of resource that you are using. If you provide a value that is
lower than what a resource can accept, an error occurs. In which case,
the error message will provide the minimum value that the resource can
accept.</p></td>
</tr>
<tr class="odd">
<td><code
id="applicationautoscaling_register_scalable_target_:_MaxCapacity">MaxCapacity</code></td>
<td><p>The maximum value that you plan to scale out to. When a scaling
policy is in effect, Application Auto Scaling can scale out (expand) as
needed to the maximum capacity limit in response to changing demand.
This property is required when registering a new scalable target.</p>
<p>Although you can specify a large maximum capacity, note that service
quotas might impose lower limits. Each service has its own default
quotas for the maximum capacity of the resource. If you want to specify
a higher limit, you can request an increase. For more information,
consult the documentation for that service. For information about the
default quotas for each service, see <a
href="https://docs.aws.amazon.com/general/latest/gr/aws-service-information.html">Service
endpoints and quotas</a> in the <em>Amazon Web Services General
Reference</em>.</p></td>
</tr>
<tr class="even">
<td><code
id="applicationautoscaling_register_scalable_target_:_RoleARN">RoleARN</code></td>
<td><p>This parameter is required for services that do not support
service-linked roles (such as Amazon EMR), and it must specify the ARN
of an IAM role that allows Application Auto Scaling to modify the
scalable target on your behalf.</p>
<p>If the service supports service-linked roles, Application Auto
Scaling uses a service-linked role, which it creates if it does not yet
exist. For more information, see <a
href="https://docs.aws.amazon.com/autoscaling/application/userguide/security_iam_service-with-iam.html#security_iam_service-with-iam-roles">Application
Auto Scaling IAM roles</a>.</p></td>
</tr>
<tr class="odd">
<td><code
id="applicationautoscaling_register_scalable_target_:_SuspendedState">SuspendedState</code></td>
<td><p>An embedded object that contains attributes and attribute values
that are used to suspend and resume automatic scaling. Setting the value
of an attribute to <code>true</code> suspends the specified scaling
activities. Setting it to <code>false</code> (default) resumes the
specified scaling activities.</p>
<p><strong>Suspension Outcomes</strong></p>
<ul>
<li><p>For <code>DynamicScalingInSuspended</code>, while a suspension is
in effect, all scale-in activities that are triggered by a scaling
policy are suspended.</p></li>
<li><p>For <code>DynamicScalingOutSuspended</code>, while a suspension
is in effect, all scale-out activities that are triggered by a scaling
policy are suspended.</p></li>
<li><p>For <code>ScheduledScalingSuspended</code>, while a suspension is
in effect, all scaling activities that involve scheduled actions are
suspended.</p></li>
</ul>
<p>For more information, see <a
href="https://docs.aws.amazon.com/autoscaling/application/userguide/application-auto-scaling-suspend-resume-scaling.html">Suspending
and resuming scaling</a> in the <em>Application Auto Scaling User
Guide</em>.</p></td>
</tr>
<tr class="even">
<td><code
id="applicationautoscaling_register_scalable_target_:_Tags">Tags</code></td>
<td><p>Assigns one or more tags to the scalable target. Use this
parameter to tag the scalable target when it is created. To tag an
existing scalable target, use the <code>tag_resource</code>
operation.</p>
<p>Each tag consists of a tag key and a tag value. Both the tag key and
the tag value are required. You cannot have more than one tag on a
scalable target with the same tag key.</p>
<p>Use tags to control access to a scalable target. For more
information, see <a
href="https://docs.aws.amazon.com/autoscaling/application/userguide/resource-tagging-support.html">Tagging
support for Application Auto Scaling</a> in the <em>Application Auto
Scaling User Guide</em>.</p></td>
</tr>
</tbody>
</table>

### Value

A list with the following syntax:

    list(
      ScalableTargetARN = "string"
    )

### Request syntax

    svc$register_scalable_target(
      ServiceNamespace = "ecs"|"elasticmapreduce"|"ec2"|"appstream"|"dynamodb"|"rds"|"sagemaker"|"custom-resource"|"comprehend"|"lambda"|"cassandra"|"kafka"|"elasticache"|"neptune",
      ResourceId = "string",
      ScalableDimension = "ecs:service:DesiredCount"|"ec2:spot-fleet-request:TargetCapacity"|"elasticmapreduce:instancegroup:InstanceCount"|"appstream:fleet:DesiredCapacity"|"dynamodb:table:ReadCapacityUnits"|"dynamodb:table:WriteCapacityUnits"|"dynamodb:index:ReadCapacityUnits"|"dynamodb:index:WriteCapacityUnits"|"rds:cluster:ReadReplicaCount"|"sagemaker:variant:DesiredInstanceCount"|"custom-resource:ResourceType:Property"|"comprehend:document-classifier-endpoint:DesiredInferenceUnits"|"comprehend:entity-recognizer-endpoint:DesiredInferenceUnits"|"lambda:function:ProvisionedConcurrency"|"cassandra:table:ReadCapacityUnits"|"cassandra:table:WriteCapacityUnits"|"kafka:broker-storage:VolumeSize"|"elasticache:replication-group:NodeGroups"|"elasticache:replication-group:Replicas"|"neptune:cluster:ReadReplicaCount"|"sagemaker:variant:DesiredProvisionedConcurrency",
      MinCapacity = 123,
      MaxCapacity = 123,
      RoleARN = "string",
      SuspendedState = list(
        DynamicScalingInSuspended = TRUE|FALSE,
        DynamicScalingOutSuspended = TRUE|FALSE,
        ScheduledScalingSuspended = TRUE|FALSE
      ),
      Tags = list(
        "string"
      )
    )

### Examples

    ## Not run: 
    # This example registers a scalable target from an Amazon ECS service
    # called web-app that is running on the default cluster, with a minimum
    # desired count of 1 task and a maximum desired count of 10 tasks.
    svc$register_scalable_target(
      MaxCapacity = 10L,
      MinCapacity = 1L,
      ResourceId = "service/default/web-app",
      ScalableDimension = "ecs:service:DesiredCount",
      ServiceNamespace = "ecs"
    )

    ## End(Not run)