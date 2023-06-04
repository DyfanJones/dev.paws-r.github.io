<table style="width: 100%;">
<tbody>
<tr class="odd">
<td>applicationautoscaling_describe_scheduled_actions</td>
<td style="text-align: right;">R Documentation</td>
</tr>
</tbody>
</table>

## Describes the Application Auto Scaling scheduled actions for the specified service namespace

### Description

Describes the Application Auto Scaling scheduled actions for the
specified service namespace.

You can filter the results using the `ResourceId`, `ScalableDimension`,
and `ScheduledActionNames` parameters.

For more information, see [Scheduled
scaling](https://docs.aws.amazon.com/autoscaling/application/userguide/application-auto-scaling-scheduled-scaling.html)
and [Managing scheduled
scaling](https://docs.aws.amazon.com/autoscaling/application/userguide/scheduled-scaling-additional-cli-commands.html)
in the *Application Auto Scaling User Guide*.

### Usage

    applicationautoscaling_describe_scheduled_actions(ScheduledActionNames,
      ServiceNamespace, ResourceId, ScalableDimension, MaxResults, NextToken)

### Arguments

<table>
<colgroup>
<col style="width: 35%" />
<col style="width: 65%" />
</colgroup>
<tbody>
<tr class="odd">
<td><code
id="applicationautoscaling_describe_scheduled_actions_:_ScheduledActionNames">ScheduledActionNames</code></td>
<td><p>The names of the scheduled actions to describe.</p></td>
</tr>
<tr class="even">
<td><code
id="applicationautoscaling_describe_scheduled_actions_:_ServiceNamespace">ServiceNamespace</code></td>
<td><p>[required] The namespace of the Amazon Web Services service that
provides the resource. For a resource provided by your own application
or service, use <code>custom-resource</code> instead.</p></td>
</tr>
<tr class="odd">
<td><code
id="applicationautoscaling_describe_scheduled_actions_:_ResourceId">ResourceId</code></td>
<td><p>The identifier of the resource associated with the scheduled
action. This string consists of the resource type and unique
identifier.</p>
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
<tr class="even">
<td><code
id="applicationautoscaling_describe_scheduled_actions_:_ScalableDimension">ScalableDimension</code></td>
<td><p>The scalable dimension. This string consists of the service
namespace, resource type, and scaling property. If you specify a
scalable dimension, you must also specify a resource ID.</p>
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
<tr class="odd">
<td><code
id="applicationautoscaling_describe_scheduled_actions_:_MaxResults">MaxResults</code></td>
<td><p>The maximum number of scheduled action results. This value can be
between 1 and 50. The default value is 50.</p>
<p>If this parameter is used, the operation returns up to
<code>MaxResults</code> results at a time, along with a
<code>NextToken</code> value. To get the next set of results, include
the <code>NextToken</code> value in a subsequent call. If this parameter
is not used, the operation returns up to 50 results and a
<code>NextToken</code> value, if applicable.</p></td>
</tr>
<tr class="even">
<td><code
id="applicationautoscaling_describe_scheduled_actions_:_NextToken">NextToken</code></td>
<td><p>The token for the next set of results.</p></td>
</tr>
</tbody>
</table>

### Value

A list with the following syntax:

    list(
      ScheduledActions = list(
        list(
          ScheduledActionName = "string",
          ScheduledActionARN = "string",
          ServiceNamespace = "ecs"|"elasticmapreduce"|"ec2"|"appstream"|"dynamodb"|"rds"|"sagemaker"|"custom-resource"|"comprehend"|"lambda"|"cassandra"|"kafka"|"elasticache"|"neptune",
          Schedule = "string",
          Timezone = "string",
          ResourceId = "string",
          ScalableDimension = "ecs:service:DesiredCount"|"ec2:spot-fleet-request:TargetCapacity"|"elasticmapreduce:instancegroup:InstanceCount"|"appstream:fleet:DesiredCapacity"|"dynamodb:table:ReadCapacityUnits"|"dynamodb:table:WriteCapacityUnits"|"dynamodb:index:ReadCapacityUnits"|"dynamodb:index:WriteCapacityUnits"|"rds:cluster:ReadReplicaCount"|"sagemaker:variant:DesiredInstanceCount"|"custom-resource:ResourceType:Property"|"comprehend:document-classifier-endpoint:DesiredInferenceUnits"|"comprehend:entity-recognizer-endpoint:DesiredInferenceUnits"|"lambda:function:ProvisionedConcurrency"|"cassandra:table:ReadCapacityUnits"|"cassandra:table:WriteCapacityUnits"|"kafka:broker-storage:VolumeSize"|"elasticache:replication-group:NodeGroups"|"elasticache:replication-group:Replicas"|"neptune:cluster:ReadReplicaCount"|"sagemaker:variant:DesiredProvisionedConcurrency",
          StartTime = as.POSIXct(
            "2015-01-01"
          ),
          EndTime = as.POSIXct(
            "2015-01-01"
          ),
          ScalableTargetAction = list(
            MinCapacity = 123,
            MaxCapacity = 123
          ),
          CreationTime = as.POSIXct(
            "2015-01-01"
          )
        )
      ),
      NextToken = "string"
    )

### Request syntax

    svc$describe_scheduled_actions(
      ScheduledActionNames = list(
        "string"
      ),
      ServiceNamespace = "ecs"|"elasticmapreduce"|"ec2"|"appstream"|"dynamodb"|"rds"|"sagemaker"|"custom-resource"|"comprehend"|"lambda"|"cassandra"|"kafka"|"elasticache"|"neptune",
      ResourceId = "string",
      ScalableDimension = "ecs:service:DesiredCount"|"ec2:spot-fleet-request:TargetCapacity"|"elasticmapreduce:instancegroup:InstanceCount"|"appstream:fleet:DesiredCapacity"|"dynamodb:table:ReadCapacityUnits"|"dynamodb:table:WriteCapacityUnits"|"dynamodb:index:ReadCapacityUnits"|"dynamodb:index:WriteCapacityUnits"|"rds:cluster:ReadReplicaCount"|"sagemaker:variant:DesiredInstanceCount"|"custom-resource:ResourceType:Property"|"comprehend:document-classifier-endpoint:DesiredInferenceUnits"|"comprehend:entity-recognizer-endpoint:DesiredInferenceUnits"|"lambda:function:ProvisionedConcurrency"|"cassandra:table:ReadCapacityUnits"|"cassandra:table:WriteCapacityUnits"|"kafka:broker-storage:VolumeSize"|"elasticache:replication-group:NodeGroups"|"elasticache:replication-group:Replicas"|"neptune:cluster:ReadReplicaCount"|"sagemaker:variant:DesiredProvisionedConcurrency",
      MaxResults = 123,
      NextToken = "string"
    )

### Examples

    ## Not run: 
    # This example describes the scheduled actions for the dynamodb service
    # namespace.
    svc$describe_scheduled_actions(
      ServiceNamespace = "dynamodb"
    )

    ## End(Not run)