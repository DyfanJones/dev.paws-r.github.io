<table style="width: 100%;">
<tbody>
<tr class="odd">
<td>ecs_put_cluster_capacity_providers</td>
<td style="text-align: right;">R Documentation</td>
</tr>
</tbody>
</table>

## Modifies the available capacity providers and the default capacity provider strategy for a cluster

### Description

Modifies the available capacity providers and the default capacity
provider strategy for a cluster.

You must specify both the available capacity providers and a default
capacity provider strategy for the cluster. If the specified cluster has
existing capacity providers associated with it, you must specify all
existing capacity providers in addition to any new ones you want to add.
Any existing capacity providers that are associated with a cluster that
are omitted from a `put_cluster_capacity_providers` API call will be
disassociated with the cluster. You can only disassociate an existing
capacity provider from a cluster if it's not being used by any existing
tasks.

When creating a service or running a task on a cluster, if no capacity
provider or launch type is specified, then the cluster's default
capacity provider strategy is used. We recommend that you define a
default capacity provider strategy for your cluster. However, you must
specify an empty array (`⁠[]⁠`) to bypass defining a default strategy.

### Usage

    ecs_put_cluster_capacity_providers(cluster, capacityProviders,
      defaultCapacityProviderStrategy)

### Arguments

<table>
<colgroup>
<col style="width: 35%" />
<col style="width: 65%" />
</colgroup>
<tbody>
<tr class="odd">
<td><code
id="ecs_put_cluster_capacity_providers_:_cluster">cluster</code></td>
<td><p>[required] The short name or full Amazon Resource Name (ARN) of
the cluster to modify the capacity provider settings for. If you don't
specify a cluster, the default cluster is assumed.</p></td>
</tr>
<tr class="even">
<td><code
id="ecs_put_cluster_capacity_providers_:_capacityProviders">capacityProviders</code></td>
<td><p>[required] The name of one or more capacity providers to
associate with the cluster.</p>
<p>If specifying a capacity provider that uses an Auto Scaling group,
the capacity provider must already be created. New capacity providers
can be created with the <code>create_capacity_provider</code> API
operation.</p>
<p>To use a Fargate capacity provider, specify either the
<code>FARGATE</code> or <code>FARGATE_SPOT</code> capacity providers.
The Fargate capacity providers are available to all accounts and only
need to be associated with a cluster to be used.</p></td>
</tr>
<tr class="odd">
<td><code
id="ecs_put_cluster_capacity_providers_:_defaultCapacityProviderStrategy">defaultCapacityProviderStrategy</code></td>
<td><p>[required] The capacity provider strategy to use by default for
the cluster.</p>
<p>When creating a service or running a task on a cluster, if no
capacity provider or launch type is specified then the default capacity
provider strategy for the cluster is used.</p>
<p>A capacity provider strategy consists of one or more capacity
providers along with the <code>base</code> and <code>weight</code> to
assign to them. A capacity provider must be associated with the cluster
to be used in a capacity provider strategy. The
<code>put_cluster_capacity_providers</code> API is used to associate a
capacity provider with a cluster. Only capacity providers with an
<code>ACTIVE</code> or <code>UPDATING</code> status can be used.</p>
<p>If specifying a capacity provider that uses an Auto Scaling group,
the capacity provider must already be created. New capacity providers
can be created with the <code>create_capacity_provider</code> API
operation.</p>
<p>To use a Fargate capacity provider, specify either the
<code>FARGATE</code> or <code>FARGATE_SPOT</code> capacity providers.
The Fargate capacity providers are available to all accounts and only
need to be associated with a cluster to be used.</p></td>
</tr>
</tbody>
</table>

### Value

A list with the following syntax:

    list(
      cluster = list(
        clusterArn = "string",
        clusterName = "string",
        configuration = list(
          executeCommandConfiguration = list(
            kmsKeyId = "string",
            logging = "NONE"|"DEFAULT"|"OVERRIDE",
            logConfiguration = list(
              cloudWatchLogGroupName = "string",
              cloudWatchEncryptionEnabled = TRUE|FALSE,
              s3BucketName = "string",
              s3EncryptionEnabled = TRUE|FALSE,
              s3KeyPrefix = "string"
            )
          )
        ),
        status = "string",
        registeredContainerInstancesCount = 123,
        runningTasksCount = 123,
        pendingTasksCount = 123,
        activeServicesCount = 123,
        statistics = list(
          list(
            name = "string",
            value = "string"
          )
        ),
        tags = list(
          list(
            key = "string",
            value = "string"
          )
        ),
        settings = list(
          list(
            name = "containerInsights",
            value = "string"
          )
        ),
        capacityProviders = list(
          "string"
        ),
        defaultCapacityProviderStrategy = list(
          list(
            capacityProvider = "string",
            weight = 123,
            base = 123
          )
        ),
        attachments = list(
          list(
            id = "string",
            type = "string",
            status = "string",
            details = list(
              list(
                name = "string",
                value = "string"
              )
            )
          )
        ),
        attachmentsStatus = "string",
        serviceConnectDefaults = list(
          namespace = "string"
        )
      )
    )

### Request syntax

    svc$put_cluster_capacity_providers(
      cluster = "string",
      capacityProviders = list(
        "string"
      ),
      defaultCapacityProviderStrategy = list(
        list(
          capacityProvider = "string",
          weight = 123,
          base = 123
        )
      )
    )