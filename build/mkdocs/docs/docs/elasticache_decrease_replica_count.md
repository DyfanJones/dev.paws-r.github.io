<table style="width: 100%;">
<tbody>
<tr class="odd">
<td>elasticache_decrease_replica_count</td>
<td style="text-align: right;">R Documentation</td>
</tr>
</tbody>
</table>

## Dynamically decreases the number of replicas in a Redis (cluster mode disabled) replication group or the number of replica nodes in one or more node groups (shards) of a Redis (cluster mode enabled) replication group

### Description

Dynamically decreases the number of replicas in a Redis (cluster mode
disabled) replication group or the number of replica nodes in one or
more node groups (shards) of a Redis (cluster mode enabled) replication
group. This operation is performed with no cluster down time.

### Usage

    elasticache_decrease_replica_count(ReplicationGroupId, NewReplicaCount,
      ReplicaConfiguration, ReplicasToRemove, ApplyImmediately)

### Arguments

<table>
<colgroup>
<col style="width: 35%" />
<col style="width: 65%" />
</colgroup>
<tbody>
<tr class="odd">
<td><code
id="elasticache_decrease_replica_count_:_ReplicationGroupId">ReplicationGroupId</code></td>
<td><p>[required] The id of the replication group from which you want to
remove replica nodes.</p></td>
</tr>
<tr class="even">
<td><code
id="elasticache_decrease_replica_count_:_NewReplicaCount">NewReplicaCount</code></td>
<td><p>The number of read replica nodes you want at the completion of
this operation. For Redis (cluster mode disabled) replication groups,
this is the number of replica nodes in the replication group. For Redis
(cluster mode enabled) replication groups, this is the number of replica
nodes in each of the replication group's node groups.</p>
<p>The minimum number of replicas in a shard or replication group
is:</p>
<ul>
<li><p>Redis (cluster mode disabled)</p>
<ul>
<li><p>If Multi-AZ is enabled: 1</p></li>
<li><p>If Multi-AZ is not enabled: 0</p></li>
</ul></li>
<li><p>Redis (cluster mode enabled): 0 (though you will not be able to
failover to a replica if your primary node fails)</p></li>
</ul></td>
</tr>
<tr class="odd">
<td><code
id="elasticache_decrease_replica_count_:_ReplicaConfiguration">ReplicaConfiguration</code></td>
<td><p>A list of <code>ConfigureShard</code> objects that can be used to
configure each shard in a Redis (cluster mode enabled) replication
group. The <code>ConfigureShard</code> has three members:
<code>NewReplicaCount</code>, <code>NodeGroupId</code>, and
<code>PreferredAvailabilityZones</code>.</p></td>
</tr>
<tr class="even">
<td><code
id="elasticache_decrease_replica_count_:_ReplicasToRemove">ReplicasToRemove</code></td>
<td><p>A list of the node ids to remove from the replication group or
node group (shard).</p></td>
</tr>
<tr class="odd">
<td><code
id="elasticache_decrease_replica_count_:_ApplyImmediately">ApplyImmediately</code></td>
<td><p>[required] If <code>True</code>, the number of replica nodes is
decreased immediately. <code>ApplyImmediately=False</code> is not
currently supported.</p></td>
</tr>
</tbody>
</table>

### Value

A list with the following syntax:

    list(
      ReplicationGroup = list(
        ReplicationGroupId = "string",
        Description = "string",
        GlobalReplicationGroupInfo = list(
          GlobalReplicationGroupId = "string",
          GlobalReplicationGroupMemberRole = "string"
        ),
        Status = "string",
        PendingModifiedValues = list(
          PrimaryClusterId = "string",
          AutomaticFailoverStatus = "enabled"|"disabled",
          Resharding = list(
            SlotMigration = list(
              ProgressPercentage = 123.0
            )
          ),
          AuthTokenStatus = "SETTING"|"ROTATING",
          UserGroups = list(
            UserGroupIdsToAdd = list(
              "string"
            ),
            UserGroupIdsToRemove = list(
              "string"
            )
          ),
          LogDeliveryConfigurations = list(
            list(
              LogType = "slow-log"|"engine-log",
              DestinationType = "cloudwatch-logs"|"kinesis-firehose",
              DestinationDetails = list(
                CloudWatchLogsDetails = list(
                  LogGroup = "string"
                ),
                KinesisFirehoseDetails = list(
                  DeliveryStream = "string"
                )
              ),
              LogFormat = "text"|"json"
            )
          ),
          TransitEncryptionEnabled = TRUE|FALSE,
          TransitEncryptionMode = "preferred"|"required",
          ClusterMode = "enabled"|"disabled"|"compatible"
        ),
        MemberClusters = list(
          "string"
        ),
        NodeGroups = list(
          list(
            NodeGroupId = "string",
            Status = "string",
            PrimaryEndpoint = list(
              Address = "string",
              Port = 123
            ),
            ReaderEndpoint = list(
              Address = "string",
              Port = 123
            ),
            Slots = "string",
            NodeGroupMembers = list(
              list(
                CacheClusterId = "string",
                CacheNodeId = "string",
                ReadEndpoint = list(
                  Address = "string",
                  Port = 123
                ),
                PreferredAvailabilityZone = "string",
                PreferredOutpostArn = "string",
                CurrentRole = "string"
              )
            )
          )
        ),
        SnapshottingClusterId = "string",
        AutomaticFailover = "enabled"|"disabled"|"enabling"|"disabling",
        MultiAZ = "enabled"|"disabled",
        ConfigurationEndpoint = list(
          Address = "string",
          Port = 123
        ),
        SnapshotRetentionLimit = 123,
        SnapshotWindow = "string",
        ClusterEnabled = TRUE|FALSE,
        CacheNodeType = "string",
        AuthTokenEnabled = TRUE|FALSE,
        AuthTokenLastModifiedDate = as.POSIXct(
          "2015-01-01"
        ),
        TransitEncryptionEnabled = TRUE|FALSE,
        AtRestEncryptionEnabled = TRUE|FALSE,
        MemberClustersOutpostArns = list(
          "string"
        ),
        KmsKeyId = "string",
        ARN = "string",
        UserGroupIds = list(
          "string"
        ),
        LogDeliveryConfigurations = list(
          list(
            LogType = "slow-log"|"engine-log",
            DestinationType = "cloudwatch-logs"|"kinesis-firehose",
            DestinationDetails = list(
              CloudWatchLogsDetails = list(
                LogGroup = "string"
              ),
              KinesisFirehoseDetails = list(
                DeliveryStream = "string"
              )
            ),
            LogFormat = "text"|"json",
            Status = "active"|"enabling"|"modifying"|"disabling"|"error",
            Message = "string"
          )
        ),
        ReplicationGroupCreateTime = as.POSIXct(
          "2015-01-01"
        ),
        DataTiering = "enabled"|"disabled",
        AutoMinorVersionUpgrade = TRUE|FALSE,
        NetworkType = "ipv4"|"ipv6"|"dual_stack",
        IpDiscovery = "ipv4"|"ipv6",
        TransitEncryptionMode = "preferred"|"required",
        ClusterMode = "enabled"|"disabled"|"compatible"
      )
    )

### Request syntax

    svc$decrease_replica_count(
      ReplicationGroupId = "string",
      NewReplicaCount = 123,
      ReplicaConfiguration = list(
        list(
          NodeGroupId = "string",
          NewReplicaCount = 123,
          PreferredAvailabilityZones = list(
            "string"
          ),
          PreferredOutpostArns = list(
            "string"
          )
        )
      ),
      ReplicasToRemove = list(
        "string"
      ),
      ApplyImmediately = TRUE|FALSE
    )