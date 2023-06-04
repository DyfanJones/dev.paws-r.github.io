<table style="width: 100%;">
<tbody>
<tr class="odd">
<td>rds_modify_global_cluster</td>
<td style="text-align: right;">R Documentation</td>
</tr>
</tbody>
</table>

## Modify a setting for an Amazon Aurora global cluster

### Description

Modify a setting for an Amazon Aurora global cluster. You can change one
or more database configuration parameters by specifying these parameters
and the new values in the request. For more information on Amazon
Aurora, see [What is Amazon
Aurora?](https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/CHAP_AuroraOverview.html)
in the *Amazon Aurora User Guide*.

This action only applies to Aurora DB clusters.

### Usage

    rds_modify_global_cluster(GlobalClusterIdentifier,
      NewGlobalClusterIdentifier, DeletionProtection, EngineVersion,
      AllowMajorVersionUpgrade)

### Arguments

<table>
<colgroup>
<col style="width: 35%" />
<col style="width: 65%" />
</colgroup>
<tbody>
<tr class="odd">
<td><code
id="rds_modify_global_cluster_:_GlobalClusterIdentifier">GlobalClusterIdentifier</code></td>
<td><p>The DB cluster identifier for the global cluster being modified.
This parameter isn't case-sensitive.</p>
<p>Constraints:</p>
<ul>
<li><p>Must match the identifier of an existing global database
cluster.</p></li>
</ul></td>
</tr>
<tr class="even">
<td><code
id="rds_modify_global_cluster_:_NewGlobalClusterIdentifier">NewGlobalClusterIdentifier</code></td>
<td><p>The new cluster identifier for the global database cluster when
modifying a global database cluster. This value is stored as a lowercase
string.</p>
<p>Constraints:</p>
<ul>
<li><p>Must contain from 1 to 63 letters, numbers, or hyphens</p></li>
<li><p>The first character must be a letter</p></li>
<li><p>Can't end with a hyphen or contain two consecutive
hyphens</p></li>
</ul>
<p>Example: <code>my-cluster2</code></p></td>
</tr>
<tr class="odd">
<td><code
id="rds_modify_global_cluster_:_DeletionProtection">DeletionProtection</code></td>
<td><p>Indicates if the global database cluster has deletion protection
enabled. The global database cluster can't be deleted when deletion
protection is enabled.</p></td>
</tr>
<tr class="even">
<td><code
id="rds_modify_global_cluster_:_EngineVersion">EngineVersion</code></td>
<td><p>The version number of the database engine to which you want to
upgrade. Changing this parameter results in an outage. The change is
applied during the next maintenance window unless
<code>ApplyImmediately</code> is enabled.</p>
<p>To list all of the available engine versions for
<code>aurora-mysql</code> (for MySQL-based Aurora global databases), use
the following command:</p>
<p><code
style="white-space: pre;">⁠aws rds describe-db-engine-versions --engine aurora-mysql --query '*[]|[?SupportsGlobalDatabases == `true`].[EngineVersion]'⁠</code></p>
<p>To list all of the available engine versions for
<code>aurora-postgresql</code> (for PostgreSQL-based Aurora global
databases), use the following command:</p>
<p><code
style="white-space: pre;">⁠aws rds describe-db-engine-versions --engine aurora-postgresql --query '*[]|[?SupportsGlobalDatabases == `true`].[EngineVersion]'⁠</code></p></td>
</tr>
<tr class="odd">
<td><code
id="rds_modify_global_cluster_:_AllowMajorVersionUpgrade">AllowMajorVersionUpgrade</code></td>
<td><p>A value that indicates whether major version upgrades are
allowed.</p>
<p>Constraints: You must allow major version upgrades when specifying a
value for the <code>EngineVersion</code> parameter that is a different
major version than the DB cluster's current version.</p>
<p>If you upgrade the major version of a global database, the cluster
and DB instance parameter groups are set to the default parameter groups
for the new version. Apply any custom parameter groups after completing
the upgrade.</p></td>
</tr>
</tbody>
</table>

### Value

A list with the following syntax:

    list(
      GlobalCluster = list(
        GlobalClusterIdentifier = "string",
        GlobalClusterResourceId = "string",
        GlobalClusterArn = "string",
        Status = "string",
        Engine = "string",
        EngineVersion = "string",
        DatabaseName = "string",
        StorageEncrypted = TRUE|FALSE,
        DeletionProtection = TRUE|FALSE,
        GlobalClusterMembers = list(
          list(
            DBClusterArn = "string",
            Readers = list(
              "string"
            ),
            IsWriter = TRUE|FALSE,
            GlobalWriteForwardingStatus = "enabled"|"disabled"|"enabling"|"disabling"|"unknown"
          )
        ),
        FailoverState = list(
          Status = "pending"|"failing-over"|"cancelling",
          FromDbClusterArn = "string",
          ToDbClusterArn = "string"
        )
      )
    )

### Request syntax

    svc$modify_global_cluster(
      GlobalClusterIdentifier = "string",
      NewGlobalClusterIdentifier = "string",
      DeletionProtection = TRUE|FALSE,
      EngineVersion = "string",
      AllowMajorVersionUpgrade = TRUE|FALSE
    )