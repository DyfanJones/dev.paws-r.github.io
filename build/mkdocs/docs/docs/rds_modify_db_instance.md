<table style="width: 100%;">
<tbody>
<tr class="odd">
<td>rds_modify_db_instance</td>
<td style="text-align: right;">R Documentation</td>
</tr>
</tbody>
</table>

## Modifies settings for a DB instance

### Description

Modifies settings for a DB instance. You can change one or more database
configuration parameters by specifying these parameters and the new
values in the request. To learn what modifications you can make to your
DB instance, call `describe_valid_db_instance_modifications` before you
call `modify_db_instance`.

### Usage

    rds_modify_db_instance(DBInstanceIdentifier, AllocatedStorage,
      DBInstanceClass, DBSubnetGroupName, DBSecurityGroups,
      VpcSecurityGroupIds, ApplyImmediately, MasterUserPassword,
      DBParameterGroupName, BackupRetentionPeriod, PreferredBackupWindow,
      PreferredMaintenanceWindow, MultiAZ, EngineVersion,
      AllowMajorVersionUpgrade, AutoMinorVersionUpgrade, LicenseModel, Iops,
      OptionGroupName, NewDBInstanceIdentifier, StorageType, TdeCredentialArn,
      TdeCredentialPassword, CACertificateIdentifier, Domain,
      CopyTagsToSnapshot, MonitoringInterval, DBPortNumber,
      PubliclyAccessible, MonitoringRoleArn, DomainIAMRoleName, PromotionTier,
      EnableIAMDatabaseAuthentication, EnablePerformanceInsights,
      PerformanceInsightsKMSKeyId, PerformanceInsightsRetentionPeriod,
      CloudwatchLogsExportConfiguration, ProcessorFeatures,
      UseDefaultProcessorFeatures, DeletionProtection, MaxAllocatedStorage,
      CertificateRotationRestart, ReplicaMode, EnableCustomerOwnedIp,
      AwsBackupRecoveryPointArn, AutomationMode,
      ResumeFullAutomationModeMinutes, NetworkType, StorageThroughput,
      ManageMasterUserPassword, RotateMasterUserPassword,
      MasterUserSecretKmsKeyId)

### Arguments

<table>
<colgroup>
<col style="width: 35%" />
<col style="width: 65%" />
</colgroup>
<tbody>
<tr class="odd">
<td><code
id="rds_modify_db_instance_:_DBInstanceIdentifier">DBInstanceIdentifier</code></td>
<td><p>[required] The DB instance identifier. This value is stored as a
lowercase string.</p>
<p>Constraints:</p>
<ul>
<li><p>Must match the identifier of an existing DBInstance.</p></li>
</ul></td>
</tr>
<tr class="even">
<td><code
id="rds_modify_db_instance_:_AllocatedStorage">AllocatedStorage</code></td>
<td><p>The new amount of storage in gibibytes (GiB) to allocate for the
DB instance.</p>
<p>For MariaDB, MySQL, Oracle, and PostgreSQL, the value supplied must
be at least 10% greater than the current value. Values that are not at
least 10% greater than the existing value are rounded up so that they
are 10% greater than the current value.</p>
<p>For the valid values for allocated storage for each engine, see
<code>create_db_instance</code>.</p></td>
</tr>
<tr class="odd">
<td><code
id="rds_modify_db_instance_:_DBInstanceClass">DBInstanceClass</code></td>
<td><p>The new compute and memory capacity of the DB instance, for
example db.m4.large. Not all DB instance classes are available in all
Amazon Web Services Regions, or for all database engines. For the full
list of DB instance classes, and availability for your engine, see <a
href="https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Concepts.DBInstanceClass.html">DB
Instance Class</a> in the <em>Amazon RDS User Guide</em> or <a
href="https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/Concepts.DBInstanceClass.html">Aurora
DB instance classes</a> in the <em>Amazon Aurora User Guide</em>. For
RDS Custom, see <a
href="https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/custom-reqs-limits.html#custom-reqs-limits.instances">DB
instance class support for RDS Custom for Oracle</a> and <a
href="https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/custom-reqs-limits-MS.html#custom-reqs-limits.instancesMS">DB
instance class support for RDS Custom for SQL Server</a>.</p>
<p>If you modify the DB instance class, an outage occurs during the
change. The change is applied during the next maintenance window, unless
you specify <code>ApplyImmediately</code> in your request.</p>
<p>Default: Uses existing setting</p></td>
</tr>
<tr class="even">
<td><code
id="rds_modify_db_instance_:_DBSubnetGroupName">DBSubnetGroupName</code></td>
<td><p>The new DB subnet group for the DB instance. You can use this
parameter to move your DB instance to a different VPC. If your DB
instance isn't in a VPC, you can also use this parameter to move your DB
instance into a VPC. For more information, see <a
href="https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_VPC.WorkingWithRDSInstanceinaVPC.html#USER_VPC.Non-VPC2VPC">Working
with a DB instance in a VPC</a> in the <em>Amazon RDS User
Guide</em>.</p>
<p>Changing the subnet group causes an outage during the change. The
change is applied during the next maintenance window, unless you enable
<code>ApplyImmediately</code>.</p>
<p>This parameter doesn't apply to RDS Custom.</p>
<p>Constraints: If supplied, must match the name of an existing
DBSubnetGroup.</p>
<p>Example: <code>mydbsubnetgroup</code></p></td>
</tr>
<tr class="odd">
<td><code
id="rds_modify_db_instance_:_DBSecurityGroups">DBSecurityGroups</code></td>
<td><p>A list of DB security groups to authorize on this DB instance.
Changing this setting doesn't result in an outage and the change is
asynchronously applied as soon as possible.</p>
<p>This setting doesn't apply to RDS Custom.</p>
<p>Constraints:</p>
<ul>
<li><p>If supplied, must match existing DBSecurityGroups.</p></li>
</ul></td>
</tr>
<tr class="even">
<td><code
id="rds_modify_db_instance_:_VpcSecurityGroupIds">VpcSecurityGroupIds</code></td>
<td><p>A list of Amazon EC2 VPC security groups to authorize on this DB
instance. This change is asynchronously applied as soon as possible.</p>
<p>This setting doesn't apply to RDS Custom.</p>
<p><strong>Amazon Aurora</strong></p>
<p>Not applicable. The associated list of EC2 VPC security groups is
managed by the DB cluster. For more information, see
<code>modify_db_cluster</code>.</p>
<p>Constraints:</p>
<ul>
<li><p>If supplied, must match existing VpcSecurityGroupIds.</p></li>
</ul></td>
</tr>
<tr class="odd">
<td><code
id="rds_modify_db_instance_:_ApplyImmediately">ApplyImmediately</code></td>
<td><p>A value that indicates whether the modifications in this request
and any pending modifications are asynchronously applied as soon as
possible, regardless of the <code>PreferredMaintenanceWindow</code>
setting for the DB instance. By default, this parameter is disabled.</p>
<p>If this parameter is disabled, changes to the DB instance are applied
during the next maintenance window. Some parameter changes can cause an
outage and are applied on the next call to
<code>reboot_db_instance</code>, or the next failure reboot. Review the
table of parameters in <a
href="https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Overview.DBInstance.Modifying.html">Modifying
a DB Instance</a> in the <em>Amazon RDS User Guide</em> to see the
impact of enabling or disabling <code>ApplyImmediately</code> for each
modified parameter and to determine when the changes are
applied.</p></td>
</tr>
<tr class="even">
<td><code
id="rds_modify_db_instance_:_MasterUserPassword">MasterUserPassword</code></td>
<td><p>The new password for the master user. The password can include
any printable ASCII character except "/", """, or "@".</p>
<p>Changing this parameter doesn't result in an outage and the change is
asynchronously applied as soon as possible. Between the time of the
request and the completion of the request, the
<code>MasterUserPassword</code> element exists in the
<code>PendingModifiedValues</code> element of the operation
response.</p>
<p>This setting doesn't apply to RDS Custom.</p>
<p><strong>Amazon Aurora</strong></p>
<p>Not applicable. The password for the master user is managed by the DB
cluster. For more information, see <code>modify_db_cluster</code>.</p>
<p>Default: Uses existing setting</p>
<p>Constraints: Can't be specified if
<code>ManageMasterUserPassword</code> is turned on.</p>
<p><strong>MariaDB</strong></p>
<p>Constraints: Must contain from 8 to 41 characters.</p>
<p><strong>Microsoft SQL Server</strong></p>
<p>Constraints: Must contain from 8 to 128 characters.</p>
<p><strong>MySQL</strong></p>
<p>Constraints: Must contain from 8 to 41 characters.</p>
<p><strong>Oracle</strong></p>
<p>Constraints: Must contain from 8 to 30 characters.</p>
<p><strong>PostgreSQL</strong></p>
<p>Constraints: Must contain from 8 to 128 characters.</p>
<p>Amazon RDS API operations never return the password, so this action
provides a way to regain access to a primary instance user if the
password is lost. This includes restoring privileges that might have
been accidentally revoked.</p></td>
</tr>
<tr class="odd">
<td><code
id="rds_modify_db_instance_:_DBParameterGroupName">DBParameterGroupName</code></td>
<td><p>The name of the DB parameter group to apply to the DB
instance.</p>
<p>Changing this setting doesn't result in an outage. The parameter
group name itself is changed immediately, but the actual parameter
changes are not applied until you reboot the instance without failover.
In this case, the DB instance isn't rebooted automatically, and the
parameter changes aren't applied during the next maintenance window.
However, if you modify dynamic parameters in the newly associated DB
parameter group, these changes are applied immediately without a
reboot.</p>
<p>This setting doesn't apply to RDS Custom.</p>
<p>Default: Uses existing setting</p>
<p>Constraints: The DB parameter group must be in the same DB parameter
group family as the DB instance.</p></td>
</tr>
<tr class="even">
<td><code
id="rds_modify_db_instance_:_BackupRetentionPeriod">BackupRetentionPeriod</code></td>
<td><p>The number of days to retain automated backups. Setting this
parameter to a positive number enables backups. Setting this parameter
to 0 disables automated backups.</p>
<p>Enabling and disabling backups can result in a brief I/O suspension
that lasts from a few seconds to a few minutes, depending on the size
and class of your DB instance.</p>
<p>These changes are applied during the next maintenance window unless
the <code>ApplyImmediately</code> parameter is enabled for this request.
If you change the parameter from one non-zero value to another non-zero
value, the change is asynchronously applied as soon as possible.</p>
<p><strong>Amazon Aurora</strong></p>
<p>Not applicable. The retention period for automated backups is managed
by the DB cluster. For more information, see
<code>modify_db_cluster</code>.</p>
<p>Default: Uses existing setting</p>
<p>Constraints:</p>
<ul>
<li><p>It must be a value from 0 to 35. It can't be set to 0 if the DB
instance is a source to read replicas. It can't be set to 0 for an RDS
Custom for Oracle DB instance.</p></li>
<li><p>It can be specified for a MySQL read replica only if the source
is running MySQL 5.6 or later.</p></li>
<li><p>It can be specified for a PostgreSQL read replica only if the
source is running PostgreSQL 9.3.5.</p></li>
</ul></td>
</tr>
<tr class="odd">
<td><code
id="rds_modify_db_instance_:_PreferredBackupWindow">PreferredBackupWindow</code></td>
<td><p>The daily time range during which automated backups are created
if automated backups are enabled, as determined by the
<code>BackupRetentionPeriod</code> parameter. Changing this parameter
doesn't result in an outage and the change is asynchronously applied as
soon as possible. The default is a 30-minute window selected at random
from an 8-hour block of time for each Amazon Web Services Region. For
more information, see <a
href="https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_WorkingWithAutomatedBackups.html#USER_WorkingWithAutomatedBackups.BackupWindow">Backup
window</a> in the <em>Amazon RDS User Guide.</em></p>
<p><strong>Amazon Aurora</strong></p>
<p>Not applicable. The daily time range for creating automated backups
is managed by the DB cluster. For more information, see
<code>modify_db_cluster</code>.</p>
<p>Constraints:</p>
<ul>
<li><p>Must be in the format hh24:mi-hh24:mi</p></li>
<li><p>Must be in Universal Time Coordinated (UTC)</p></li>
<li><p>Must not conflict with the preferred maintenance window</p></li>
<li><p>Must be at least 30 minutes</p></li>
</ul></td>
</tr>
<tr class="even">
<td><code
id="rds_modify_db_instance_:_PreferredMaintenanceWindow">PreferredMaintenanceWindow</code></td>
<td><p>The weekly time range (in UTC) during which system maintenance
can occur, which might result in an outage. Changing this parameter
doesn't result in an outage, except in the following situation, and the
change is asynchronously applied as soon as possible. If there are
pending actions that cause a reboot, and the maintenance window is
changed to include the current time, then changing this parameter will
cause a reboot of the DB instance. If moving this window to the current
time, there must be at least 30 minutes between the current time and end
of the window to ensure pending changes are applied.</p>
<p>For more information, see <a
href="https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_UpgradeDBInstance.Maintenance.html#Concepts.DBMaintenance">Amazon
RDS Maintenance Window</a> in the <em>Amazon RDS User Guide.</em></p>
<p>Default: Uses existing setting</p>
<p>Format: ddd:hh24:mi-ddd:hh24:mi</p>
<p>Valid Days: Mon | Tue | Wed | Thu | Fri | Sat | Sun</p>
<p>Constraints: Must be at least 30 minutes</p></td>
</tr>
<tr class="odd">
<td><code id="rds_modify_db_instance_:_MultiAZ">MultiAZ</code></td>
<td><p>A value that indicates whether the DB instance is a Multi-AZ
deployment. Changing this parameter doesn't result in an outage. The
change is applied during the next maintenance window unless the
<code>ApplyImmediately</code> parameter is enabled for this request.</p>
<p>This setting doesn't apply to RDS Custom.</p></td>
</tr>
<tr class="even">
<td><code
id="rds_modify_db_instance_:_EngineVersion">EngineVersion</code></td>
<td><p>The version number of the database engine to upgrade to. Changing
this parameter results in an outage and the change is applied during the
next maintenance window unless the <code>ApplyImmediately</code>
parameter is enabled for this request.</p>
<p>For major version upgrades, if a nondefault DB parameter group is
currently in use, a new DB parameter group in the DB parameter group
family for the new engine version must be specified. The new DB
parameter group can be the default for that DB parameter group
family.</p>
<p>If you specify only a major version, Amazon RDS will update the DB
instance to the default minor version if the current minor version is
lower. For information about valid engine versions, see
<code>create_db_instance</code>, or call
<code>describe_db_engine_versions</code>.</p>
<p>If the instance that you're modifying is acting as a read replica,
the engine version that you specify must be the same or later than the
version that the source DB instance or cluster is running.</p>
<p>In RDS Custom for Oracle, this parameter is supported for read
replicas only if they are in the <code>PATCH_DB_FAILURE</code>
lifecycle.</p></td>
</tr>
<tr class="odd">
<td><code
id="rds_modify_db_instance_:_AllowMajorVersionUpgrade">AllowMajorVersionUpgrade</code></td>
<td><p>A value that indicates whether major version upgrades are
allowed. Changing this parameter doesn't result in an outage and the
change is asynchronously applied as soon as possible.</p>
<p>This setting doesn't apply to RDS Custom.</p>
<p>Constraints: Major version upgrades must be allowed when specifying a
value for the EngineVersion parameter that is a different major version
than the DB instance's current version.</p></td>
</tr>
<tr class="even">
<td><code
id="rds_modify_db_instance_:_AutoMinorVersionUpgrade">AutoMinorVersionUpgrade</code></td>
<td><p>A value that indicates whether minor version upgrades are applied
automatically to the DB instance during the maintenance window. An
outage occurs when all the following conditions are met:</p>
<ul>
<li><p>The automatic upgrade is enabled for the maintenance
window.</p></li>
<li><p>A newer minor version is available.</p></li>
<li><p>RDS has enabled automatic patching for the engine
version.</p></li>
</ul>
<p>If any of the preceding conditions isn't met, RDS applies the change
as soon as possible and doesn't cause an outage.</p>
<p>For an RDS Custom DB instance, set
<code>AutoMinorVersionUpgrade</code> to <code>false</code>. Otherwise,
the operation returns an error.</p></td>
</tr>
<tr class="odd">
<td><code
id="rds_modify_db_instance_:_LicenseModel">LicenseModel</code></td>
<td><p>The license model for the DB instance.</p>
<p>This setting doesn't apply to RDS Custom.</p>
<p>Valid values: <code>license-included</code> |
<code>bring-your-own-license</code> |
<code>general-public-license</code></p></td>
</tr>
<tr class="even">
<td><code id="rds_modify_db_instance_:_Iops">Iops</code></td>
<td><p>The new Provisioned IOPS (I/O operations per second) value for
the RDS instance.</p>
<p>Changing this setting doesn't result in an outage and the change is
applied during the next maintenance window unless the
<code>ApplyImmediately</code> parameter is enabled for this request. If
you are migrating from Provisioned IOPS to standard storage, set this
value to 0. The DB instance will require a reboot for the change in
storage type to take effect.</p>
<p>If you choose to migrate your DB instance from using standard storage
to using Provisioned IOPS, or from using Provisioned IOPS to using
standard storage, the process can take time. The duration of the
migration depends on several factors such as database load, storage
size, storage type (standard or Provisioned IOPS), amount of IOPS
provisioned (if any), and the number of prior scale storage operations.
Typical migration times are under 24 hours, but the process can take up
to several days in some cases. During the migration, the DB instance is
available for use, but might experience performance degradation. While
the migration takes place, nightly backups for the instance are
suspended. No other Amazon RDS operations can take place for the
instance, including modifying the instance, rebooting the instance,
deleting the instance, creating a read replica for the instance, and
creating a DB snapshot of the instance.</p>
<p>Constraints: For MariaDB, MySQL, Oracle, and PostgreSQL, the value
supplied must be at least 10% greater than the current value. Values
that are not at least 10% greater than the existing value are rounded up
so that they are 10% greater than the current value.</p>
<p>Default: Uses existing setting</p></td>
</tr>
<tr class="odd">
<td><code
id="rds_modify_db_instance_:_OptionGroupName">OptionGroupName</code></td>
<td><p>A value that indicates the DB instance should be associated with
the specified option group.</p>
<p>Changing this parameter doesn't result in an outage, with one
exception. If the parameter change results in an option group that
enables OEM, it can cause a brief period, lasting less than a second,
during which new connections are rejected but existing connections
aren't interrupted.</p>
<p>The change is applied during the next maintenance window unless the
<code>ApplyImmediately</code> parameter is enabled for this request.</p>
<p>Permanent options, such as the TDE option for Oracle Advanced
Security TDE, can't be removed from an option group, and that option
group can't be removed from a DB instance after it is associated with a
DB instance.</p>
<p>This setting doesn't apply to RDS Custom.</p></td>
</tr>
<tr class="even">
<td><code
id="rds_modify_db_instance_:_NewDBInstanceIdentifier">NewDBInstanceIdentifier</code></td>
<td><p>The new DB instance identifier for the DB instance when renaming
a DB instance. When you change the DB instance identifier, an instance
reboot occurs immediately if you enable <code>ApplyImmediately</code>,
or will occur during the next maintenance window if you disable Apply
Immediately. This value is stored as a lowercase string.</p>
<p>This setting doesn't apply to RDS Custom.</p>
<p>Constraints:</p>
<ul>
<li><p>Must contain from 1 to 63 letters, numbers, or hyphens.</p></li>
<li><p>The first character must be a letter.</p></li>
<li><p>Can't end with a hyphen or contain two consecutive
hyphens.</p></li>
</ul>
<p>Example: <code>mydbinstance</code></p></td>
</tr>
<tr class="odd">
<td><code
id="rds_modify_db_instance_:_StorageType">StorageType</code></td>
<td><p>Specifies the storage type to be associated with the DB
instance.</p>
<p>If you specify Provisioned IOPS (<code>io1</code>), you must also
include a value for the <code>Iops</code> parameter.</p>
<p>If you choose to migrate your DB instance from using standard storage
to using Provisioned IOPS, or from using Provisioned IOPS to using
standard storage, the process can take time. The duration of the
migration depends on several factors such as database load, storage
size, storage type (standard or Provisioned IOPS), amount of IOPS
provisioned (if any), and the number of prior scale storage operations.
Typical migration times are under 24 hours, but the process can take up
to several days in some cases. During the migration, the DB instance is
available for use, but might experience performance degradation. While
the migration takes place, nightly backups for the instance are
suspended. No other Amazon RDS operations can take place for the
instance, including modifying the instance, rebooting the instance,
deleting the instance, creating a read replica for the instance, and
creating a DB snapshot of the instance.</p>
<p>Valid values: <code>gp2 | gp3 | io1 | standard</code></p>
<p>Default: <code>io1</code> if the <code>Iops</code> parameter is
specified, otherwise <code>gp2</code></p></td>
</tr>
<tr class="even">
<td><code
id="rds_modify_db_instance_:_TdeCredentialArn">TdeCredentialArn</code></td>
<td><p>The ARN from the key store with which to associate the instance
for TDE encryption.</p>
<p>This setting doesn't apply to RDS Custom.</p></td>
</tr>
<tr class="odd">
<td><code
id="rds_modify_db_instance_:_TdeCredentialPassword">TdeCredentialPassword</code></td>
<td><p>The password for the given ARN from the key store in order to
access the device.</p>
<p>This setting doesn't apply to RDS Custom.</p></td>
</tr>
<tr class="even">
<td><code
id="rds_modify_db_instance_:_CACertificateIdentifier">CACertificateIdentifier</code></td>
<td><p>Specifies the CA certificate identifier to use for the DB
instance’s server certificate.</p>
<p>This setting doesn't apply to RDS Custom.</p>
<p>For more information, see <a
href="https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/UsingWithRDS.SSL.html">Using
SSL/TLS to encrypt a connection to a DB instance</a> in the <em>Amazon
RDS User Guide</em> and <a
href="https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/UsingWithRDS.SSL.html">Using
SSL/TLS to encrypt a connection to a DB cluster</a> in the <em>Amazon
Aurora User Guide</em>.</p></td>
</tr>
<tr class="odd">
<td><code id="rds_modify_db_instance_:_Domain">Domain</code></td>
<td><p>The Active Directory directory ID to move the DB instance to.
Specify <code>none</code> to remove the instance from its current
domain. You must create the domain before this operation. Currently, you
can create only MySQL, Microsoft SQL Server, Oracle, and PostgreSQL DB
instances in an Active Directory Domain.</p>
<p>For more information, see <a
href="https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/database-authentication.html">Kerberos
Authentication</a> in the <em>Amazon RDS User Guide</em>.</p>
<p>This setting doesn't apply to RDS Custom.</p></td>
</tr>
<tr class="even">
<td><code
id="rds_modify_db_instance_:_CopyTagsToSnapshot">CopyTagsToSnapshot</code></td>
<td><p>A value that indicates whether to copy all tags from the DB
instance to snapshots of the DB instance. By default, tags are not
copied.</p>
<p><strong>Amazon Aurora</strong></p>
<p>Not applicable. Copying tags to snapshots is managed by the DB
cluster. Setting this value for an Aurora DB instance has no effect on
the DB cluster setting. For more information, see
<code>modify_db_cluster</code>.</p></td>
</tr>
<tr class="odd">
<td><code
id="rds_modify_db_instance_:_MonitoringInterval">MonitoringInterval</code></td>
<td><p>The interval, in seconds, between points when Enhanced Monitoring
metrics are collected for the DB instance. To disable collecting
Enhanced Monitoring metrics, specify 0, which is the default.</p>
<p>If <code>MonitoringRoleArn</code> is specified, set
<code>MonitoringInterval</code> to a value other than 0.</p>
<p>This setting doesn't apply to RDS Custom.</p>
<p>Valid Values: <code
style="white-space: pre;">⁠0, 1, 5, 10, 15, 30, 60⁠</code></p></td>
</tr>
<tr class="even">
<td><code
id="rds_modify_db_instance_:_DBPortNumber">DBPortNumber</code></td>
<td><p>The port number on which the database accepts connections.</p>
<p>The value of the <code>DBPortNumber</code> parameter must not match
any of the port values specified for options in the option group for the
DB instance.</p>
<p>If you change the <code>DBPortNumber</code> value, your database
restarts regardless of the value of the <code>ApplyImmediately</code>
parameter.</p>
<p>This setting doesn't apply to RDS Custom.</p>
<p><strong>MySQL</strong></p>
<p>Default: <code>3306</code></p>
<p>Valid values: <code>1150-65535</code></p>
<p><strong>MariaDB</strong></p>
<p>Default: <code>3306</code></p>
<p>Valid values: <code>1150-65535</code></p>
<p><strong>PostgreSQL</strong></p>
<p>Default: <code>5432</code></p>
<p>Valid values: <code>1150-65535</code></p>
<p>Type: Integer</p>
<p><strong>Oracle</strong></p>
<p>Default: <code>1521</code></p>
<p>Valid values: <code>1150-65535</code></p>
<p><strong>SQL Server</strong></p>
<p>Default: <code>1433</code></p>
<p>Valid values: <code>1150-65535</code> except <code>1234</code>,
<code>1434</code>, <code>3260</code>, <code>3343</code>,
<code>3389</code>, <code>47001</code>, and <code>49152-49156</code>.</p>
<p><strong>Amazon Aurora</strong></p>
<p>Default: <code>3306</code></p>
<p>Valid values: <code>1150-65535</code></p></td>
</tr>
<tr class="odd">
<td><code
id="rds_modify_db_instance_:_PubliclyAccessible">PubliclyAccessible</code></td>
<td><p>A value that indicates whether the DB instance is publicly
accessible.</p>
<p>When the DB cluster is publicly accessible, its Domain Name System
(DNS) endpoint resolves to the private IP address from within the DB
cluster's virtual private cloud (VPC). It resolves to the public IP
address from outside of the DB cluster's VPC. Access to the DB cluster
is ultimately controlled by the security group it uses. That public
access isn't permitted if the security group assigned to the DB cluster
doesn't permit it.</p>
<p>When the DB instance isn't publicly accessible, it is an internal DB
instance with a DNS name that resolves to a private IP address.</p>
<p><code>PubliclyAccessible</code> only applies to DB instances in a
VPC. The DB instance must be part of a public subnet and
<code>PubliclyAccessible</code> must be enabled for it to be publicly
accessible.</p>
<p>Changes to the <code>PubliclyAccessible</code> parameter are applied
immediately regardless of the value of the <code>ApplyImmediately</code>
parameter.</p></td>
</tr>
<tr class="even">
<td><code
id="rds_modify_db_instance_:_MonitoringRoleArn">MonitoringRoleArn</code></td>
<td><p>The ARN for the IAM role that permits RDS to send enhanced
monitoring metrics to Amazon CloudWatch Logs. For example,
<code>arn:aws:iam:123456789012:role/emaccess</code>. For information on
creating a monitoring role, see <a
href="https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_Monitoring.html#USER_Monitoring.OS.IAMRole">To
create an IAM role for Amazon RDS Enhanced Monitoring</a> in the
<em>Amazon RDS User Guide.</em></p>
<p>If <code>MonitoringInterval</code> is set to a value other than 0,
supply a <code>MonitoringRoleArn</code> value.</p>
<p>This setting doesn't apply to RDS Custom.</p></td>
</tr>
<tr class="odd">
<td><code
id="rds_modify_db_instance_:_DomainIAMRoleName">DomainIAMRoleName</code></td>
<td><p>The name of the IAM role to use when making API calls to the
Directory Service.</p>
<p>This setting doesn't apply to RDS Custom.</p></td>
</tr>
<tr class="even">
<td><code
id="rds_modify_db_instance_:_PromotionTier">PromotionTier</code></td>
<td><p>A value that specifies the order in which an Aurora Replica is
promoted to the primary instance after a failure of the existing primary
instance. For more information, see <a
href="https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/Aurora.Managing.Backups.html#Aurora.Managing.FaultTolerance">Fault
Tolerance for an Aurora DB Cluster</a> in the <em>Amazon Aurora User
Guide</em>.</p>
<p>This setting doesn't apply to RDS Custom.</p>
<p>Default: 1</p>
<p>Valid Values: 0 - 15</p></td>
</tr>
<tr class="odd">
<td><code
id="rds_modify_db_instance_:_EnableIAMDatabaseAuthentication">EnableIAMDatabaseAuthentication</code></td>
<td><p>A value that indicates whether to enable mapping of Amazon Web
Services Identity and Access Management (IAM) accounts to database
accounts. By default, mapping isn't enabled.</p>
<p>This setting doesn't apply to Amazon Aurora. Mapping Amazon Web
Services IAM accounts to database accounts is managed by the DB
cluster.</p>
<p>For more information about IAM database authentication, see <a
href="https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/UsingWithRDS.IAMDBAuth.html">IAM
Database Authentication for MySQL and PostgreSQL</a> in the <em>Amazon
RDS User Guide.</em></p>
<p>This setting doesn't apply to RDS Custom.</p></td>
</tr>
<tr class="even">
<td><code
id="rds_modify_db_instance_:_EnablePerformanceInsights">EnablePerformanceInsights</code></td>
<td><p>A value that indicates whether to enable Performance Insights for
the DB instance.</p>
<p>For more information, see <a
href="https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_PerfInsights.html">Using
Amazon Performance Insights</a> in the <em>Amazon RDS User
Guide</em>.</p>
<p>This setting doesn't apply to RDS Custom.</p></td>
</tr>
<tr class="odd">
<td><code
id="rds_modify_db_instance_:_PerformanceInsightsKMSKeyId">PerformanceInsightsKMSKeyId</code></td>
<td><p>The Amazon Web Services KMS key identifier for encryption of
Performance Insights data.</p>
<p>The Amazon Web Services KMS key identifier is the key ARN, key ID,
alias ARN, or alias name for the KMS key.</p>
<p>If you do not specify a value for
<code>PerformanceInsightsKMSKeyId</code>, then Amazon RDS uses your
default KMS key. There is a default KMS key for your Amazon Web Services
account. Your Amazon Web Services account has a different default KMS
key for each Amazon Web Services Region.</p>
<p>This setting doesn't apply to RDS Custom.</p></td>
</tr>
<tr class="even">
<td><code
id="rds_modify_db_instance_:_PerformanceInsightsRetentionPeriod">PerformanceInsightsRetentionPeriod</code></td>
<td><p>The number of days to retain Performance Insights data. The
default is 7 days. The following values are valid:</p>
<ul>
<li><p>7</p></li>
<li><p><em>month</em> * 31, where <em>month</em> is a number of months
from 1-23</p></li>
<li><p>731</p></li>
</ul>
<p>For example, the following values are valid:</p>
<ul>
<li><p>93 (3 months * 31)</p></li>
<li><p>341 (11 months * 31)</p></li>
<li><p>589 (19 months * 31)</p></li>
<li><p>731</p></li>
</ul>
<p>If you specify a retention period such as 94, which isn't a valid
value, RDS issues an error.</p>
<p>This setting doesn't apply to RDS Custom.</p></td>
</tr>
<tr class="odd">
<td><code
id="rds_modify_db_instance_:_CloudwatchLogsExportConfiguration">CloudwatchLogsExportConfiguration</code></td>
<td><p>The configuration setting for the log types to be enabled for
export to CloudWatch Logs for a specific DB instance.</p>
<p>A change to the <code>CloudwatchLogsExportConfiguration</code>
parameter is always applied to the DB instance immediately. Therefore,
the <code>ApplyImmediately</code> parameter has no effect.</p>
<p>This setting doesn't apply to RDS Custom.</p></td>
</tr>
<tr class="even">
<td><code
id="rds_modify_db_instance_:_ProcessorFeatures">ProcessorFeatures</code></td>
<td><p>The number of CPU cores and the number of threads per core for
the DB instance class of the DB instance.</p>
<p>This setting doesn't apply to RDS Custom.</p></td>
</tr>
<tr class="odd">
<td><code
id="rds_modify_db_instance_:_UseDefaultProcessorFeatures">UseDefaultProcessorFeatures</code></td>
<td><p>A value that indicates whether the DB instance class of the DB
instance uses its default processor features.</p>
<p>This setting doesn't apply to RDS Custom.</p></td>
</tr>
<tr class="even">
<td><code
id="rds_modify_db_instance_:_DeletionProtection">DeletionProtection</code></td>
<td><p>A value that indicates whether the DB instance has deletion
protection enabled. The database can't be deleted when deletion
protection is enabled. By default, deletion protection isn't enabled.
For more information, see <a
href="https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_DeleteInstance.html">Deleting
a DB Instance</a>.</p></td>
</tr>
<tr class="odd">
<td><code
id="rds_modify_db_instance_:_MaxAllocatedStorage">MaxAllocatedStorage</code></td>
<td><p>The upper limit in gibibytes (GiB) to which Amazon RDS can
automatically scale the storage of the DB instance.</p>
<p>For more information about this setting, including limitations that
apply to it, see <a
href="https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_PIOPS.StorageTypes.html#USER_PIOPS.Autoscaling">Managing
capacity automatically with Amazon RDS storage autoscaling</a> in the
<em>Amazon RDS User Guide</em>.</p>
<p>This setting doesn't apply to RDS Custom.</p></td>
</tr>
<tr class="even">
<td><code
id="rds_modify_db_instance_:_CertificateRotationRestart">CertificateRotationRestart</code></td>
<td><p>A value that indicates whether the DB instance is restarted when
you rotate your SSL/TLS certificate.</p>
<p>By default, the DB instance is restarted when you rotate your SSL/TLS
certificate. The certificate is not updated until the DB instance is
restarted.</p>
<p>Set this parameter only if you are <em>not</em> using SSL/TLS to
connect to the DB instance.</p>
<p>If you are using SSL/TLS to connect to the DB instance, follow the
appropriate instructions for your DB engine to rotate your SSL/TLS
certificate:</p>
<ul>
<li><p>For more information about rotating your SSL/TLS certificate for
RDS DB engines, see <a
href="https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/UsingWithRDS.SSL-certificate-rotation.html">Rotating
Your SSL/TLS Certificate.</a> in the <em>Amazon RDS User
Guide.</em></p></li>
<li><p>For more information about rotating your SSL/TLS certificate for
Aurora DB engines, see <a
href="https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/UsingWithRDS.SSL-certificate-rotation.html">Rotating
Your SSL/TLS Certificate</a> in the <em>Amazon Aurora User
Guide</em>.</p></li>
</ul>
<p>This setting doesn't apply to RDS Custom.</p></td>
</tr>
<tr class="odd">
<td><code
id="rds_modify_db_instance_:_ReplicaMode">ReplicaMode</code></td>
<td><p>A value that sets the open mode of a replica database to either
mounted or read-only.</p>
<p>Currently, this parameter is only supported for Oracle DB
instances.</p>
<p>Mounted DB replicas are included in Oracle Enterprise Edition. The
main use case for mounted replicas is cross-Region disaster recovery.
The primary database doesn't use Active Data Guard to transmit
information to the mounted replica. Because it doesn't accept user
connections, a mounted replica can't serve a read-only workload. For
more information, see <a
href="https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/oracle-read-replicas.html">Working
with Oracle Read Replicas for Amazon RDS</a> in the <em>Amazon RDS User
Guide</em>.</p>
<p>This setting doesn't apply to RDS Custom.</p></td>
</tr>
<tr class="even">
<td><code
id="rds_modify_db_instance_:_EnableCustomerOwnedIp">EnableCustomerOwnedIp</code></td>
<td><p>A value that indicates whether to enable a customer-owned IP
address (CoIP) for an RDS on Outposts DB instance.</p>
<p>A <em>CoIP</em> provides local or external connectivity to resources
in your Outpost subnets through your on-premises network. For some use
cases, a CoIP can provide lower latency for connections to the DB
instance from outside of its virtual private cloud (VPC) on your local
network.</p>
<p>For more information about RDS on Outposts, see <a
href="https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/rds-on-outposts.html">Working
with Amazon RDS on Amazon Web Services Outposts</a> in the <em>Amazon
RDS User Guide</em>.</p>
<p>For more information about CoIPs, see <a
href="https://docs.aws.amazon.com/outposts/latest/userguide/routing.html#ip-addressing">Customer-owned
IP addresses</a> in the <em>Amazon Web Services Outposts User
Guide</em>.</p></td>
</tr>
<tr class="odd">
<td><code
id="rds_modify_db_instance_:_AwsBackupRecoveryPointArn">AwsBackupRecoveryPointArn</code></td>
<td><p>The Amazon Resource Name (ARN) of the recovery point in Amazon
Web Services Backup.</p>
<p>This setting doesn't apply to RDS Custom.</p></td>
</tr>
<tr class="even">
<td><code
id="rds_modify_db_instance_:_AutomationMode">AutomationMode</code></td>
<td><p>The automation mode of the RDS Custom DB instance:
<code>full</code> or <code style="white-space: pre;">⁠all paused⁠</code>.
If <code>full</code>, the DB instance automates monitoring and instance
recovery. If <code style="white-space: pre;">⁠all paused⁠</code>, the
instance pauses automation for the duration set by
<code>ResumeFullAutomationModeMinutes</code>.</p></td>
</tr>
<tr class="odd">
<td><code
id="rds_modify_db_instance_:_ResumeFullAutomationModeMinutes">ResumeFullAutomationModeMinutes</code></td>
<td><p>The number of minutes to pause the automation. When the time
period ends, RDS Custom resumes full automation. The minimum value is
<code>60</code> (default). The maximum value is <code
style="white-space: pre;">⁠1,440⁠</code>.</p></td>
</tr>
<tr class="even">
<td><code
id="rds_modify_db_instance_:_NetworkType">NetworkType</code></td>
<td><p>The network type of the DB instance.</p>
<p>Valid values:</p>
<ul>
<li><p><code>IPV4</code></p></li>
<li><p><code>DUAL</code></p></li>
</ul>
<p>The network type is determined by the <code>DBSubnetGroup</code>
specified for the DB instance. A <code>DBSubnetGroup</code> can support
only the IPv4 protocol or the IPv4 and the IPv6 protocols
(<code>DUAL</code>).</p>
<p>For more information, see <a
href="https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_VPC.WorkingWithRDSInstanceinaVPC.html">Working
with a DB instance in a VPC</a> in the <em>Amazon RDS User
Guide.</em></p></td>
</tr>
<tr class="odd">
<td><code
id="rds_modify_db_instance_:_StorageThroughput">StorageThroughput</code></td>
<td><p>Specifies the storage throughput value for the DB instance.</p>
<p>This setting applies only to the <code>gp3</code> storage type.</p>
<p>This setting doesn't apply to RDS Custom or Amazon Aurora.</p></td>
</tr>
<tr class="even">
<td><code
id="rds_modify_db_instance_:_ManageMasterUserPassword">ManageMasterUserPassword</code></td>
<td><p>A value that indicates whether to manage the master user password
with Amazon Web Services Secrets Manager.</p>
<p>If the DB instance doesn't manage the master user password with
Amazon Web Services Secrets Manager, you can turn on this management. In
this case, you can't specify <code>MasterUserPassword</code>.</p>
<p>If the DB instance already manages the master user password with
Amazon Web Services Secrets Manager, and you specify that the master
user password is not managed with Amazon Web Services Secrets Manager,
then you must specify <code>MasterUserPassword</code>. In this case, RDS
deletes the secret and uses the new password for the master user
specified by <code>MasterUserPassword</code>.</p>
<p>For more information, see <a
href="https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/rds-secrets-manager.html">Password
management with Amazon Web Services Secrets Manager</a> in the
<em>Amazon RDS User Guide.</em></p>
<p>Constraints:</p>
<ul>
<li><p>Can't manage the master user password with Amazon Web Services
Secrets Manager if <code>MasterUserPassword</code> is
specified.</p></li>
</ul></td>
</tr>
<tr class="odd">
<td><code
id="rds_modify_db_instance_:_RotateMasterUserPassword">RotateMasterUserPassword</code></td>
<td><p>A value that indicates whether to rotate the secret managed by
Amazon Web Services Secrets Manager for the master user password.</p>
<p>This setting is valid only if the master user password is managed by
RDS in Amazon Web Services Secrets Manager for the DB cluster. The
secret value contains the updated password.</p>
<p>For more information, see <a
href="https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/rds-secrets-manager.html">Password
management with Amazon Web Services Secrets Manager</a> in the
<em>Amazon RDS User Guide.</em></p>
<p>Constraints:</p>
<ul>
<li><p>You must apply the change immediately when rotating the master
user password.</p></li>
</ul></td>
</tr>
<tr class="even">
<td><code
id="rds_modify_db_instance_:_MasterUserSecretKmsKeyId">MasterUserSecretKmsKeyId</code></td>
<td><p>The Amazon Web Services KMS key identifier to encrypt a secret
that is automatically generated and managed in Amazon Web Services
Secrets Manager.</p>
<p>This setting is valid only if both of the following conditions are
met:</p>
<ul>
<li><p>The DB instance doesn't manage the master user password in Amazon
Web Services Secrets Manager.</p>
<p>If the DB instance already manages the master user password in Amazon
Web Services Secrets Manager, you can't change the KMS key used to
encrypt the secret.</p></li>
<li><p>You are turning on <code>ManageMasterUserPassword</code> to
manage the master user password in Amazon Web Services Secrets
Manager.</p>
<p>If you are turning on <code>ManageMasterUserPassword</code> and don't
specify <code>MasterUserSecretKmsKeyId</code>, then the
<code>aws/secretsmanager</code> KMS key is used to encrypt the secret.
If the secret is in a different Amazon Web Services account, then you
can't use the <code>aws/secretsmanager</code> KMS key to encrypt the
secret, and you must use a customer managed KMS key.</p></li>
</ul>
<p>The Amazon Web Services KMS key identifier is the key ARN, key ID,
alias ARN, or alias name for the KMS key. To use a KMS key in a
different Amazon Web Services account, specify the key ARN or alias
ARN.</p>
<p>There is a default KMS key for your Amazon Web Services account. Your
Amazon Web Services account has a different default KMS key for each
Amazon Web Services Region.</p></td>
</tr>
</tbody>
</table>

### Value

A list with the following syntax:

    list(
      DBInstance = list(
        DBInstanceIdentifier = "string",
        DBInstanceClass = "string",
        Engine = "string",
        DBInstanceStatus = "string",
        AutomaticRestartTime = as.POSIXct(
          "2015-01-01"
        ),
        MasterUsername = "string",
        DBName = "string",
        Endpoint = list(
          Address = "string",
          Port = 123,
          HostedZoneId = "string"
        ),
        AllocatedStorage = 123,
        InstanceCreateTime = as.POSIXct(
          "2015-01-01"
        ),
        PreferredBackupWindow = "string",
        BackupRetentionPeriod = 123,
        DBSecurityGroups = list(
          list(
            DBSecurityGroupName = "string",
            Status = "string"
          )
        ),
        VpcSecurityGroups = list(
          list(
            VpcSecurityGroupId = "string",
            Status = "string"
          )
        ),
        DBParameterGroups = list(
          list(
            DBParameterGroupName = "string",
            ParameterApplyStatus = "string"
          )
        ),
        AvailabilityZone = "string",
        DBSubnetGroup = list(
          DBSubnetGroupName = "string",
          DBSubnetGroupDescription = "string",
          VpcId = "string",
          SubnetGroupStatus = "string",
          Subnets = list(
            list(
              SubnetIdentifier = "string",
              SubnetAvailabilityZone = list(
                Name = "string"
              ),
              SubnetOutpost = list(
                Arn = "string"
              ),
              SubnetStatus = "string"
            )
          ),
          DBSubnetGroupArn = "string",
          SupportedNetworkTypes = list(
            "string"
          )
        ),
        PreferredMaintenanceWindow = "string",
        PendingModifiedValues = list(
          DBInstanceClass = "string",
          AllocatedStorage = 123,
          MasterUserPassword = "string",
          Port = 123,
          BackupRetentionPeriod = 123,
          MultiAZ = TRUE|FALSE,
          EngineVersion = "string",
          LicenseModel = "string",
          Iops = 123,
          DBInstanceIdentifier = "string",
          StorageType = "string",
          CACertificateIdentifier = "string",
          DBSubnetGroupName = "string",
          PendingCloudwatchLogsExports = list(
            LogTypesToEnable = list(
              "string"
            ),
            LogTypesToDisable = list(
              "string"
            )
          ),
          ProcessorFeatures = list(
            list(
              Name = "string",
              Value = "string"
            )
          ),
          IAMDatabaseAuthenticationEnabled = TRUE|FALSE,
          AutomationMode = "full"|"all-paused",
          ResumeFullAutomationModeTime = as.POSIXct(
            "2015-01-01"
          ),
          StorageThroughput = 123
        ),
        LatestRestorableTime = as.POSIXct(
          "2015-01-01"
        ),
        MultiAZ = TRUE|FALSE,
        EngineVersion = "string",
        AutoMinorVersionUpgrade = TRUE|FALSE,
        ReadReplicaSourceDBInstanceIdentifier = "string",
        ReadReplicaDBInstanceIdentifiers = list(
          "string"
        ),
        ReadReplicaDBClusterIdentifiers = list(
          "string"
        ),
        ReplicaMode = "open-read-only"|"mounted",
        LicenseModel = "string",
        Iops = 123,
        OptionGroupMemberships = list(
          list(
            OptionGroupName = "string",
            Status = "string"
          )
        ),
        CharacterSetName = "string",
        NcharCharacterSetName = "string",
        SecondaryAvailabilityZone = "string",
        PubliclyAccessible = TRUE|FALSE,
        StatusInfos = list(
          list(
            StatusType = "string",
            Normal = TRUE|FALSE,
            Status = "string",
            Message = "string"
          )
        ),
        StorageType = "string",
        TdeCredentialArn = "string",
        DbInstancePort = 123,
        DBClusterIdentifier = "string",
        StorageEncrypted = TRUE|FALSE,
        KmsKeyId = "string",
        DbiResourceId = "string",
        CACertificateIdentifier = "string",
        DomainMemberships = list(
          list(
            Domain = "string",
            Status = "string",
            FQDN = "string",
            IAMRoleName = "string"
          )
        ),
        CopyTagsToSnapshot = TRUE|FALSE,
        MonitoringInterval = 123,
        EnhancedMonitoringResourceArn = "string",
        MonitoringRoleArn = "string",
        PromotionTier = 123,
        DBInstanceArn = "string",
        Timezone = "string",
        IAMDatabaseAuthenticationEnabled = TRUE|FALSE,
        PerformanceInsightsEnabled = TRUE|FALSE,
        PerformanceInsightsKMSKeyId = "string",
        PerformanceInsightsRetentionPeriod = 123,
        EnabledCloudwatchLogsExports = list(
          "string"
        ),
        ProcessorFeatures = list(
          list(
            Name = "string",
            Value = "string"
          )
        ),
        DeletionProtection = TRUE|FALSE,
        AssociatedRoles = list(
          list(
            RoleArn = "string",
            FeatureName = "string",
            Status = "string"
          )
        ),
        ListenerEndpoint = list(
          Address = "string",
          Port = 123,
          HostedZoneId = "string"
        ),
        MaxAllocatedStorage = 123,
        TagList = list(
          list(
            Key = "string",
            Value = "string"
          )
        ),
        DBInstanceAutomatedBackupsReplications = list(
          list(
            DBInstanceAutomatedBackupsArn = "string"
          )
        ),
        CustomerOwnedIpEnabled = TRUE|FALSE,
        AwsBackupRecoveryPointArn = "string",
        ActivityStreamStatus = "stopped"|"starting"|"started"|"stopping",
        ActivityStreamKmsKeyId = "string",
        ActivityStreamKinesisStreamName = "string",
        ActivityStreamMode = "sync"|"async",
        ActivityStreamEngineNativeAuditFieldsIncluded = TRUE|FALSE,
        AutomationMode = "full"|"all-paused",
        ResumeFullAutomationModeTime = as.POSIXct(
          "2015-01-01"
        ),
        CustomIamInstanceProfile = "string",
        BackupTarget = "string",
        NetworkType = "string",
        ActivityStreamPolicyStatus = "locked"|"unlocked"|"locking-policy"|"unlocking-policy",
        StorageThroughput = 123,
        DBSystemId = "string",
        MasterUserSecret = list(
          SecretArn = "string",
          SecretStatus = "string",
          KmsKeyId = "string"
        ),
        CertificateDetails = list(
          CAIdentifier = "string",
          ValidTill = as.POSIXct(
            "2015-01-01"
          )
        ),
        ReadReplicaSourceDBClusterIdentifier = "string"
      )
    )

### Request syntax

    svc$modify_db_instance(
      DBInstanceIdentifier = "string",
      AllocatedStorage = 123,
      DBInstanceClass = "string",
      DBSubnetGroupName = "string",
      DBSecurityGroups = list(
        "string"
      ),
      VpcSecurityGroupIds = list(
        "string"
      ),
      ApplyImmediately = TRUE|FALSE,
      MasterUserPassword = "string",
      DBParameterGroupName = "string",
      BackupRetentionPeriod = 123,
      PreferredBackupWindow = "string",
      PreferredMaintenanceWindow = "string",
      MultiAZ = TRUE|FALSE,
      EngineVersion = "string",
      AllowMajorVersionUpgrade = TRUE|FALSE,
      AutoMinorVersionUpgrade = TRUE|FALSE,
      LicenseModel = "string",
      Iops = 123,
      OptionGroupName = "string",
      NewDBInstanceIdentifier = "string",
      StorageType = "string",
      TdeCredentialArn = "string",
      TdeCredentialPassword = "string",
      CACertificateIdentifier = "string",
      Domain = "string",
      CopyTagsToSnapshot = TRUE|FALSE,
      MonitoringInterval = 123,
      DBPortNumber = 123,
      PubliclyAccessible = TRUE|FALSE,
      MonitoringRoleArn = "string",
      DomainIAMRoleName = "string",
      PromotionTier = 123,
      EnableIAMDatabaseAuthentication = TRUE|FALSE,
      EnablePerformanceInsights = TRUE|FALSE,
      PerformanceInsightsKMSKeyId = "string",
      PerformanceInsightsRetentionPeriod = 123,
      CloudwatchLogsExportConfiguration = list(
        EnableLogTypes = list(
          "string"
        ),
        DisableLogTypes = list(
          "string"
        )
      ),
      ProcessorFeatures = list(
        list(
          Name = "string",
          Value = "string"
        )
      ),
      UseDefaultProcessorFeatures = TRUE|FALSE,
      DeletionProtection = TRUE|FALSE,
      MaxAllocatedStorage = 123,
      CertificateRotationRestart = TRUE|FALSE,
      ReplicaMode = "open-read-only"|"mounted",
      EnableCustomerOwnedIp = TRUE|FALSE,
      AwsBackupRecoveryPointArn = "string",
      AutomationMode = "full"|"all-paused",
      ResumeFullAutomationModeMinutes = 123,
      NetworkType = "string",
      StorageThroughput = 123,
      ManageMasterUserPassword = TRUE|FALSE,
      RotateMasterUserPassword = TRUE|FALSE,
      MasterUserSecretKmsKeyId = "string"
    )