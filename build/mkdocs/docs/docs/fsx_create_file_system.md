<table style="width: 100%;">
<tbody>
<tr class="odd">
<td>fsx_create_file_system</td>
<td style="text-align: right;">R Documentation</td>
</tr>
</tbody>
</table>

## Creates a new, empty Amazon FSx file system

### Description

Creates a new, empty Amazon FSx file system. You can create the
following supported Amazon FSx file systems using the
`create_file_system` API operation:

-   Amazon FSx for Lustre

-   Amazon FSx for NetApp ONTAP

-   Amazon FSx for OpenZFS

-   Amazon FSx for Windows File Server

This operation requires a client request token in the request that
Amazon FSx uses to ensure idempotent creation. This means that calling
the operation multiple times with the same client request token has no
effect. By using the idempotent operation, you can retry a
`create_file_system` operation without the risk of creating an extra
file system. This approach can be useful when an initial call fails in a
way that makes it unclear whether a file system was created. Examples
are if a transport level timeout occurred, or your connection was reset.
If you use the same client request token and the initial call created a
file system, the client receives success as long as the parameters are
the same.

If a file system with the specified client request token exists and the
parameters match, `create_file_system` returns the description of the
existing file system. If a file system with the specified client request
token exists and the parameters don't match, this call returns
`IncompatibleParameterError`. If a file system with the specified client
request token doesn't exist, `create_file_system` does the following:

-   Creates a new, empty Amazon FSx file system with an assigned ID, and
    an initial lifecycle state of `CREATING`.

-   Returns the description of the file system in JSON format.

The `create_file_system` call returns while the file system's lifecycle
state is still `CREATING`. You can check the file-system creation status
by calling the `describe_file_systems` operation, which returns the file
system state along with other information.

### Usage

    fsx_create_file_system(ClientRequestToken, FileSystemType,
      StorageCapacity, StorageType, SubnetIds, SecurityGroupIds, Tags,
      KmsKeyId, WindowsConfiguration, LustreConfiguration, OntapConfiguration,
      FileSystemTypeVersion, OpenZFSConfiguration)

### Arguments

<table>
<colgroup>
<col style="width: 35%" />
<col style="width: 65%" />
</colgroup>
<tbody>
<tr class="odd">
<td><code
id="fsx_create_file_system_:_ClientRequestToken">ClientRequestToken</code></td>
<td><p>A string of up to 63 ASCII characters that Amazon FSx uses to
ensure idempotent creation. This string is automatically filled on your
behalf when you use the Command Line Interface (CLI) or an Amazon Web
Services SDK.</p></td>
</tr>
<tr class="even">
<td><code
id="fsx_create_file_system_:_FileSystemType">FileSystemType</code></td>
<td><p>[required] The type of Amazon FSx file system to create. Valid
values are <code>WINDOWS</code>, <code>LUSTRE</code>,
<code>ONTAP</code>, and <code>OPENZFS</code>.</p></td>
</tr>
<tr class="odd">
<td><code
id="fsx_create_file_system_:_StorageCapacity">StorageCapacity</code></td>
<td><p>[required] Sets the storage capacity of the file system that
you're creating, in gibibytes (GiB).</p>
<p><strong>FSx for Lustre file systems</strong> - The amount of storage
capacity that you can configure depends on the value that you set for
<code>StorageType</code> and the Lustre <code>DeploymentType</code>, as
follows:</p>
<ul>
<li><p>For <code>SCRATCH_2</code>, <code>PERSISTENT_2</code> and
<code>PERSISTENT_1</code> deployment types using SSD storage type, the
valid values are 1200 GiB, 2400 GiB, and increments of 2400
GiB.</p></li>
<li><p>For <code>PERSISTENT_1</code> HDD file systems, valid values are
increments of 6000 GiB for 12 MB/s/TiB file systems and increments of
1800 GiB for 40 MB/s/TiB file systems.</p></li>
<li><p>For <code>SCRATCH_1</code> deployment type, valid values are 1200
GiB, 2400 GiB, and increments of 3600 GiB.</p></li>
</ul>
<p><strong>FSx for ONTAP file systems</strong> - The amount of storage
capacity that you can configure is from 1024 GiB up to 196,608 GiB (192
TiB).</p>
<p><strong>FSx for OpenZFS file systems</strong> - The amount of storage
capacity that you can configure is from 64 GiB up to 524,288 GiB (512
TiB).</p>
<p><strong>FSx for Windows File Server file systems</strong> - The
amount of storage capacity that you can configure depends on the value
that you set for <code>StorageType</code> as follows:</p>
<ul>
<li><p>For SSD storage, valid values are 32 GiB-65,536 GiB (64
TiB).</p></li>
<li><p>For HDD storage, valid values are 2000 GiB-65,536 GiB (64
TiB).</p></li>
</ul></td>
</tr>
<tr class="even">
<td><code
id="fsx_create_file_system_:_StorageType">StorageType</code></td>
<td><p>Sets the storage type for the file system that you're creating.
Valid values are <code>SSD</code> and <code>HDD</code>.</p>
<ul>
<li><p>Set to <code>SSD</code> to use solid state drive storage. SSD is
supported on all Windows, Lustre, ONTAP, and OpenZFS deployment
types.</p></li>
<li><p>Set to <code>HDD</code> to use hard disk drive storage. HDD is
supported on <code>SINGLE_AZ_2</code> and <code>MULTI_AZ_1</code>
Windows file system deployment types, and on <code>PERSISTENT_1</code>
Lustre file system deployment types.</p></li>
</ul>
<p>Default value is <code>SSD</code>. For more information, see <a
href="https://docs.aws.amazon.com/fsx/latest/WindowsGuide/optimize-fsx-costs.html#storage-type-options">Storage
type options</a> in the <em>FSx for Windows File Server User Guide</em>
and <a
href="https://docs.aws.amazon.com/fsx/latest/LustreGuide/what-is.html#storage-options">Multiple
storage options</a> in the <em>FSx for Lustre User Guide</em>.</p></td>
</tr>
<tr class="odd">
<td><code id="fsx_create_file_system_:_SubnetIds">SubnetIds</code></td>
<td><p>[required] Specifies the IDs of the subnets that the file system
will be accessible from. For Windows and ONTAP <code>MULTI_AZ_1</code>
deployment types,provide exactly two subnet IDs, one for the preferred
file server and one for the standby file server. You specify one of
these subnets as the preferred subnet using the
<code>WindowsConfiguration &gt; PreferredSubnetID</code> or
<code>OntapConfiguration &gt; PreferredSubnetID</code> properties. For
more information about Multi-AZ file system configuration, see <a
href="https://docs.aws.amazon.com/fsx/latest/WindowsGuide/high-availability-multiAZ.html">Availability
and durability: Single-AZ and Multi-AZ file systems</a> in the
<em>Amazon FSx for Windows User Guide</em> and <a
href="https://docs.aws.amazon.com/fsx/latest/ONTAPGuide/">Availability
and durability</a> in the <em>Amazon FSx for ONTAP User Guide</em>.</p>
<p>For Windows <code>SINGLE_AZ_1</code> and <code>SINGLE_AZ_2</code> and
all Lustre deployment types, provide exactly one subnet ID. The file
server is launched in that subnet's Availability Zone.</p></td>
</tr>
<tr class="even">
<td><code
id="fsx_create_file_system_:_SecurityGroupIds">SecurityGroupIds</code></td>
<td><p>A list of IDs specifying the security groups to apply to all
network interfaces created for file system access. This list isn't
returned in later requests to describe the file system.</p></td>
</tr>
<tr class="odd">
<td><code id="fsx_create_file_system_:_Tags">Tags</code></td>
<td><p>The tags to apply to the file system that's being created. The
key value of the <code>Name</code> tag appears in the console as the
file system name.</p></td>
</tr>
<tr class="even">
<td><code id="fsx_create_file_system_:_KmsKeyId">KmsKeyId</code></td>
<td></td>
</tr>
<tr class="odd">
<td><code
id="fsx_create_file_system_:_WindowsConfiguration">WindowsConfiguration</code></td>
<td><p>The Microsoft Windows configuration for the file system that's
being created.</p></td>
</tr>
<tr class="even">
<td><code
id="fsx_create_file_system_:_LustreConfiguration">LustreConfiguration</code></td>
<td></td>
</tr>
<tr class="odd">
<td><code
id="fsx_create_file_system_:_OntapConfiguration">OntapConfiguration</code></td>
<td></td>
</tr>
<tr class="even">
<td><code
id="fsx_create_file_system_:_FileSystemTypeVersion">FileSystemTypeVersion</code></td>
<td><p>(Optional) For FSx for Lustre file systems, sets the Lustre
version for the file system that you're creating. Valid values are
<code>2.10</code> and <code>2.12</code>:</p>
<ul>
<li><p>2.10 is supported by the Scratch and Persistent_1 Lustre
deployment types.</p></li>
<li><p>2.12 is supported by all Lustre deployment types.
<code>2.12</code> is required when setting FSx for Lustre
<code>DeploymentType</code> to <code>PERSISTENT_2</code>.</p></li>
</ul>
<p>Default value = <code>2.10</code>, except when
<code>DeploymentType</code> is set to <code>PERSISTENT_2</code>, then
the default is <code>2.12</code>.</p>
<p>If you set <code>FileSystemTypeVersion</code> to <code>2.10</code>
for a <code>PERSISTENT_2</code> Lustre deployment type, the
<code>create_file_system</code> operation fails.</p></td>
</tr>
<tr class="odd">
<td><code
id="fsx_create_file_system_:_OpenZFSConfiguration">OpenZFSConfiguration</code></td>
<td><p>The OpenZFS configuration for the file system that's being
created.</p></td>
</tr>
</tbody>
</table>

### Value

A list with the following syntax:

    list(
      FileSystem = list(
        OwnerId = "string",
        CreationTime = as.POSIXct(
          "2015-01-01"
        ),
        FileSystemId = "string",
        FileSystemType = "WINDOWS"|"LUSTRE"|"ONTAP"|"OPENZFS",
        Lifecycle = "AVAILABLE"|"CREATING"|"FAILED"|"DELETING"|"MISCONFIGURED"|"UPDATING"|"MISCONFIGURED_UNAVAILABLE",
        FailureDetails = list(
          Message = "string"
        ),
        StorageCapacity = 123,
        StorageType = "SSD"|"HDD",
        VpcId = "string",
        SubnetIds = list(
          "string"
        ),
        NetworkInterfaceIds = list(
          "string"
        ),
        DNSName = "string",
        KmsKeyId = "string",
        ResourceARN = "string",
        Tags = list(
          list(
            Key = "string",
            Value = "string"
          )
        ),
        WindowsConfiguration = list(
          ActiveDirectoryId = "string",
          SelfManagedActiveDirectoryConfiguration = list(
            DomainName = "string",
            OrganizationalUnitDistinguishedName = "string",
            FileSystemAdministratorsGroup = "string",
            UserName = "string",
            DnsIps = list(
              "string"
            )
          ),
          DeploymentType = "MULTI_AZ_1"|"SINGLE_AZ_1"|"SINGLE_AZ_2",
          RemoteAdministrationEndpoint = "string",
          PreferredSubnetId = "string",
          PreferredFileServerIp = "string",
          ThroughputCapacity = 123,
          MaintenanceOperationsInProgress = list(
            "PATCHING"|"BACKING_UP"
          ),
          WeeklyMaintenanceStartTime = "string",
          DailyAutomaticBackupStartTime = "string",
          AutomaticBackupRetentionDays = 123,
          CopyTagsToBackups = TRUE|FALSE,
          Aliases = list(
            list(
              Name = "string",
              Lifecycle = "AVAILABLE"|"CREATING"|"DELETING"|"CREATE_FAILED"|"DELETE_FAILED"
            )
          ),
          AuditLogConfiguration = list(
            FileAccessAuditLogLevel = "DISABLED"|"SUCCESS_ONLY"|"FAILURE_ONLY"|"SUCCESS_AND_FAILURE",
            FileShareAccessAuditLogLevel = "DISABLED"|"SUCCESS_ONLY"|"FAILURE_ONLY"|"SUCCESS_AND_FAILURE",
            AuditLogDestination = "string"
          )
        ),
        LustreConfiguration = list(
          WeeklyMaintenanceStartTime = "string",
          DataRepositoryConfiguration = list(
            Lifecycle = "CREATING"|"AVAILABLE"|"MISCONFIGURED"|"UPDATING"|"DELETING"|"FAILED",
            ImportPath = "string",
            ExportPath = "string",
            ImportedFileChunkSize = 123,
            AutoImportPolicy = "NONE"|"NEW"|"NEW_CHANGED"|"NEW_CHANGED_DELETED",
            FailureDetails = list(
              Message = "string"
            )
          ),
          DeploymentType = "SCRATCH_1"|"SCRATCH_2"|"PERSISTENT_1"|"PERSISTENT_2",
          PerUnitStorageThroughput = 123,
          MountName = "string",
          DailyAutomaticBackupStartTime = "string",
          AutomaticBackupRetentionDays = 123,
          CopyTagsToBackups = TRUE|FALSE,
          DriveCacheType = "NONE"|"READ",
          DataCompressionType = "NONE"|"LZ4",
          LogConfiguration = list(
            Level = "DISABLED"|"WARN_ONLY"|"ERROR_ONLY"|"WARN_ERROR",
            Destination = "string"
          ),
          RootSquashConfiguration = list(
            RootSquash = "string",
            NoSquashNids = list(
              "string"
            )
          )
        ),
        AdministrativeActions = list(
          list(
            AdministrativeActionType = "FILE_SYSTEM_UPDATE"|"STORAGE_OPTIMIZATION"|"FILE_SYSTEM_ALIAS_ASSOCIATION"|"FILE_SYSTEM_ALIAS_DISASSOCIATION"|"VOLUME_UPDATE"|"SNAPSHOT_UPDATE"|"RELEASE_NFS_V3_LOCKS"|"VOLUME_RESTORE",
            ProgressPercent = 123,
            RequestTime = as.POSIXct(
              "2015-01-01"
            ),
            Status = "FAILED"|"IN_PROGRESS"|"PENDING"|"COMPLETED"|"UPDATED_OPTIMIZING",
            TargetFileSystemValues = list(),
            FailureDetails = list(
              Message = "string"
            ),
            TargetVolumeValues = list(
              CreationTime = as.POSIXct(
                "2015-01-01"
              ),
              FileSystemId = "string",
              Lifecycle = "CREATING"|"CREATED"|"DELETING"|"FAILED"|"MISCONFIGURED"|"PENDING"|"AVAILABLE",
              Name = "string",
              OntapConfiguration = list(
                FlexCacheEndpointType = "NONE"|"ORIGIN"|"CACHE",
                JunctionPath = "string",
                SecurityStyle = "UNIX"|"NTFS"|"MIXED",
                SizeInMegabytes = 123,
                StorageEfficiencyEnabled = TRUE|FALSE,
                StorageVirtualMachineId = "string",
                StorageVirtualMachineRoot = TRUE|FALSE,
                TieringPolicy = list(
                  CoolingPeriod = 123,
                  Name = "SNAPSHOT_ONLY"|"AUTO"|"ALL"|"NONE"
                ),
                UUID = "string",
                OntapVolumeType = "RW"|"DP"|"LS",
                SnapshotPolicy = "string",
                CopyTagsToBackups = TRUE|FALSE
              ),
              ResourceARN = "string",
              Tags = list(
                list(
                  Key = "string",
                  Value = "string"
                )
              ),
              VolumeId = "string",
              VolumeType = "ONTAP"|"OPENZFS",
              LifecycleTransitionReason = list(
                Message = "string"
              ),
              AdministrativeActions = list(),
              OpenZFSConfiguration = list(
                ParentVolumeId = "string",
                VolumePath = "string",
                StorageCapacityReservationGiB = 123,
                StorageCapacityQuotaGiB = 123,
                RecordSizeKiB = 123,
                DataCompressionType = "NONE"|"ZSTD"|"LZ4",
                CopyTagsToSnapshots = TRUE|FALSE,
                OriginSnapshot = list(
                  SnapshotARN = "string",
                  CopyStrategy = "CLONE"|"FULL_COPY"
                ),
                ReadOnly = TRUE|FALSE,
                NfsExports = list(
                  list(
                    ClientConfigurations = list(
                      list(
                        Clients = "string",
                        Options = list(
                          "string"
                        )
                      )
                    )
                  )
                ),
                UserAndGroupQuotas = list(
                  list(
                    Type = "USER"|"GROUP",
                    Id = 123,
                    StorageCapacityQuotaGiB = 123
                  )
                ),
                RestoreToSnapshot = "string",
                DeleteIntermediateSnaphots = TRUE|FALSE,
                DeleteClonedVolumes = TRUE|FALSE
              )
            ),
            TargetSnapshotValues = list(
              ResourceARN = "string",
              SnapshotId = "string",
              Name = "string",
              VolumeId = "string",
              CreationTime = as.POSIXct(
                "2015-01-01"
              ),
              Lifecycle = "PENDING"|"CREATING"|"DELETING"|"AVAILABLE",
              LifecycleTransitionReason = list(
                Message = "string"
              ),
              Tags = list(
                list(
                  Key = "string",
                  Value = "string"
                )
              ),
              AdministrativeActions = list()
            )
          )
        ),
        OntapConfiguration = list(
          AutomaticBackupRetentionDays = 123,
          DailyAutomaticBackupStartTime = "string",
          DeploymentType = "MULTI_AZ_1"|"SINGLE_AZ_1",
          EndpointIpAddressRange = "string",
          Endpoints = list(
            Intercluster = list(
              DNSName = "string",
              IpAddresses = list(
                "string"
              )
            ),
            Management = list(
              DNSName = "string",
              IpAddresses = list(
                "string"
              )
            )
          ),
          DiskIopsConfiguration = list(
            Mode = "AUTOMATIC"|"USER_PROVISIONED",
            Iops = 123
          ),
          PreferredSubnetId = "string",
          RouteTableIds = list(
            "string"
          ),
          ThroughputCapacity = 123,
          WeeklyMaintenanceStartTime = "string"
        ),
        FileSystemTypeVersion = "string",
        OpenZFSConfiguration = list(
          AutomaticBackupRetentionDays = 123,
          CopyTagsToBackups = TRUE|FALSE,
          CopyTagsToVolumes = TRUE|FALSE,
          DailyAutomaticBackupStartTime = "string",
          DeploymentType = "SINGLE_AZ_1"|"SINGLE_AZ_2",
          ThroughputCapacity = 123,
          WeeklyMaintenanceStartTime = "string",
          DiskIopsConfiguration = list(
            Mode = "AUTOMATIC"|"USER_PROVISIONED",
            Iops = 123
          ),
          RootVolumeId = "string"
        )
      )
    )

### Request syntax

    svc$create_file_system(
      ClientRequestToken = "string",
      FileSystemType = "WINDOWS"|"LUSTRE"|"ONTAP"|"OPENZFS",
      StorageCapacity = 123,
      StorageType = "SSD"|"HDD",
      SubnetIds = list(
        "string"
      ),
      SecurityGroupIds = list(
        "string"
      ),
      Tags = list(
        list(
          Key = "string",
          Value = "string"
        )
      ),
      KmsKeyId = "string",
      WindowsConfiguration = list(
        ActiveDirectoryId = "string",
        SelfManagedActiveDirectoryConfiguration = list(
          DomainName = "string",
          OrganizationalUnitDistinguishedName = "string",
          FileSystemAdministratorsGroup = "string",
          UserName = "string",
          Password = "string",
          DnsIps = list(
            "string"
          )
        ),
        DeploymentType = "MULTI_AZ_1"|"SINGLE_AZ_1"|"SINGLE_AZ_2",
        PreferredSubnetId = "string",
        ThroughputCapacity = 123,
        WeeklyMaintenanceStartTime = "string",
        DailyAutomaticBackupStartTime = "string",
        AutomaticBackupRetentionDays = 123,
        CopyTagsToBackups = TRUE|FALSE,
        Aliases = list(
          "string"
        ),
        AuditLogConfiguration = list(
          FileAccessAuditLogLevel = "DISABLED"|"SUCCESS_ONLY"|"FAILURE_ONLY"|"SUCCESS_AND_FAILURE",
          FileShareAccessAuditLogLevel = "DISABLED"|"SUCCESS_ONLY"|"FAILURE_ONLY"|"SUCCESS_AND_FAILURE",
          AuditLogDestination = "string"
        )
      ),
      LustreConfiguration = list(
        WeeklyMaintenanceStartTime = "string",
        ImportPath = "string",
        ExportPath = "string",
        ImportedFileChunkSize = 123,
        DeploymentType = "SCRATCH_1"|"SCRATCH_2"|"PERSISTENT_1"|"PERSISTENT_2",
        AutoImportPolicy = "NONE"|"NEW"|"NEW_CHANGED"|"NEW_CHANGED_DELETED",
        PerUnitStorageThroughput = 123,
        DailyAutomaticBackupStartTime = "string",
        AutomaticBackupRetentionDays = 123,
        CopyTagsToBackups = TRUE|FALSE,
        DriveCacheType = "NONE"|"READ",
        DataCompressionType = "NONE"|"LZ4",
        LogConfiguration = list(
          Level = "DISABLED"|"WARN_ONLY"|"ERROR_ONLY"|"WARN_ERROR",
          Destination = "string"
        ),
        RootSquashConfiguration = list(
          RootSquash = "string",
          NoSquashNids = list(
            "string"
          )
        )
      ),
      OntapConfiguration = list(
        AutomaticBackupRetentionDays = 123,
        DailyAutomaticBackupStartTime = "string",
        DeploymentType = "MULTI_AZ_1"|"SINGLE_AZ_1",
        EndpointIpAddressRange = "string",
        FsxAdminPassword = "string",
        DiskIopsConfiguration = list(
          Mode = "AUTOMATIC"|"USER_PROVISIONED",
          Iops = 123
        ),
        PreferredSubnetId = "string",
        RouteTableIds = list(
          "string"
        ),
        ThroughputCapacity = 123,
        WeeklyMaintenanceStartTime = "string"
      ),
      FileSystemTypeVersion = "string",
      OpenZFSConfiguration = list(
        AutomaticBackupRetentionDays = 123,
        CopyTagsToBackups = TRUE|FALSE,
        CopyTagsToVolumes = TRUE|FALSE,
        DailyAutomaticBackupStartTime = "string",
        DeploymentType = "SINGLE_AZ_1"|"SINGLE_AZ_2",
        ThroughputCapacity = 123,
        WeeklyMaintenanceStartTime = "string",
        DiskIopsConfiguration = list(
          Mode = "AUTOMATIC"|"USER_PROVISIONED",
          Iops = 123
        ),
        RootVolumeConfiguration = list(
          RecordSizeKiB = 123,
          DataCompressionType = "NONE"|"ZSTD"|"LZ4",
          NfsExports = list(
            list(
              ClientConfigurations = list(
                list(
                  Clients = "string",
                  Options = list(
                    "string"
                  )
                )
              )
            )
          ),
          UserAndGroupQuotas = list(
            list(
              Type = "USER"|"GROUP",
              Id = 123,
              StorageCapacityQuotaGiB = 123
            )
          ),
          CopyTagsToSnapshots = TRUE|FALSE,
          ReadOnly = TRUE|FALSE
        )
      )
    )

### Examples

    ## Not run: 
    # This operation creates a new Amazon FSx for Windows File Server file
    # system.
    svc$create_file_system(
      ClientRequestToken = "a8ca07e4-61ec-4399-99f4-19853801bcd5",
      FileSystemType = "WINDOWS",
      KmsKeyId = "arn:aws:kms:us-east-1:012345678912:key/1111abcd-2222-3333-4444-55556666eeff",
      SecurityGroupIds = list(
        "sg-edcd9784"
      ),
      StorageCapacity = 3200L,
      StorageType = "HDD",
      SubnetIds = list(
        "subnet-1234abcd"
      ),
      Tags = list(
        list(
          Key = "Name",
          Value = "MyFileSystem"
        )
      ),
      WindowsConfiguration = list(
        ActiveDirectoryId = "d-1234abcd12",
        Aliases = list(
          "accounting.corp.example.com"
        ),
        AutomaticBackupRetentionDays = 30L,
        DailyAutomaticBackupStartTime = "05:00",
        ThroughputCapacity = 32L,
        WeeklyMaintenanceStartTime = "1:05:00"
      )
    )

    ## End(Not run)