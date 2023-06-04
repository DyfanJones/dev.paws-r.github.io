<table style="width: 100%;">
<tbody>
<tr class="odd">
<td>fsx_update_snapshot</td>
<td style="text-align: right;">R Documentation</td>
</tr>
</tbody>
</table>

## Updates the name of an Amazon FSx for OpenZFS snapshot

### Description

Updates the name of an Amazon FSx for OpenZFS snapshot.

### Usage

    fsx_update_snapshot(ClientRequestToken, Name, SnapshotId)

### Arguments

<table>
<colgroup>
<col style="width: 35%" />
<col style="width: 65%" />
</colgroup>
<tbody>
<tr class="odd">
<td><code
id="fsx_update_snapshot_:_ClientRequestToken">ClientRequestToken</code></td>
<td></td>
</tr>
<tr class="even">
<td><code id="fsx_update_snapshot_:_Name">Name</code></td>
<td><p>[required] The name of the snapshot to update.</p></td>
</tr>
<tr class="odd">
<td><code id="fsx_update_snapshot_:_SnapshotId">SnapshotId</code></td>
<td><p>[required] The ID of the snapshot that you want to update, in the
format <code
style="white-space: pre;">⁠fsvolsnap-0123456789abcdef0⁠</code>.</p></td>
</tr>
</tbody>
</table>

### Value

A list with the following syntax:

    list(
      Snapshot = list(
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
        AdministrativeActions = list(
          list(
            AdministrativeActionType = "FILE_SYSTEM_UPDATE"|"STORAGE_OPTIMIZATION"|"FILE_SYSTEM_ALIAS_ASSOCIATION"|"FILE_SYSTEM_ALIAS_DISASSOCIATION"|"VOLUME_UPDATE"|"SNAPSHOT_UPDATE"|"RELEASE_NFS_V3_LOCKS"|"VOLUME_RESTORE",
            ProgressPercent = 123,
            RequestTime = as.POSIXct(
              "2015-01-01"
            ),
            Status = "FAILED"|"IN_PROGRESS"|"PENDING"|"COMPLETED"|"UPDATED_OPTIMIZING",
            TargetFileSystemValues = list(
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
              AdministrativeActions = list(),
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
            ),
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
            TargetSnapshotValues = list()
          )
        )
      )
    )

### Request syntax

    svc$update_snapshot(
      ClientRequestToken = "string",
      Name = "string",
      SnapshotId = "string"
    )