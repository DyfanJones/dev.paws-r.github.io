<table style="width: 100%;">
<tbody>
<tr class="odd">
<td>securityhub_get_findings</td>
<td style="text-align: right;">R Documentation</td>
</tr>
</tbody>
</table>

## Returns a list of findings that match the specified criteria

### Description

Returns a list of findings that match the specified criteria.

If finding aggregation is enabled, then when you call `get_findings`
from the aggregation Region, the results include all of the matching
findings from both the aggregation Region and the linked Regions.

### Usage

    securityhub_get_findings(Filters, SortCriteria, NextToken, MaxResults)

### Arguments

<table>
<colgroup>
<col style="width: 35%" />
<col style="width: 65%" />
</colgroup>
<tbody>
<tr class="odd">
<td><code id="securityhub_get_findings_:_Filters">Filters</code></td>
<td><p>The finding attributes used to define a condition to filter the
returned findings.</p>
<p>You can filter by up to 10 finding attributes. For each attribute,
you can provide up to 20 filter values.</p>
<p>Note that in the available filter fields, <code>WorkflowState</code>
is deprecated. To search for a finding based on its workflow status, use
<code>WorkflowStatus</code>.</p></td>
</tr>
<tr class="even">
<td><code
id="securityhub_get_findings_:_SortCriteria">SortCriteria</code></td>
<td><p>The finding attributes used to sort the list of returned
findings.</p></td>
</tr>
<tr class="odd">
<td><code
id="securityhub_get_findings_:_NextToken">NextToken</code></td>
<td><p>The token that is required for pagination. On your first call to
the <code>get_findings</code> operation, set the value of this parameter
to <code>NULL</code>.</p>
<p>For subsequent calls to the operation, to continue listing data, set
the value of this parameter to the value returned from the previous
response.</p></td>
</tr>
<tr class="even">
<td><code
id="securityhub_get_findings_:_MaxResults">MaxResults</code></td>
<td><p>The maximum number of findings to return.</p></td>
</tr>
</tbody>
</table>

### Value

A list with the following syntax:

    list(
      Findings = list(
        list(
          SchemaVersion = "string",
          Id = "string",
          ProductArn = "string",
          ProductName = "string",
          CompanyName = "string",
          Region = "string",
          GeneratorId = "string",
          AwsAccountId = "string",
          Types = list(
            "string"
          ),
          FirstObservedAt = "string",
          LastObservedAt = "string",
          CreatedAt = "string",
          UpdatedAt = "string",
          Severity = list(
            Product = 123.0,
            Label = "INFORMATIONAL"|"LOW"|"MEDIUM"|"HIGH"|"CRITICAL",
            Normalized = 123,
            Original = "string"
          ),
          Confidence = 123,
          Criticality = 123,
          Title = "string",
          Description = "string",
          Remediation = list(
            Recommendation = list(
              Text = "string",
              Url = "string"
            )
          ),
          SourceUrl = "string",
          ProductFields = list(
            "string"
          ),
          UserDefinedFields = list(
            "string"
          ),
          Malware = list(
            list(
              Name = "string",
              Type = "ADWARE"|"BLENDED_THREAT"|"BOTNET_AGENT"|"COIN_MINER"|"EXPLOIT_KIT"|"KEYLOGGER"|"MACRO"|"POTENTIALLY_UNWANTED"|"SPYWARE"|"RANSOMWARE"|"REMOTE_ACCESS"|"ROOTKIT"|"TROJAN"|"VIRUS"|"WORM",
              Path = "string",
              State = "OBSERVED"|"REMOVAL_FAILED"|"REMOVED"
            )
          ),
          Network = list(
            Direction = "IN"|"OUT",
            Protocol = "string",
            OpenPortRange = list(
              Begin = 123,
              End = 123
            ),
            SourceIpV4 = "string",
            SourceIpV6 = "string",
            SourcePort = 123,
            SourceDomain = "string",
            SourceMac = "string",
            DestinationIpV4 = "string",
            DestinationIpV6 = "string",
            DestinationPort = 123,
            DestinationDomain = "string"
          ),
          NetworkPath = list(
            list(
              ComponentId = "string",
              ComponentType = "string",
              Egress = list(
                Protocol = "string",
                Destination = list(
                  Address = list(
                    "string"
                  ),
                  PortRanges = list(
                    list(
                      Begin = 123,
                      End = 123
                    )
                  )
                ),
                Source = list(
                  Address = list(
                    "string"
                  ),
                  PortRanges = list(
                    list(
                      Begin = 123,
                      End = 123
                    )
                  )
                )
              ),
              Ingress = list(
                Protocol = "string",
                Destination = list(
                  Address = list(
                    "string"
                  ),
                  PortRanges = list(
                    list(
                      Begin = 123,
                      End = 123
                    )
                  )
                ),
                Source = list(
                  Address = list(
                    "string"
                  ),
                  PortRanges = list(
                    list(
                      Begin = 123,
                      End = 123
                    )
                  )
                )
              )
            )
          ),
          Process = list(
            Name = "string",
            Path = "string",
            Pid = 123,
            ParentPid = 123,
            LaunchedAt = "string",
            TerminatedAt = "string"
          ),
          Threats = list(
            list(
              Name = "string",
              Severity = "string",
              ItemCount = 123,
              FilePaths = list(
                list(
                  FilePath = "string",
                  FileName = "string",
                  ResourceId = "string",
                  Hash = "string"
                )
              )
            )
          ),
          ThreatIntelIndicators = list(
            list(
              Type = "DOMAIN"|"EMAIL_ADDRESS"|"HASH_MD5"|"HASH_SHA1"|"HASH_SHA256"|"HASH_SHA512"|"IPV4_ADDRESS"|"IPV6_ADDRESS"|"MUTEX"|"PROCESS"|"URL",
              Value = "string",
              Category = "BACKDOOR"|"CARD_STEALER"|"COMMAND_AND_CONTROL"|"DROP_SITE"|"EXPLOIT_SITE"|"KEYLOGGER",
              LastObservedAt = "string",
              Source = "string",
              SourceUrl = "string"
            )
          ),
          Resources = list(
            list(
              Type = "string",
              Id = "string",
              Partition = "aws"|"aws-cn"|"aws-us-gov",
              Region = "string",
              ResourceRole = "string",
              Tags = list(
                "string"
              ),
              DataClassification = list(
                DetailedResultsLocation = "string",
                Result = list(
                  MimeType = "string",
                  SizeClassified = 123,
                  AdditionalOccurrences = TRUE|FALSE,
                  Status = list(
                    Code = "string",
                    Reason = "string"
                  ),
                  SensitiveData = list(
                    list(
                      Category = "string",
                      Detections = list(
                        list(
                          Count = 123,
                          Type = "string",
                          Occurrences = list(
                            LineRanges = list(
                              list(
                                Start = 123,
                                End = 123,
                                StartColumn = 123
                              )
                            ),
                            OffsetRanges = list(
                              list(
                                Start = 123,
                                End = 123,
                                StartColumn = 123
                              )
                            ),
                            Pages = list(
                              list(
                                PageNumber = 123,
                                LineRange = list(
                                  Start = 123,
                                  End = 123,
                                  StartColumn = 123
                                ),
                                OffsetRange = list(
                                  Start = 123,
                                  End = 123,
                                  StartColumn = 123
                                )
                              )
                            ),
                            Records = list(
                              list(
                                JsonPath = "string",
                                RecordIndex = 123
                              )
                            ),
                            Cells = list(
                              list(
                                Column = 123,
                                Row = 123,
                                ColumnName = "string",
                                CellReference = "string"
                              )
                            )
                          )
                        )
                      ),
                      TotalCount = 123
                    )
                  ),
                  CustomDataIdentifiers = list(
                    Detections = list(
                      list(
                        Count = 123,
                        Arn = "string",
                        Name = "string",
                        Occurrences = list(
                          LineRanges = list(
                            list(
                              Start = 123,
                              End = 123,
                              StartColumn = 123
                            )
                          ),
                          OffsetRanges = list(
                            list(
                              Start = 123,
                              End = 123,
                              StartColumn = 123
                            )
                          ),
                          Pages = list(
                            list(
                              PageNumber = 123,
                              LineRange = list(
                                Start = 123,
                                End = 123,
                                StartColumn = 123
                              ),
                              OffsetRange = list(
                                Start = 123,
                                End = 123,
                                StartColumn = 123
                              )
                            )
                          ),
                          Records = list(
                            list(
                              JsonPath = "string",
                              RecordIndex = 123
                            )
                          ),
                          Cells = list(
                            list(
                              Column = 123,
                              Row = 123,
                              ColumnName = "string",
                              CellReference = "string"
                            )
                          )
                        )
                      )
                    ),
                    TotalCount = 123
                  )
                )
              ),
              Details = list(
                AwsAutoScalingAutoScalingGroup = list(
                  LaunchConfigurationName = "string",
                  LoadBalancerNames = list(
                    "string"
                  ),
                  HealthCheckType = "string",
                  HealthCheckGracePeriod = 123,
                  CreatedTime = "string",
                  MixedInstancesPolicy = list(
                    InstancesDistribution = list(
                      OnDemandAllocationStrategy = "string",
                      OnDemandBaseCapacity = 123,
                      OnDemandPercentageAboveBaseCapacity = 123,
                      SpotAllocationStrategy = "string",
                      SpotInstancePools = 123,
                      SpotMaxPrice = "string"
                    ),
                    LaunchTemplate = list(
                      LaunchTemplateSpecification = list(
                        LaunchTemplateId = "string",
                        LaunchTemplateName = "string",
                        Version = "string"
                      ),
                      Overrides = list(
                        list(
                          InstanceType = "string",
                          WeightedCapacity = "string"
                        )
                      )
                    )
                  ),
                  AvailabilityZones = list(
                    list(
                      Value = "string"
                    )
                  ),
                  LaunchTemplate = list(
                    LaunchTemplateId = "string",
                    LaunchTemplateName = "string",
                    Version = "string"
                  ),
                  CapacityRebalance = TRUE|FALSE
                ),
                AwsCodeBuildProject = list(
                  EncryptionKey = "string",
                  Artifacts = list(
                    list(
                      ArtifactIdentifier = "string",
                      EncryptionDisabled = TRUE|FALSE,
                      Location = "string",
                      Name = "string",
                      NamespaceType = "string",
                      OverrideArtifactName = TRUE|FALSE,
                      Packaging = "string",
                      Path = "string",
                      Type = "string"
                    )
                  ),
                  Environment = list(
                    Certificate = "string",
                    EnvironmentVariables = list(
                      list(
                        Name = "string",
                        Type = "string",
                        Value = "string"
                      )
                    ),
                    PrivilegedMode = TRUE|FALSE,
                    ImagePullCredentialsType = "string",
                    RegistryCredential = list(
                      Credential = "string",
                      CredentialProvider = "string"
                    ),
                    Type = "string"
                  ),
                  Name = "string",
                  Source = list(
                    Type = "string",
                    Location = "string",
                    GitCloneDepth = 123,
                    InsecureSsl = TRUE|FALSE
                  ),
                  ServiceRole = "string",
                  LogsConfig = list(
                    CloudWatchLogs = list(
                      GroupName = "string",
                      Status = "string",
                      StreamName = "string"
                    ),
                    S3Logs = list(
                      EncryptionDisabled = TRUE|FALSE,
                      Location = "string",
                      Status = "string"
                    )
                  ),
                  VpcConfig = list(
                    VpcId = "string",
                    Subnets = list(
                      "string"
                    ),
                    SecurityGroupIds = list(
                      "string"
                    )
                  ),
                  SecondaryArtifacts = list(
                    list(
                      ArtifactIdentifier = "string",
                      EncryptionDisabled = TRUE|FALSE,
                      Location = "string",
                      Name = "string",
                      NamespaceType = "string",
                      OverrideArtifactName = TRUE|FALSE,
                      Packaging = "string",
                      Path = "string",
                      Type = "string"
                    )
                  )
                ),
                AwsCloudFrontDistribution = list(
                  CacheBehaviors = list(
                    Items = list(
                      list(
                        ViewerProtocolPolicy = "string"
                      )
                    )
                  ),
                  DefaultCacheBehavior = list(
                    ViewerProtocolPolicy = "string"
                  ),
                  DefaultRootObject = "string",
                  DomainName = "string",
                  ETag = "string",
                  LastModifiedTime = "string",
                  Logging = list(
                    Bucket = "string",
                    Enabled = TRUE|FALSE,
                    IncludeCookies = TRUE|FALSE,
                    Prefix = "string"
                  ),
                  Origins = list(
                    Items = list(
                      list(
                        DomainName = "string",
                        Id = "string",
                        OriginPath = "string",
                        S3OriginConfig = list(
                          OriginAccessIdentity = "string"
                        ),
                        CustomOriginConfig = list(
                          HttpPort = 123,
                          HttpsPort = 123,
                          OriginKeepaliveTimeout = 123,
                          OriginProtocolPolicy = "string",
                          OriginReadTimeout = 123,
                          OriginSslProtocols = list(
                            Items = list(
                              "string"
                            ),
                            Quantity = 123
                          )
                        )
                      )
                    )
                  ),
                  OriginGroups = list(
                    Items = list(
                      list(
                        FailoverCriteria = list(
                          StatusCodes = list(
                            Items = list(
                              123
                            ),
                            Quantity = 123
                          )
                        )
                      )
                    )
                  ),
                  ViewerCertificate = list(
                    AcmCertificateArn = "string",
                    Certificate = "string",
                    CertificateSource = "string",
                    CloudFrontDefaultCertificate = TRUE|FALSE,
                    IamCertificateId = "string",
                    MinimumProtocolVersion = "string",
                    SslSupportMethod = "string"
                  ),
                  Status = "string",
                  WebAclId = "string"
                ),
                AwsEc2Instance = list(
                  Type = "string",
                  ImageId = "string",
                  IpV4Addresses = list(
                    "string"
                  ),
                  IpV6Addresses = list(
                    "string"
                  ),
                  KeyName = "string",
                  IamInstanceProfileArn = "string",
                  VpcId = "string",
                  SubnetId = "string",
                  LaunchedAt = "string",
                  NetworkInterfaces = list(
                    list(
                      NetworkInterfaceId = "string"
                    )
                  ),
                  VirtualizationType = "string",
                  MetadataOptions = list(
                    HttpEndpoint = "string",
                    HttpProtocolIpv6 = "string",
                    HttpPutResponseHopLimit = 123,
                    HttpTokens = "string",
                    InstanceMetadataTags = "string"
                  ),
                  Monitoring = list(
                    State = "string"
                  )
                ),
                AwsEc2NetworkInterface = list(
                  Attachment = list(
                    AttachTime = "string",
                    AttachmentId = "string",
                    DeleteOnTermination = TRUE|FALSE,
                    DeviceIndex = 123,
                    InstanceId = "string",
                    InstanceOwnerId = "string",
                    Status = "string"
                  ),
                  NetworkInterfaceId = "string",
                  SecurityGroups = list(
                    list(
                      GroupName = "string",
                      GroupId = "string"
                    )
                  ),
                  SourceDestCheck = TRUE|FALSE,
                  IpV6Addresses = list(
                    list(
                      IpV6Address = "string"
                    )
                  ),
                  PrivateIpAddresses = list(
                    list(
                      PrivateIpAddress = "string",
                      PrivateDnsName = "string"
                    )
                  ),
                  PublicDnsName = "string",
                  PublicIp = "string"
                ),
                AwsEc2SecurityGroup = list(
                  GroupName = "string",
                  GroupId = "string",
                  OwnerId = "string",
                  VpcId = "string",
                  IpPermissions = list(
                    list(
                      IpProtocol = "string",
                      FromPort = 123,
                      ToPort = 123,
                      UserIdGroupPairs = list(
                        list(
                          GroupId = "string",
                          GroupName = "string",
                          PeeringStatus = "string",
                          UserId = "string",
                          VpcId = "string",
                          VpcPeeringConnectionId = "string"
                        )
                      ),
                      IpRanges = list(
                        list(
                          CidrIp = "string"
                        )
                      ),
                      Ipv6Ranges = list(
                        list(
                          CidrIpv6 = "string"
                        )
                      ),
                      PrefixListIds = list(
                        list(
                          PrefixListId = "string"
                        )
                      )
                    )
                  ),
                  IpPermissionsEgress = list(
                    list(
                      IpProtocol = "string",
                      FromPort = 123,
                      ToPort = 123,
                      UserIdGroupPairs = list(
                        list(
                          GroupId = "string",
                          GroupName = "string",
                          PeeringStatus = "string",
                          UserId = "string",
                          VpcId = "string",
                          VpcPeeringConnectionId = "string"
                        )
                      ),
                      IpRanges = list(
                        list(
                          CidrIp = "string"
                        )
                      ),
                      Ipv6Ranges = list(
                        list(
                          CidrIpv6 = "string"
                        )
                      ),
                      PrefixListIds = list(
                        list(
                          PrefixListId = "string"
                        )
                      )
                    )
                  )
                ),
                AwsEc2Volume = list(
                  CreateTime = "string",
                  DeviceName = "string",
                  Encrypted = TRUE|FALSE,
                  Size = 123,
                  SnapshotId = "string",
                  Status = "string",
                  KmsKeyId = "string",
                  Attachments = list(
                    list(
                      AttachTime = "string",
                      DeleteOnTermination = TRUE|FALSE,
                      InstanceId = "string",
                      Status = "string"
                    )
                  ),
                  VolumeId = "string",
                  VolumeType = "string",
                  VolumeScanStatus = "string"
                ),
                AwsEc2Vpc = list(
                  CidrBlockAssociationSet = list(
                    list(
                      AssociationId = "string",
                      CidrBlock = "string",
                      CidrBlockState = "string"
                    )
                  ),
                  Ipv6CidrBlockAssociationSet = list(
                    list(
                      AssociationId = "string",
                      Ipv6CidrBlock = "string",
                      CidrBlockState = "string"
                    )
                  ),
                  DhcpOptionsId = "string",
                  State = "string"
                ),
                AwsEc2Eip = list(
                  InstanceId = "string",
                  PublicIp = "string",
                  AllocationId = "string",
                  AssociationId = "string",
                  Domain = "string",
                  PublicIpv4Pool = "string",
                  NetworkBorderGroup = "string",
                  NetworkInterfaceId = "string",
                  NetworkInterfaceOwnerId = "string",
                  PrivateIpAddress = "string"
                ),
                AwsEc2Subnet = list(
                  AssignIpv6AddressOnCreation = TRUE|FALSE,
                  AvailabilityZone = "string",
                  AvailabilityZoneId = "string",
                  AvailableIpAddressCount = 123,
                  CidrBlock = "string",
                  DefaultForAz = TRUE|FALSE,
                  MapPublicIpOnLaunch = TRUE|FALSE,
                  OwnerId = "string",
                  State = "string",
                  SubnetArn = "string",
                  SubnetId = "string",
                  VpcId = "string",
                  Ipv6CidrBlockAssociationSet = list(
                    list(
                      AssociationId = "string",
                      Ipv6CidrBlock = "string",
                      CidrBlockState = "string"
                    )
                  )
                ),
                AwsEc2NetworkAcl = list(
                  IsDefault = TRUE|FALSE,
                  NetworkAclId = "string",
                  OwnerId = "string",
                  VpcId = "string",
                  Associations = list(
                    list(
                      NetworkAclAssociationId = "string",
                      NetworkAclId = "string",
                      SubnetId = "string"
                    )
                  ),
                  Entries = list(
                    list(
                      CidrBlock = "string",
                      Egress = TRUE|FALSE,
                      IcmpTypeCode = list(
                        Code = 123,
                        Type = 123
                      ),
                      Ipv6CidrBlock = "string",
                      PortRange = list(
                        From = 123,
                        To = 123
                      ),
                      Protocol = "string",
                      RuleAction = "string",
                      RuleNumber = 123
                    )
                  )
                ),
                AwsElbv2LoadBalancer = list(
                  AvailabilityZones = list(
                    list(
                      ZoneName = "string",
                      SubnetId = "string"
                    )
                  ),
                  CanonicalHostedZoneId = "string",
                  CreatedTime = "string",
                  DNSName = "string",
                  IpAddressType = "string",
                  Scheme = "string",
                  SecurityGroups = list(
                    "string"
                  ),
                  State = list(
                    Code = "string",
                    Reason = "string"
                  ),
                  Type = "string",
                  VpcId = "string",
                  LoadBalancerAttributes = list(
                    list(
                      Key = "string",
                      Value = "string"
                    )
                  )
                ),
                AwsElasticBeanstalkEnvironment = list(
                  ApplicationName = "string",
                  Cname = "string",
                  DateCreated = "string",
                  DateUpdated = "string",
                  Description = "string",
                  EndpointUrl = "string",
                  EnvironmentArn = "string",
                  EnvironmentId = "string",
                  EnvironmentLinks = list(
                    list(
                      EnvironmentName = "string",
                      LinkName = "string"
                    )
                  ),
                  EnvironmentName = "string",
                  OptionSettings = list(
                    list(
                      Namespace = "string",
                      OptionName = "string",
                      ResourceName = "string",
                      Value = "string"
                    )
                  ),
                  PlatformArn = "string",
                  SolutionStackName = "string",
                  Status = "string",
                  Tier = list(
                    Name = "string",
                    Type = "string",
                    Version = "string"
                  ),
                  VersionLabel = "string"
                ),
                AwsElasticsearchDomain = list(
                  AccessPolicies = "string",
                  DomainEndpointOptions = list(
                    EnforceHTTPS = TRUE|FALSE,
                    TLSSecurityPolicy = "string"
                  ),
                  DomainId = "string",
                  DomainName = "string",
                  Endpoint = "string",
                  Endpoints = list(
                    "string"
                  ),
                  ElasticsearchVersion = "string",
                  ElasticsearchClusterConfig = list(
                    DedicatedMasterCount = 123,
                    DedicatedMasterEnabled = TRUE|FALSE,
                    DedicatedMasterType = "string",
                    InstanceCount = 123,
                    InstanceType = "string",
                    ZoneAwarenessConfig = list(
                      AvailabilityZoneCount = 123
                    ),
                    ZoneAwarenessEnabled = TRUE|FALSE
                  ),
                  EncryptionAtRestOptions = list(
                    Enabled = TRUE|FALSE,
                    KmsKeyId = "string"
                  ),
                  LogPublishingOptions = list(
                    IndexSlowLogs = list(
                      CloudWatchLogsLogGroupArn = "string",
                      Enabled = TRUE|FALSE
                    ),
                    SearchSlowLogs = list(
                      CloudWatchLogsLogGroupArn = "string",
                      Enabled = TRUE|FALSE
                    ),
                    AuditLogs = list(
                      CloudWatchLogsLogGroupArn = "string",
                      Enabled = TRUE|FALSE
                    )
                  ),
                  NodeToNodeEncryptionOptions = list(
                    Enabled = TRUE|FALSE
                  ),
                  ServiceSoftwareOptions = list(
                    AutomatedUpdateDate = "string",
                    Cancellable = TRUE|FALSE,
                    CurrentVersion = "string",
                    Description = "string",
                    NewVersion = "string",
                    UpdateAvailable = TRUE|FALSE,
                    UpdateStatus = "string"
                  ),
                  VPCOptions = list(
                    AvailabilityZones = list(
                      "string"
                    ),
                    SecurityGroupIds = list(
                      "string"
                    ),
                    SubnetIds = list(
                      "string"
                    ),
                    VPCId = "string"
                  )
                ),
                AwsS3Bucket = list(
                  OwnerId = "string",
                  OwnerName = "string",
                  OwnerAccountId = "string",
                  CreatedAt = "string",
                  ServerSideEncryptionConfiguration = list(
                    Rules = list(
                      list(
                        ApplyServerSideEncryptionByDefault = list(
                          SSEAlgorithm = "string",
                          KMSMasterKeyID = "string"
                        )
                      )
                    )
                  ),
                  BucketLifecycleConfiguration = list(
                    Rules = list(
                      list(
                        AbortIncompleteMultipartUpload = list(
                          DaysAfterInitiation = 123
                        ),
                        ExpirationDate = "string",
                        ExpirationInDays = 123,
                        ExpiredObjectDeleteMarker = TRUE|FALSE,
                        Filter = list(
                          Predicate = list(
                            Operands = list(
                              list(
                                Prefix = "string",
                                Tag = list(
                                  Key = "string",
                                  Value = "string"
                                ),
                                Type = "string"
                              )
                            ),
                            Prefix = "string",
                            Tag = list(
                              Key = "string",
                              Value = "string"
                            ),
                            Type = "string"
                          )
                        ),
                        ID = "string",
                        NoncurrentVersionExpirationInDays = 123,
                        NoncurrentVersionTransitions = list(
                          list(
                            Days = 123,
                            StorageClass = "string"
                          )
                        ),
                        Prefix = "string",
                        Status = "string",
                        Transitions = list(
                          list(
                            Date = "string",
                            Days = 123,
                            StorageClass = "string"
                          )
                        )
                      )
                    )
                  ),
                  PublicAccessBlockConfiguration = list(
                    BlockPublicAcls = TRUE|FALSE,
                    BlockPublicPolicy = TRUE|FALSE,
                    IgnorePublicAcls = TRUE|FALSE,
                    RestrictPublicBuckets = TRUE|FALSE
                  ),
                  AccessControlList = "string",
                  BucketLoggingConfiguration = list(
                    DestinationBucketName = "string",
                    LogFilePrefix = "string"
                  ),
                  BucketWebsiteConfiguration = list(
                    ErrorDocument = "string",
                    IndexDocumentSuffix = "string",
                    RedirectAllRequestsTo = list(
                      Hostname = "string",
                      Protocol = "string"
                    ),
                    RoutingRules = list(
                      list(
                        Condition = list(
                          HttpErrorCodeReturnedEquals = "string",
                          KeyPrefixEquals = "string"
                        ),
                        Redirect = list(
                          Hostname = "string",
                          HttpRedirectCode = "string",
                          Protocol = "string",
                          ReplaceKeyPrefixWith = "string",
                          ReplaceKeyWith = "string"
                        )
                      )
                    )
                  ),
                  BucketNotificationConfiguration = list(
                    Configurations = list(
                      list(
                        Events = list(
                          "string"
                        ),
                        Filter = list(
                          S3KeyFilter = list(
                            FilterRules = list(
                              list(
                                Name = "Prefix"|"Suffix",
                                Value = "string"
                              )
                            )
                          )
                        ),
                        Destination = "string",
                        Type = "string"
                      )
                    )
                  ),
                  BucketVersioningConfiguration = list(
                    IsMfaDeleteEnabled = TRUE|FALSE,
                    Status = "string"
                  ),
                  ObjectLockConfiguration = list(
                    ObjectLockEnabled = "string",
                    Rule = list(
                      DefaultRetention = list(
                        Days = 123,
                        Mode = "string",
                        Years = 123
                      )
                    )
                  )
                ),
                AwsS3AccountPublicAccessBlock = list(
                  BlockPublicAcls = TRUE|FALSE,
                  BlockPublicPolicy = TRUE|FALSE,
                  IgnorePublicAcls = TRUE|FALSE,
                  RestrictPublicBuckets = TRUE|FALSE
                ),
                AwsS3Object = list(
                  LastModified = "string",
                  ETag = "string",
                  VersionId = "string",
                  ContentType = "string",
                  ServerSideEncryption = "string",
                  SSEKMSKeyId = "string"
                ),
                AwsSecretsManagerSecret = list(
                  RotationRules = list(
                    AutomaticallyAfterDays = 123
                  ),
                  RotationOccurredWithinFrequency = TRUE|FALSE,
                  KmsKeyId = "string",
                  RotationEnabled = TRUE|FALSE,
                  RotationLambdaArn = "string",
                  Deleted = TRUE|FALSE,
                  Name = "string",
                  Description = "string"
                ),
                AwsIamAccessKey = list(
                  UserName = "string",
                  Status = "Active"|"Inactive",
                  CreatedAt = "string",
                  PrincipalId = "string",
                  PrincipalType = "string",
                  PrincipalName = "string",
                  AccountId = "string",
                  AccessKeyId = "string",
                  SessionContext = list(
                    Attributes = list(
                      MfaAuthenticated = TRUE|FALSE,
                      CreationDate = "string"
                    ),
                    SessionIssuer = list(
                      Type = "string",
                      PrincipalId = "string",
                      Arn = "string",
                      AccountId = "string",
                      UserName = "string"
                    )
                  )
                ),
                AwsIamUser = list(
                  AttachedManagedPolicies = list(
                    list(
                      PolicyName = "string",
                      PolicyArn = "string"
                    )
                  ),
                  CreateDate = "string",
                  GroupList = list(
                    "string"
                  ),
                  Path = "string",
                  PermissionsBoundary = list(
                    PermissionsBoundaryArn = "string",
                    PermissionsBoundaryType = "string"
                  ),
                  UserId = "string",
                  UserName = "string",
                  UserPolicyList = list(
                    list(
                      PolicyName = "string"
                    )
                  )
                ),
                AwsIamPolicy = list(
                  AttachmentCount = 123,
                  CreateDate = "string",
                  DefaultVersionId = "string",
                  Description = "string",
                  IsAttachable = TRUE|FALSE,
                  Path = "string",
                  PermissionsBoundaryUsageCount = 123,
                  PolicyId = "string",
                  PolicyName = "string",
                  PolicyVersionList = list(
                    list(
                      VersionId = "string",
                      IsDefaultVersion = TRUE|FALSE,
                      CreateDate = "string"
                    )
                  ),
                  UpdateDate = "string"
                ),
                AwsApiGatewayV2Stage = list(
                  ClientCertificateId = "string",
                  CreatedDate = "string",
                  Description = "string",
                  DefaultRouteSettings = list(
                    DetailedMetricsEnabled = TRUE|FALSE,
                    LoggingLevel = "string",
                    DataTraceEnabled = TRUE|FALSE,
                    ThrottlingBurstLimit = 123,
                    ThrottlingRateLimit = 123.0
                  ),
                  DeploymentId = "string",
                  LastUpdatedDate = "string",
                  RouteSettings = list(
                    DetailedMetricsEnabled = TRUE|FALSE,
                    LoggingLevel = "string",
                    DataTraceEnabled = TRUE|FALSE,
                    ThrottlingBurstLimit = 123,
                    ThrottlingRateLimit = 123.0
                  ),
                  StageName = "string",
                  StageVariables = list(
                    "string"
                  ),
                  AccessLogSettings = list(
                    Format = "string",
                    DestinationArn = "string"
                  ),
                  AutoDeploy = TRUE|FALSE,
                  LastDeploymentStatusMessage = "string",
                  ApiGatewayManaged = TRUE|FALSE
                ),
                AwsApiGatewayV2Api = list(
                  ApiEndpoint = "string",
                  ApiId = "string",
                  ApiKeySelectionExpression = "string",
                  CreatedDate = "string",
                  Description = "string",
                  Version = "string",
                  Name = "string",
                  ProtocolType = "string",
                  RouteSelectionExpression = "string",
                  CorsConfiguration = list(
                    AllowOrigins = list(
                      "string"
                    ),
                    AllowCredentials = TRUE|FALSE,
                    ExposeHeaders = list(
                      "string"
                    ),
                    MaxAge = 123,
                    AllowMethods = list(
                      "string"
                    ),
                    AllowHeaders = list(
                      "string"
                    )
                  )
                ),
                AwsDynamoDbTable = list(
                  AttributeDefinitions = list(
                    list(
                      AttributeName = "string",
                      AttributeType = "string"
                    )
                  ),
                  BillingModeSummary = list(
                    BillingMode = "string",
                    LastUpdateToPayPerRequestDateTime = "string"
                  ),
                  CreationDateTime = "string",
                  GlobalSecondaryIndexes = list(
                    list(
                      Backfilling = TRUE|FALSE,
                      IndexArn = "string",
                      IndexName = "string",
                      IndexSizeBytes = 123,
                      IndexStatus = "string",
                      ItemCount = 123,
                      KeySchema = list(
                        list(
                          AttributeName = "string",
                          KeyType = "string"
                        )
                      ),
                      Projection = list(
                        NonKeyAttributes = list(
                          "string"
                        ),
                        ProjectionType = "string"
                      ),
                      ProvisionedThroughput = list(
                        LastDecreaseDateTime = "string",
                        LastIncreaseDateTime = "string",
                        NumberOfDecreasesToday = 123,
                        ReadCapacityUnits = 123,
                        WriteCapacityUnits = 123
                      )
                    )
                  ),
                  GlobalTableVersion = "string",
                  ItemCount = 123,
                  KeySchema = list(
                    list(
                      AttributeName = "string",
                      KeyType = "string"
                    )
                  ),
                  LatestStreamArn = "string",
                  LatestStreamLabel = "string",
                  LocalSecondaryIndexes = list(
                    list(
                      IndexArn = "string",
                      IndexName = "string",
                      KeySchema = list(
                        list(
                          AttributeName = "string",
                          KeyType = "string"
                        )
                      ),
                      Projection = list(
                        NonKeyAttributes = list(
                          "string"
                        ),
                        ProjectionType = "string"
                      )
                    )
                  ),
                  ProvisionedThroughput = list(
                    LastDecreaseDateTime = "string",
                    LastIncreaseDateTime = "string",
                    NumberOfDecreasesToday = 123,
                    ReadCapacityUnits = 123,
                    WriteCapacityUnits = 123
                  ),
                  Replicas = list(
                    list(
                      GlobalSecondaryIndexes = list(
                        list(
                          IndexName = "string",
                          ProvisionedThroughputOverride = list(
                            ReadCapacityUnits = 123
                          )
                        )
                      ),
                      KmsMasterKeyId = "string",
                      ProvisionedThroughputOverride = list(
                        ReadCapacityUnits = 123
                      ),
                      RegionName = "string",
                      ReplicaStatus = "string",
                      ReplicaStatusDescription = "string"
                    )
                  ),
                  RestoreSummary = list(
                    SourceBackupArn = "string",
                    SourceTableArn = "string",
                    RestoreDateTime = "string",
                    RestoreInProgress = TRUE|FALSE
                  ),
                  SseDescription = list(
                    InaccessibleEncryptionDateTime = "string",
                    Status = "string",
                    SseType = "string",
                    KmsMasterKeyArn = "string"
                  ),
                  StreamSpecification = list(
                    StreamEnabled = TRUE|FALSE,
                    StreamViewType = "string"
                  ),
                  TableId = "string",
                  TableName = "string",
                  TableSizeBytes = 123,
                  TableStatus = "string"
                ),
                AwsApiGatewayStage = list(
                  DeploymentId = "string",
                  ClientCertificateId = "string",
                  StageName = "string",
                  Description = "string",
                  CacheClusterEnabled = TRUE|FALSE,
                  CacheClusterSize = "string",
                  CacheClusterStatus = "string",
                  MethodSettings = list(
                    list(
                      MetricsEnabled = TRUE|FALSE,
                      LoggingLevel = "string",
                      DataTraceEnabled = TRUE|FALSE,
                      ThrottlingBurstLimit = 123,
                      ThrottlingRateLimit = 123.0,
                      CachingEnabled = TRUE|FALSE,
                      CacheTtlInSeconds = 123,
                      CacheDataEncrypted = TRUE|FALSE,
                      RequireAuthorizationForCacheControl = TRUE|FALSE,
                      UnauthorizedCacheControlHeaderStrategy = "string",
                      HttpMethod = "string",
                      ResourcePath = "string"
                    )
                  ),
                  Variables = list(
                    "string"
                  ),
                  DocumentationVersion = "string",
                  AccessLogSettings = list(
                    Format = "string",
                    DestinationArn = "string"
                  ),
                  CanarySettings = list(
                    PercentTraffic = 123.0,
                    DeploymentId = "string",
                    StageVariableOverrides = list(
                      "string"
                    ),
                    UseStageCache = TRUE|FALSE
                  ),
                  TracingEnabled = TRUE|FALSE,
                  CreatedDate = "string",
                  LastUpdatedDate = "string",
                  WebAclArn = "string"
                ),
                AwsApiGatewayRestApi = list(
                  Id = "string",
                  Name = "string",
                  Description = "string",
                  CreatedDate = "string",
                  Version = "string",
                  BinaryMediaTypes = list(
                    "string"
                  ),
                  MinimumCompressionSize = 123,
                  ApiKeySource = "string",
                  EndpointConfiguration = list(
                    Types = list(
                      "string"
                    )
                  )
                ),
                AwsCloudTrailTrail = list(
                  CloudWatchLogsLogGroupArn = "string",
                  CloudWatchLogsRoleArn = "string",
                  HasCustomEventSelectors = TRUE|FALSE,
                  HomeRegion = "string",
                  IncludeGlobalServiceEvents = TRUE|FALSE,
                  IsMultiRegionTrail = TRUE|FALSE,
                  IsOrganizationTrail = TRUE|FALSE,
                  KmsKeyId = "string",
                  LogFileValidationEnabled = TRUE|FALSE,
                  Name = "string",
                  S3BucketName = "string",
                  S3KeyPrefix = "string",
                  SnsTopicArn = "string",
                  SnsTopicName = "string",
                  TrailArn = "string"
                ),
                AwsSsmPatchCompliance = list(
                  Patch = list(
                    ComplianceSummary = list(
                      Status = "string",
                      CompliantCriticalCount = 123,
                      CompliantHighCount = 123,
                      CompliantMediumCount = 123,
                      ExecutionType = "string",
                      NonCompliantCriticalCount = 123,
                      CompliantInformationalCount = 123,
                      NonCompliantInformationalCount = 123,
                      CompliantUnspecifiedCount = 123,
                      NonCompliantLowCount = 123,
                      NonCompliantHighCount = 123,
                      CompliantLowCount = 123,
                      ComplianceType = "string",
                      PatchBaselineId = "string",
                      OverallSeverity = "string",
                      NonCompliantMediumCount = 123,
                      NonCompliantUnspecifiedCount = 123,
                      PatchGroup = "string"
                    )
                  )
                ),
                AwsCertificateManagerCertificate = list(
                  CertificateAuthorityArn = "string",
                  CreatedAt = "string",
                  DomainName = "string",
                  DomainValidationOptions = list(
                    list(
                      DomainName = "string",
                      ResourceRecord = list(
                        Name = "string",
                        Type = "string",
                        Value = "string"
                      ),
                      ValidationDomain = "string",
                      ValidationEmails = list(
                        "string"
                      ),
                      ValidationMethod = "string",
                      ValidationStatus = "string"
                    )
                  ),
                  ExtendedKeyUsages = list(
                    list(
                      Name = "string",
                      OId = "string"
                    )
                  ),
                  FailureReason = "string",
                  ImportedAt = "string",
                  InUseBy = list(
                    "string"
                  ),
                  IssuedAt = "string",
                  Issuer = "string",
                  KeyAlgorithm = "string",
                  KeyUsages = list(
                    list(
                      Name = "string"
                    )
                  ),
                  NotAfter = "string",
                  NotBefore = "string",
                  Options = list(
                    CertificateTransparencyLoggingPreference = "string"
                  ),
                  RenewalEligibility = "string",
                  RenewalSummary = list(
                    DomainValidationOptions = list(
                      list(
                        DomainName = "string",
                        ResourceRecord = list(
                          Name = "string",
                          Type = "string",
                          Value = "string"
                        ),
                        ValidationDomain = "string",
                        ValidationEmails = list(
                          "string"
                        ),
                        ValidationMethod = "string",
                        ValidationStatus = "string"
                      )
                    ),
                    RenewalStatus = "string",
                    RenewalStatusReason = "string",
                    UpdatedAt = "string"
                  ),
                  Serial = "string",
                  SignatureAlgorithm = "string",
                  Status = "string",
                  Subject = "string",
                  SubjectAlternativeNames = list(
                    "string"
                  ),
                  Type = "string"
                ),
                AwsRedshiftCluster = list(
                  AllowVersionUpgrade = TRUE|FALSE,
                  AutomatedSnapshotRetentionPeriod = 123,
                  AvailabilityZone = "string",
                  ClusterAvailabilityStatus = "string",
                  ClusterCreateTime = "string",
                  ClusterIdentifier = "string",
                  ClusterNodes = list(
                    list(
                      NodeRole = "string",
                      PrivateIpAddress = "string",
                      PublicIpAddress = "string"
                    )
                  ),
                  ClusterParameterGroups = list(
                    list(
                      ClusterParameterStatusList = list(
                        list(
                          ParameterName = "string",
                          ParameterApplyStatus = "string",
                          ParameterApplyErrorDescription = "string"
                        )
                      ),
                      ParameterApplyStatus = "string",
                      ParameterGroupName = "string"
                    )
                  ),
                  ClusterPublicKey = "string",
                  ClusterRevisionNumber = "string",
                  ClusterSecurityGroups = list(
                    list(
                      ClusterSecurityGroupName = "string",
                      Status = "string"
                    )
                  ),
                  ClusterSnapshotCopyStatus = list(
                    DestinationRegion = "string",
                    ManualSnapshotRetentionPeriod = 123,
                    RetentionPeriod = 123,
                    SnapshotCopyGrantName = "string"
                  ),
                  ClusterStatus = "string",
                  ClusterSubnetGroupName = "string",
                  ClusterVersion = "string",
                  DBName = "string",
                  DeferredMaintenanceWindows = list(
                    list(
                      DeferMaintenanceEndTime = "string",
                      DeferMaintenanceIdentifier = "string",
                      DeferMaintenanceStartTime = "string"
                    )
                  ),
                  ElasticIpStatus = list(
                    ElasticIp = "string",
                    Status = "string"
                  ),
                  ElasticResizeNumberOfNodeOptions = "string",
                  Encrypted = TRUE|FALSE,
                  Endpoint = list(
                    Address = "string",
                    Port = 123
                  ),
                  EnhancedVpcRouting = TRUE|FALSE,
                  ExpectedNextSnapshotScheduleTime = "string",
                  ExpectedNextSnapshotScheduleTimeStatus = "string",
                  HsmStatus = list(
                    HsmClientCertificateIdentifier = "string",
                    HsmConfigurationIdentifier = "string",
                    Status = "string"
                  ),
                  IamRoles = list(
                    list(
                      ApplyStatus = "string",
                      IamRoleArn = "string"
                    )
                  ),
                  KmsKeyId = "string",
                  MaintenanceTrackName = "string",
                  ManualSnapshotRetentionPeriod = 123,
                  MasterUsername = "string",
                  NextMaintenanceWindowStartTime = "string",
                  NodeType = "string",
                  NumberOfNodes = 123,
                  PendingActions = list(
                    "string"
                  ),
                  PendingModifiedValues = list(
                    AutomatedSnapshotRetentionPeriod = 123,
                    ClusterIdentifier = "string",
                    ClusterType = "string",
                    ClusterVersion = "string",
                    EncryptionType = "string",
                    EnhancedVpcRouting = TRUE|FALSE,
                    MaintenanceTrackName = "string",
                    MasterUserPassword = "string",
                    NodeType = "string",
                    NumberOfNodes = 123,
                    PubliclyAccessible = TRUE|FALSE
                  ),
                  PreferredMaintenanceWindow = "string",
                  PubliclyAccessible = TRUE|FALSE,
                  ResizeInfo = list(
                    AllowCancelResize = TRUE|FALSE,
                    ResizeType = "string"
                  ),
                  RestoreStatus = list(
                    CurrentRestoreRateInMegaBytesPerSecond = 123.0,
                    ElapsedTimeInSeconds = 123,
                    EstimatedTimeToCompletionInSeconds = 123,
                    ProgressInMegaBytes = 123,
                    SnapshotSizeInMegaBytes = 123,
                    Status = "string"
                  ),
                  SnapshotScheduleIdentifier = "string",
                  SnapshotScheduleState = "string",
                  VpcId = "string",
                  VpcSecurityGroups = list(
                    list(
                      Status = "string",
                      VpcSecurityGroupId = "string"
                    )
                  ),
                  LoggingStatus = list(
                    BucketName = "string",
                    LastFailureMessage = "string",
                    LastFailureTime = "string",
                    LastSuccessfulDeliveryTime = "string",
                    LoggingEnabled = TRUE|FALSE,
                    S3KeyPrefix = "string"
                  )
                ),
                AwsElbLoadBalancer = list(
                  AvailabilityZones = list(
                    "string"
                  ),
                  BackendServerDescriptions = list(
                    list(
                      InstancePort = 123,
                      PolicyNames = list(
                        "string"
                      )
                    )
                  ),
                  CanonicalHostedZoneName = "string",
                  CanonicalHostedZoneNameID = "string",
                  CreatedTime = "string",
                  DnsName = "string",
                  HealthCheck = list(
                    HealthyThreshold = 123,
                    Interval = 123,
                    Target = "string",
                    Timeout = 123,
                    UnhealthyThreshold = 123
                  ),
                  Instances = list(
                    list(
                      InstanceId = "string"
                    )
                  ),
                  ListenerDescriptions = list(
                    list(
                      Listener = list(
                        InstancePort = 123,
                        InstanceProtocol = "string",
                        LoadBalancerPort = 123,
                        Protocol = "string",
                        SslCertificateId = "string"
                      ),
                      PolicyNames = list(
                        "string"
                      )
                    )
                  ),
                  LoadBalancerAttributes = list(
                    AccessLog = list(
                      EmitInterval = 123,
                      Enabled = TRUE|FALSE,
                      S3BucketName = "string",
                      S3BucketPrefix = "string"
                    ),
                    ConnectionDraining = list(
                      Enabled = TRUE|FALSE,
                      Timeout = 123
                    ),
                    ConnectionSettings = list(
                      IdleTimeout = 123
                    ),
                    CrossZoneLoadBalancing = list(
                      Enabled = TRUE|FALSE
                    ),
                    AdditionalAttributes = list(
                      list(
                        Key = "string",
                        Value = "string"
                      )
                    )
                  ),
                  LoadBalancerName = "string",
                  Policies = list(
                    AppCookieStickinessPolicies = list(
                      list(
                        CookieName = "string",
                        PolicyName = "string"
                      )
                    ),
                    LbCookieStickinessPolicies = list(
                      list(
                        CookieExpirationPeriod = 123,
                        PolicyName = "string"
                      )
                    ),
                    OtherPolicies = list(
                      "string"
                    )
                  ),
                  Scheme = "string",
                  SecurityGroups = list(
                    "string"
                  ),
                  SourceSecurityGroup = list(
                    GroupName = "string",
                    OwnerAlias = "string"
                  ),
                  Subnets = list(
                    "string"
                  ),
                  VpcId = "string"
                ),
                AwsIamGroup = list(
                  AttachedManagedPolicies = list(
                    list(
                      PolicyName = "string",
                      PolicyArn = "string"
                    )
                  ),
                  CreateDate = "string",
                  GroupId = "string",
                  GroupName = "string",
                  GroupPolicyList = list(
                    list(
                      PolicyName = "string"
                    )
                  ),
                  Path = "string"
                ),
                AwsIamRole = list(
                  AssumeRolePolicyDocument = "string",
                  AttachedManagedPolicies = list(
                    list(
                      PolicyName = "string",
                      PolicyArn = "string"
                    )
                  ),
                  CreateDate = "string",
                  InstanceProfileList = list(
                    list(
                      Arn = "string",
                      CreateDate = "string",
                      InstanceProfileId = "string",
                      InstanceProfileName = "string",
                      Path = "string",
                      Roles = list(
                        list(
                          Arn = "string",
                          AssumeRolePolicyDocument = "string",
                          CreateDate = "string",
                          Path = "string",
                          RoleId = "string",
                          RoleName = "string"
                        )
                      )
                    )
                  ),
                  PermissionsBoundary = list(
                    PermissionsBoundaryArn = "string",
                    PermissionsBoundaryType = "string"
                  ),
                  RoleId = "string",
                  RoleName = "string",
                  RolePolicyList = list(
                    list(
                      PolicyName = "string"
                    )
                  ),
                  MaxSessionDuration = 123,
                  Path = "string"
                ),
                AwsKmsKey = list(
                  AWSAccountId = "string",
                  CreationDate = 123.0,
                  KeyId = "string",
                  KeyManager = "string",
                  KeyState = "string",
                  Origin = "string",
                  Description = "string",
                  KeyRotationStatus = TRUE|FALSE
                ),
                AwsLambdaFunction = list(
                  Code = list(
                    S3Bucket = "string",
                    S3Key = "string",
                    S3ObjectVersion = "string",
                    ZipFile = "string"
                  ),
                  CodeSha256 = "string",
                  DeadLetterConfig = list(
                    TargetArn = "string"
                  ),
                  Environment = list(
                    Variables = list(
                      "string"
                    ),
                    Error = list(
                      ErrorCode = "string",
                      Message = "string"
                    )
                  ),
                  FunctionName = "string",
                  Handler = "string",
                  KmsKeyArn = "string",
                  LastModified = "string",
                  Layers = list(
                    list(
                      Arn = "string",
                      CodeSize = 123
                    )
                  ),
                  MasterArn = "string",
                  MemorySize = 123,
                  RevisionId = "string",
                  Role = "string",
                  Runtime = "string",
                  Timeout = 123,
                  TracingConfig = list(
                    Mode = "string"
                  ),
                  VpcConfig = list(
                    SecurityGroupIds = list(
                      "string"
                    ),
                    SubnetIds = list(
                      "string"
                    ),
                    VpcId = "string"
                  ),
                  Version = "string",
                  Architectures = list(
                    "string"
                  ),
                  PackageType = "string"
                ),
                AwsLambdaLayerVersion = list(
                  Version = 123,
                  CompatibleRuntimes = list(
                    "string"
                  ),
                  CreatedDate = "string"
                ),
                AwsRdsDbInstance = list(
                  AssociatedRoles = list(
                    list(
                      RoleArn = "string",
                      FeatureName = "string",
                      Status = "string"
                    )
                  ),
                  CACertificateIdentifier = "string",
                  DBClusterIdentifier = "string",
                  DBInstanceIdentifier = "string",
                  DBInstanceClass = "string",
                  DbInstancePort = 123,
                  DbiResourceId = "string",
                  DBName = "string",
                  DeletionProtection = TRUE|FALSE,
                  Endpoint = list(
                    Address = "string",
                    Port = 123,
                    HostedZoneId = "string"
                  ),
                  Engine = "string",
                  EngineVersion = "string",
                  IAMDatabaseAuthenticationEnabled = TRUE|FALSE,
                  InstanceCreateTime = "string",
                  KmsKeyId = "string",
                  PubliclyAccessible = TRUE|FALSE,
                  StorageEncrypted = TRUE|FALSE,
                  TdeCredentialArn = "string",
                  VpcSecurityGroups = list(
                    list(
                      VpcSecurityGroupId = "string",
                      Status = "string"
                    )
                  ),
                  MultiAz = TRUE|FALSE,
                  EnhancedMonitoringResourceArn = "string",
                  DbInstanceStatus = "string",
                  MasterUsername = "string",
                  AllocatedStorage = 123,
                  PreferredBackupWindow = "string",
                  BackupRetentionPeriod = 123,
                  DbSecurityGroups = list(
                    "string"
                  ),
                  DbParameterGroups = list(
                    list(
                      DbParameterGroupName = "string",
                      ParameterApplyStatus = "string"
                    )
                  ),
                  AvailabilityZone = "string",
                  DbSubnetGroup = list(
                    DbSubnetGroupName = "string",
                    DbSubnetGroupDescription = "string",
                    VpcId = "string",
                    SubnetGroupStatus = "string",
                    Subnets = list(
                      list(
                        SubnetIdentifier = "string",
                        SubnetAvailabilityZone = list(
                          Name = "string"
                        ),
                        SubnetStatus = "string"
                      )
                    ),
                    DbSubnetGroupArn = "string"
                  ),
                  PreferredMaintenanceWindow = "string",
                  PendingModifiedValues = list(
                    DbInstanceClass = "string",
                    AllocatedStorage = 123,
                    MasterUserPassword = "string",
                    Port = 123,
                    BackupRetentionPeriod = 123,
                    MultiAZ = TRUE|FALSE,
                    EngineVersion = "string",
                    LicenseModel = "string",
                    Iops = 123,
                    DbInstanceIdentifier = "string",
                    StorageType = "string",
                    CaCertificateIdentifier = "string",
                    DbSubnetGroupName = "string",
                    PendingCloudWatchLogsExports = list(
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
                    )
                  ),
                  LatestRestorableTime = "string",
                  AutoMinorVersionUpgrade = TRUE|FALSE,
                  ReadReplicaSourceDBInstanceIdentifier = "string",
                  ReadReplicaDBInstanceIdentifiers = list(
                    "string"
                  ),
                  ReadReplicaDBClusterIdentifiers = list(
                    "string"
                  ),
                  LicenseModel = "string",
                  Iops = 123,
                  OptionGroupMemberships = list(
                    list(
                      OptionGroupName = "string",
                      Status = "string"
                    )
                  ),
                  CharacterSetName = "string",
                  SecondaryAvailabilityZone = "string",
                  StatusInfos = list(
                    list(
                      StatusType = "string",
                      Normal = TRUE|FALSE,
                      Status = "string",
                      Message = "string"
                    )
                  ),
                  StorageType = "string",
                  DomainMemberships = list(
                    list(
                      Domain = "string",
                      Status = "string",
                      Fqdn = "string",
                      IamRoleName = "string"
                    )
                  ),
                  CopyTagsToSnapshot = TRUE|FALSE,
                  MonitoringInterval = 123,
                  MonitoringRoleArn = "string",
                  PromotionTier = 123,
                  Timezone = "string",
                  PerformanceInsightsEnabled = TRUE|FALSE,
                  PerformanceInsightsKmsKeyId = "string",
                  PerformanceInsightsRetentionPeriod = 123,
                  EnabledCloudWatchLogsExports = list(
                    "string"
                  ),
                  ProcessorFeatures = list(
                    list(
                      Name = "string",
                      Value = "string"
                    )
                  ),
                  ListenerEndpoint = list(
                    Address = "string",
                    Port = 123,
                    HostedZoneId = "string"
                  ),
                  MaxAllocatedStorage = 123
                ),
                AwsSnsTopic = list(
                  KmsMasterKeyId = "string",
                  Subscription = list(
                    list(
                      Endpoint = "string",
                      Protocol = "string"
                    )
                  ),
                  TopicName = "string",
                  Owner = "string",
                  SqsSuccessFeedbackRoleArn = "string",
                  SqsFailureFeedbackRoleArn = "string",
                  ApplicationSuccessFeedbackRoleArn = "string",
                  FirehoseSuccessFeedbackRoleArn = "string",
                  FirehoseFailureFeedbackRoleArn = "string",
                  HttpSuccessFeedbackRoleArn = "string",
                  HttpFailureFeedbackRoleArn = "string"
                ),
                AwsSqsQueue = list(
                  KmsDataKeyReusePeriodSeconds = 123,
                  KmsMasterKeyId = "string",
                  QueueName = "string",
                  DeadLetterTargetArn = "string"
                ),
                AwsWafWebAcl = list(
                  Name = "string",
                  DefaultAction = "string",
                  Rules = list(
                    list(
                      Action = list(
                        Type = "string"
                      ),
                      ExcludedRules = list(
                        list(
                          RuleId = "string"
                        )
                      ),
                      OverrideAction = list(
                        Type = "string"
                      ),
                      Priority = 123,
                      RuleId = "string",
                      Type = "string"
                    )
                  ),
                  WebAclId = "string"
                ),
                AwsRdsDbSnapshot = list(
                  DbSnapshotIdentifier = "string",
                  DbInstanceIdentifier = "string",
                  SnapshotCreateTime = "string",
                  Engine = "string",
                  AllocatedStorage = 123,
                  Status = "string",
                  Port = 123,
                  AvailabilityZone = "string",
                  VpcId = "string",
                  InstanceCreateTime = "string",
                  MasterUsername = "string",
                  EngineVersion = "string",
                  LicenseModel = "string",
                  SnapshotType = "string",
                  Iops = 123,
                  OptionGroupName = "string",
                  PercentProgress = 123,
                  SourceRegion = "string",
                  SourceDbSnapshotIdentifier = "string",
                  StorageType = "string",
                  TdeCredentialArn = "string",
                  Encrypted = TRUE|FALSE,
                  KmsKeyId = "string",
                  Timezone = "string",
                  IamDatabaseAuthenticationEnabled = TRUE|FALSE,
                  ProcessorFeatures = list(
                    list(
                      Name = "string",
                      Value = "string"
                    )
                  ),
                  DbiResourceId = "string"
                ),
                AwsRdsDbClusterSnapshot = list(
                  AvailabilityZones = list(
                    "string"
                  ),
                  SnapshotCreateTime = "string",
                  Engine = "string",
                  AllocatedStorage = 123,
                  Status = "string",
                  Port = 123,
                  VpcId = "string",
                  ClusterCreateTime = "string",
                  MasterUsername = "string",
                  EngineVersion = "string",
                  LicenseModel = "string",
                  SnapshotType = "string",
                  PercentProgress = 123,
                  StorageEncrypted = TRUE|FALSE,
                  KmsKeyId = "string",
                  DbClusterIdentifier = "string",
                  DbClusterSnapshotIdentifier = "string",
                  IamDatabaseAuthenticationEnabled = TRUE|FALSE
                ),
                AwsRdsDbCluster = list(
                  AllocatedStorage = 123,
                  AvailabilityZones = list(
                    "string"
                  ),
                  BackupRetentionPeriod = 123,
                  DatabaseName = "string",
                  Status = "string",
                  Endpoint = "string",
                  ReaderEndpoint = "string",
                  CustomEndpoints = list(
                    "string"
                  ),
                  MultiAz = TRUE|FALSE,
                  Engine = "string",
                  EngineVersion = "string",
                  Port = 123,
                  MasterUsername = "string",
                  PreferredBackupWindow = "string",
                  PreferredMaintenanceWindow = "string",
                  ReadReplicaIdentifiers = list(
                    "string"
                  ),
                  VpcSecurityGroups = list(
                    list(
                      VpcSecurityGroupId = "string",
                      Status = "string"
                    )
                  ),
                  HostedZoneId = "string",
                  StorageEncrypted = TRUE|FALSE,
                  KmsKeyId = "string",
                  DbClusterResourceId = "string",
                  AssociatedRoles = list(
                    list(
                      RoleArn = "string",
                      Status = "string"
                    )
                  ),
                  ClusterCreateTime = "string",
                  EnabledCloudWatchLogsExports = list(
                    "string"
                  ),
                  EngineMode = "string",
                  DeletionProtection = TRUE|FALSE,
                  HttpEndpointEnabled = TRUE|FALSE,
                  ActivityStreamStatus = "string",
                  CopyTagsToSnapshot = TRUE|FALSE,
                  CrossAccountClone = TRUE|FALSE,
                  DomainMemberships = list(
                    list(
                      Domain = "string",
                      Status = "string",
                      Fqdn = "string",
                      IamRoleName = "string"
                    )
                  ),
                  DbClusterParameterGroup = "string",
                  DbSubnetGroup = "string",
                  DbClusterOptionGroupMemberships = list(
                    list(
                      DbClusterOptionGroupName = "string",
                      Status = "string"
                    )
                  ),
                  DbClusterIdentifier = "string",
                  DbClusterMembers = list(
                    list(
                      IsClusterWriter = TRUE|FALSE,
                      PromotionTier = 123,
                      DbInstanceIdentifier = "string",
                      DbClusterParameterGroupStatus = "string"
                    )
                  ),
                  IamDatabaseAuthenticationEnabled = TRUE|FALSE
                ),
                AwsEcsCluster = list(
                  ClusterArn = "string",
                  ActiveServicesCount = 123,
                  CapacityProviders = list(
                    "string"
                  ),
                  ClusterSettings = list(
                    list(
                      Name = "string",
                      Value = "string"
                    )
                  ),
                  Configuration = list(
                    ExecuteCommandConfiguration = list(
                      KmsKeyId = "string",
                      LogConfiguration = list(
                        CloudWatchEncryptionEnabled = TRUE|FALSE,
                        CloudWatchLogGroupName = "string",
                        S3BucketName = "string",
                        S3EncryptionEnabled = TRUE|FALSE,
                        S3KeyPrefix = "string"
                      ),
                      Logging = "string"
                    )
                  ),
                  DefaultCapacityProviderStrategy = list(
                    list(
                      Base = 123,
                      CapacityProvider = "string",
                      Weight = 123
                    )
                  ),
                  ClusterName = "string",
                  RegisteredContainerInstancesCount = 123,
                  RunningTasksCount = 123,
                  Status = "string"
                ),
                AwsEcsContainer = list(
                  Name = "string",
                  Image = "string",
                  MountPoints = list(
                    list(
                      SourceVolume = "string",
                      ContainerPath = "string"
                    )
                  ),
                  Privileged = TRUE|FALSE
                ),
                AwsEcsTaskDefinition = list(
                  ContainerDefinitions = list(
                    list(
                      Command = list(
                        "string"
                      ),
                      Cpu = 123,
                      DependsOn = list(
                        list(
                          Condition = "string",
                          ContainerName = "string"
                        )
                      ),
                      DisableNetworking = TRUE|FALSE,
                      DnsSearchDomains = list(
                        "string"
                      ),
                      DnsServers = list(
                        "string"
                      ),
                      DockerLabels = list(
                        "string"
                      ),
                      DockerSecurityOptions = list(
                        "string"
                      ),
                      EntryPoint = list(
                        "string"
                      ),
                      Environment = list(
                        list(
                          Name = "string",
                          Value = "string"
                        )
                      ),
                      EnvironmentFiles = list(
                        list(
                          Type = "string",
                          Value = "string"
                        )
                      ),
                      Essential = TRUE|FALSE,
                      ExtraHosts = list(
                        list(
                          Hostname = "string",
                          IpAddress = "string"
                        )
                      ),
                      FirelensConfiguration = list(
                        Options = list(
                          "string"
                        ),
                        Type = "string"
                      ),
                      HealthCheck = list(
                        Command = list(
                          "string"
                        ),
                        Interval = 123,
                        Retries = 123,
                        StartPeriod = 123,
                        Timeout = 123
                      ),
                      Hostname = "string",
                      Image = "string",
                      Interactive = TRUE|FALSE,
                      Links = list(
                        "string"
                      ),
                      LinuxParameters = list(
                        Capabilities = list(
                          Add = list(
                            "string"
                          ),
                          Drop = list(
                            "string"
                          )
                        ),
                        Devices = list(
                          list(
                            ContainerPath = "string",
                            HostPath = "string",
                            Permissions = list(
                              "string"
                            )
                          )
                        ),
                        InitProcessEnabled = TRUE|FALSE,
                        MaxSwap = 123,
                        SharedMemorySize = 123,
                        Swappiness = 123,
                        Tmpfs = list(
                          list(
                            ContainerPath = "string",
                            MountOptions = list(
                              "string"
                            ),
                            Size = 123
                          )
                        )
                      ),
                      LogConfiguration = list(
                        LogDriver = "string",
                        Options = list(
                          "string"
                        ),
                        SecretOptions = list(
                          list(
                            Name = "string",
                            ValueFrom = "string"
                          )
                        )
                      ),
                      Memory = 123,
                      MemoryReservation = 123,
                      MountPoints = list(
                        list(
                          ContainerPath = "string",
                          ReadOnly = TRUE|FALSE,
                          SourceVolume = "string"
                        )
                      ),
                      Name = "string",
                      PortMappings = list(
                        list(
                          ContainerPort = 123,
                          HostPort = 123,
                          Protocol = "string"
                        )
                      ),
                      Privileged = TRUE|FALSE,
                      PseudoTerminal = TRUE|FALSE,
                      ReadonlyRootFilesystem = TRUE|FALSE,
                      RepositoryCredentials = list(
                        CredentialsParameter = "string"
                      ),
                      ResourceRequirements = list(
                        list(
                          Type = "string",
                          Value = "string"
                        )
                      ),
                      Secrets = list(
                        list(
                          Name = "string",
                          ValueFrom = "string"
                        )
                      ),
                      StartTimeout = 123,
                      StopTimeout = 123,
                      SystemControls = list(
                        list(
                          Namespace = "string",
                          Value = "string"
                        )
                      ),
                      Ulimits = list(
                        list(
                          HardLimit = 123,
                          Name = "string",
                          SoftLimit = 123
                        )
                      ),
                      User = "string",
                      VolumesFrom = list(
                        list(
                          ReadOnly = TRUE|FALSE,
                          SourceContainer = "string"
                        )
                      ),
                      WorkingDirectory = "string"
                    )
                  ),
                  Cpu = "string",
                  ExecutionRoleArn = "string",
                  Family = "string",
                  InferenceAccelerators = list(
                    list(
                      DeviceName = "string",
                      DeviceType = "string"
                    )
                  ),
                  IpcMode = "string",
                  Memory = "string",
                  NetworkMode = "string",
                  PidMode = "string",
                  PlacementConstraints = list(
                    list(
                      Expression = "string",
                      Type = "string"
                    )
                  ),
                  ProxyConfiguration = list(
                    ContainerName = "string",
                    ProxyConfigurationProperties = list(
                      list(
                        Name = "string",
                        Value = "string"
                      )
                    ),
                    Type = "string"
                  ),
                  RequiresCompatibilities = list(
                    "string"
                  ),
                  TaskRoleArn = "string",
                  Volumes = list(
                    list(
                      DockerVolumeConfiguration = list(
                        Autoprovision = TRUE|FALSE,
                        Driver = "string",
                        DriverOpts = list(
                          "string"
                        ),
                        Labels = list(
                          "string"
                        ),
                        Scope = "string"
                      ),
                      EfsVolumeConfiguration = list(
                        AuthorizationConfig = list(
                          AccessPointId = "string",
                          Iam = "string"
                        ),
                        FilesystemId = "string",
                        RootDirectory = "string",
                        TransitEncryption = "string",
                        TransitEncryptionPort = 123
                      ),
                      Host = list(
                        SourcePath = "string"
                      ),
                      Name = "string"
                    )
                  )
                ),
                Container = list(
                  ContainerRuntime = "string",
                  Name = "string",
                  ImageId = "string",
                  ImageName = "string",
                  LaunchedAt = "string",
                  VolumeMounts = list(
                    list(
                      Name = "string",
                      MountPath = "string"
                    )
                  ),
                  Privileged = TRUE|FALSE
                ),
                Other = list(
                  "string"
                ),
                AwsRdsEventSubscription = list(
                  CustSubscriptionId = "string",
                  CustomerAwsId = "string",
                  Enabled = TRUE|FALSE,
                  EventCategoriesList = list(
                    "string"
                  ),
                  EventSubscriptionArn = "string",
                  SnsTopicArn = "string",
                  SourceIdsList = list(
                    "string"
                  ),
                  SourceType = "string",
                  Status = "string",
                  SubscriptionCreationTime = "string"
                ),
                AwsEcsService = list(
                  CapacityProviderStrategy = list(
                    list(
                      Base = 123,
                      CapacityProvider = "string",
                      Weight = 123
                    )
                  ),
                  Cluster = "string",
                  DeploymentConfiguration = list(
                    DeploymentCircuitBreaker = list(
                      Enable = TRUE|FALSE,
                      Rollback = TRUE|FALSE
                    ),
                    MaximumPercent = 123,
                    MinimumHealthyPercent = 123
                  ),
                  DeploymentController = list(
                    Type = "string"
                  ),
                  DesiredCount = 123,
                  EnableEcsManagedTags = TRUE|FALSE,
                  EnableExecuteCommand = TRUE|FALSE,
                  HealthCheckGracePeriodSeconds = 123,
                  LaunchType = "string",
                  LoadBalancers = list(
                    list(
                      ContainerName = "string",
                      ContainerPort = 123,
                      LoadBalancerName = "string",
                      TargetGroupArn = "string"
                    )
                  ),
                  Name = "string",
                  NetworkConfiguration = list(
                    AwsVpcConfiguration = list(
                      AssignPublicIp = "string",
                      SecurityGroups = list(
                        "string"
                      ),
                      Subnets = list(
                        "string"
                      )
                    )
                  ),
                  PlacementConstraints = list(
                    list(
                      Expression = "string",
                      Type = "string"
                    )
                  ),
                  PlacementStrategies = list(
                    list(
                      Field = "string",
                      Type = "string"
                    )
                  ),
                  PlatformVersion = "string",
                  PropagateTags = "string",
                  Role = "string",
                  SchedulingStrategy = "string",
                  ServiceArn = "string",
                  ServiceName = "string",
                  ServiceRegistries = list(
                    list(
                      ContainerName = "string",
                      ContainerPort = 123,
                      Port = 123,
                      RegistryArn = "string"
                    )
                  ),
                  TaskDefinition = "string"
                ),
                AwsAutoScalingLaunchConfiguration = list(
                  AssociatePublicIpAddress = TRUE|FALSE,
                  BlockDeviceMappings = list(
                    list(
                      DeviceName = "string",
                      Ebs = list(
                        DeleteOnTermination = TRUE|FALSE,
                        Encrypted = TRUE|FALSE,
                        Iops = 123,
                        SnapshotId = "string",
                        VolumeSize = 123,
                        VolumeType = "string"
                      ),
                      NoDevice = TRUE|FALSE,
                      VirtualName = "string"
                    )
                  ),
                  ClassicLinkVpcId = "string",
                  ClassicLinkVpcSecurityGroups = list(
                    "string"
                  ),
                  CreatedTime = "string",
                  EbsOptimized = TRUE|FALSE,
                  IamInstanceProfile = "string",
                  ImageId = "string",
                  InstanceMonitoring = list(
                    Enabled = TRUE|FALSE
                  ),
                  InstanceType = "string",
                  KernelId = "string",
                  KeyName = "string",
                  LaunchConfigurationName = "string",
                  PlacementTenancy = "string",
                  RamdiskId = "string",
                  SecurityGroups = list(
                    "string"
                  ),
                  SpotPrice = "string",
                  UserData = "string",
                  MetadataOptions = list(
                    HttpEndpoint = "string",
                    HttpPutResponseHopLimit = 123,
                    HttpTokens = "string"
                  )
                ),
                AwsEc2VpnConnection = list(
                  VpnConnectionId = "string",
                  State = "string",
                  CustomerGatewayId = "string",
                  CustomerGatewayConfiguration = "string",
                  Type = "string",
                  VpnGatewayId = "string",
                  Category = "string",
                  VgwTelemetry = list(
                    list(
                      AcceptedRouteCount = 123,
                      CertificateArn = "string",
                      LastStatusChange = "string",
                      OutsideIpAddress = "string",
                      Status = "string",
                      StatusMessage = "string"
                    )
                  ),
                  Options = list(
                    StaticRoutesOnly = TRUE|FALSE,
                    TunnelOptions = list(
                      list(
                        DpdTimeoutSeconds = 123,
                        IkeVersions = list(
                          "string"
                        ),
                        OutsideIpAddress = "string",
                        Phase1DhGroupNumbers = list(
                          123
                        ),
                        Phase1EncryptionAlgorithms = list(
                          "string"
                        ),
                        Phase1IntegrityAlgorithms = list(
                          "string"
                        ),
                        Phase1LifetimeSeconds = 123,
                        Phase2DhGroupNumbers = list(
                          123
                        ),
                        Phase2EncryptionAlgorithms = list(
                          "string"
                        ),
                        Phase2IntegrityAlgorithms = list(
                          "string"
                        ),
                        Phase2LifetimeSeconds = 123,
                        PreSharedKey = "string",
                        RekeyFuzzPercentage = 123,
                        RekeyMarginTimeSeconds = 123,
                        ReplayWindowSize = 123,
                        TunnelInsideCidr = "string"
                      )
                    )
                  ),
                  Routes = list(
                    list(
                      DestinationCidrBlock = "string",
                      State = "string"
                    )
                  ),
                  TransitGatewayId = "string"
                ),
                AwsEcrContainerImage = list(
                  RegistryId = "string",
                  RepositoryName = "string",
                  Architecture = "string",
                  ImageDigest = "string",
                  ImageTags = list(
                    "string"
                  ),
                  ImagePublishedAt = "string"
                ),
                AwsOpenSearchServiceDomain = list(
                  Arn = "string",
                  AccessPolicies = "string",
                  DomainName = "string",
                  Id = "string",
                  DomainEndpoint = "string",
                  EngineVersion = "string",
                  EncryptionAtRestOptions = list(
                    Enabled = TRUE|FALSE,
                    KmsKeyId = "string"
                  ),
                  NodeToNodeEncryptionOptions = list(
                    Enabled = TRUE|FALSE
                  ),
                  ServiceSoftwareOptions = list(
                    AutomatedUpdateDate = "string",
                    Cancellable = TRUE|FALSE,
                    CurrentVersion = "string",
                    Description = "string",
                    NewVersion = "string",
                    UpdateAvailable = TRUE|FALSE,
                    UpdateStatus = "string",
                    OptionalDeployment = TRUE|FALSE
                  ),
                  ClusterConfig = list(
                    InstanceCount = 123,
                    WarmEnabled = TRUE|FALSE,
                    WarmCount = 123,
                    DedicatedMasterEnabled = TRUE|FALSE,
                    ZoneAwarenessConfig = list(
                      AvailabilityZoneCount = 123
                    ),
                    DedicatedMasterCount = 123,
                    InstanceType = "string",
                    WarmType = "string",
                    ZoneAwarenessEnabled = TRUE|FALSE,
                    DedicatedMasterType = "string"
                  ),
                  DomainEndpointOptions = list(
                    CustomEndpointCertificateArn = "string",
                    CustomEndpointEnabled = TRUE|FALSE,
                    EnforceHTTPS = TRUE|FALSE,
                    CustomEndpoint = "string",
                    TLSSecurityPolicy = "string"
                  ),
                  VpcOptions = list(
                    SecurityGroupIds = list(
                      "string"
                    ),
                    SubnetIds = list(
                      "string"
                    )
                  ),
                  LogPublishingOptions = list(
                    IndexSlowLogs = list(
                      CloudWatchLogsLogGroupArn = "string",
                      Enabled = TRUE|FALSE
                    ),
                    SearchSlowLogs = list(
                      CloudWatchLogsLogGroupArn = "string",
                      Enabled = TRUE|FALSE
                    ),
                    AuditLogs = list(
                      CloudWatchLogsLogGroupArn = "string",
                      Enabled = TRUE|FALSE
                    )
                  ),
                  DomainEndpoints = list(
                    "string"
                  ),
                  AdvancedSecurityOptions = list(
                    Enabled = TRUE|FALSE,
                    InternalUserDatabaseEnabled = TRUE|FALSE,
                    MasterUserOptions = list(
                      MasterUserArn = "string",
                      MasterUserName = "string",
                      MasterUserPassword = "string"
                    )
                  )
                ),
                AwsEc2VpcEndpointService = list(
                  AcceptanceRequired = TRUE|FALSE,
                  AvailabilityZones = list(
                    "string"
                  ),
                  BaseEndpointDnsNames = list(
                    "string"
                  ),
                  ManagesVpcEndpoints = TRUE|FALSE,
                  GatewayLoadBalancerArns = list(
                    "string"
                  ),
                  NetworkLoadBalancerArns = list(
                    "string"
                  ),
                  PrivateDnsName = "string",
                  ServiceId = "string",
                  ServiceName = "string",
                  ServiceState = "string",
                  ServiceType = list(
                    list(
                      ServiceType = "string"
                    )
                  )
                ),
                AwsXrayEncryptionConfig = list(
                  KeyId = "string",
                  Status = "string",
                  Type = "string"
                ),
                AwsWafRateBasedRule = list(
                  MetricName = "string",
                  Name = "string",
                  RateKey = "string",
                  RateLimit = 123,
                  RuleId = "string",
                  MatchPredicates = list(
                    list(
                      DataId = "string",
                      Negated = TRUE|FALSE,
                      Type = "string"
                    )
                  )
                ),
                AwsWafRegionalRateBasedRule = list(
                  MetricName = "string",
                  Name = "string",
                  RateKey = "string",
                  RateLimit = 123,
                  RuleId = "string",
                  MatchPredicates = list(
                    list(
                      DataId = "string",
                      Negated = TRUE|FALSE,
                      Type = "string"
                    )
                  )
                ),
                AwsEcrRepository = list(
                  Arn = "string",
                  ImageScanningConfiguration = list(
                    ScanOnPush = TRUE|FALSE
                  ),
                  ImageTagMutability = "string",
                  LifecyclePolicy = list(
                    LifecyclePolicyText = "string",
                    RegistryId = "string"
                  ),
                  RepositoryName = "string",
                  RepositoryPolicyText = "string"
                ),
                AwsEksCluster = list(
                  Arn = "string",
                  CertificateAuthorityData = "string",
                  ClusterStatus = "string",
                  Endpoint = "string",
                  Name = "string",
                  ResourcesVpcConfig = list(
                    SecurityGroupIds = list(
                      "string"
                    ),
                    SubnetIds = list(
                      "string"
                    ),
                    EndpointPublicAccess = TRUE|FALSE
                  ),
                  RoleArn = "string",
                  Version = "string",
                  Logging = list(
                    ClusterLogging = list(
                      list(
                        Enabled = TRUE|FALSE,
                        Types = list(
                          "string"
                        )
                      )
                    )
                  )
                ),
                AwsNetworkFirewallFirewallPolicy = list(
                  FirewallPolicy = list(
                    StatefulRuleGroupReferences = list(
                      list(
                        ResourceArn = "string"
                      )
                    ),
                    StatelessCustomActions = list(
                      list(
                        ActionDefinition = list(
                          PublishMetricAction = list(
                            Dimensions = list(
                              list(
                                Value = "string"
                              )
                            )
                          )
                        ),
                        ActionName = "string"
                      )
                    ),
                    StatelessDefaultActions = list(
                      "string"
                    ),
                    StatelessFragmentDefaultActions = list(
                      "string"
                    ),
                    StatelessRuleGroupReferences = list(
                      list(
                        Priority = 123,
                        ResourceArn = "string"
                      )
                    )
                  ),
                  FirewallPolicyArn = "string",
                  FirewallPolicyId = "string",
                  FirewallPolicyName = "string",
                  Description = "string"
                ),
                AwsNetworkFirewallFirewall = list(
                  DeleteProtection = TRUE|FALSE,
                  Description = "string",
                  FirewallArn = "string",
                  FirewallId = "string",
                  FirewallName = "string",
                  FirewallPolicyArn = "string",
                  FirewallPolicyChangeProtection = TRUE|FALSE,
                  SubnetChangeProtection = TRUE|FALSE,
                  SubnetMappings = list(
                    list(
                      SubnetId = "string"
                    )
                  ),
                  VpcId = "string"
                ),
                AwsNetworkFirewallRuleGroup = list(
                  Capacity = 123,
                  Description = "string",
                  RuleGroup = list(
                    RuleVariables = list(
                      IpSets = list(
                        Definition = list(
                          "string"
                        )
                      ),
                      PortSets = list(
                        Definition = list(
                          "string"
                        )
                      )
                    ),
                    RulesSource = list(
                      RulesSourceList = list(
                        GeneratedRulesType = "string",
                        TargetTypes = list(
                          "string"
                        ),
                        Targets = list(
                          "string"
                        )
                      ),
                      RulesString = "string",
                      StatefulRules = list(
                        list(
                          Action = "string",
                          Header = list(
                            Destination = "string",
                            DestinationPort = "string",
                            Direction = "string",
                            Protocol = "string",
                            Source = "string",
                            SourcePort = "string"
                          ),
                          RuleOptions = list(
                            list(
                              Keyword = "string",
                              Settings = list(
                                "string"
                              )
                            )
                          )
                        )
                      ),
                      StatelessRulesAndCustomActions = list(
                        CustomActions = list(
                          list(
                            ActionDefinition = list(
                              PublishMetricAction = list(
                                Dimensions = list(
                                  list(
                                    Value = "string"
                                  )
                                )
                              )
                            ),
                            ActionName = "string"
                          )
                        ),
                        StatelessRules = list(
                          list(
                            Priority = 123,
                            RuleDefinition = list(
                              Actions = list(
                                "string"
                              ),
                              MatchAttributes = list(
                                DestinationPorts = list(
                                  list(
                                    FromPort = 123,
                                    ToPort = 123
                                  )
                                ),
                                Destinations = list(
                                  list(
                                    AddressDefinition = "string"
                                  )
                                ),
                                Protocols = list(
                                  123
                                ),
                                SourcePorts = list(
                                  list(
                                    FromPort = 123,
                                    ToPort = 123
                                  )
                                ),
                                Sources = list(
                                  list(
                                    AddressDefinition = "string"
                                  )
                                ),
                                TcpFlags = list(
                                  list(
                                    Flags = list(
                                      "string"
                                    ),
                                    Masks = list(
                                      "string"
                                    )
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  ),
                  RuleGroupArn = "string",
                  RuleGroupId = "string",
                  RuleGroupName = "string",
                  Type = "string"
                ),
                AwsRdsDbSecurityGroup = list(
                  DbSecurityGroupArn = "string",
                  DbSecurityGroupDescription = "string",
                  DbSecurityGroupName = "string",
                  Ec2SecurityGroups = list(
                    list(
                      Ec2SecurityGroupId = "string",
                      Ec2SecurityGroupName = "string",
                      Ec2SecurityGroupOwnerId = "string",
                      Status = "string"
                    )
                  ),
                  IpRanges = list(
                    list(
                      CidrIp = "string",
                      Status = "string"
                    )
                  ),
                  OwnerId = "string",
                  VpcId = "string"
                ),
                AwsKinesisStream = list(
                  Name = "string",
                  Arn = "string",
                  StreamEncryption = list(
                    EncryptionType = "string",
                    KeyId = "string"
                  ),
                  ShardCount = 123,
                  RetentionPeriodHours = 123
                ),
                AwsEc2TransitGateway = list(
                  Id = "string",
                  Description = "string",
                  DefaultRouteTablePropagation = "string",
                  AutoAcceptSharedAttachments = "string",
                  DefaultRouteTableAssociation = "string",
                  TransitGatewayCidrBlocks = list(
                    "string"
                  ),
                  AssociationDefaultRouteTableId = "string",
                  PropagationDefaultRouteTableId = "string",
                  VpnEcmpSupport = "string",
                  DnsSupport = "string",
                  MulticastSupport = "string",
                  AmazonSideAsn = 123
                ),
                AwsEfsAccessPoint = list(
                  AccessPointId = "string",
                  Arn = "string",
                  ClientToken = "string",
                  FileSystemId = "string",
                  PosixUser = list(
                    Gid = "string",
                    SecondaryGids = list(
                      "string"
                    ),
                    Uid = "string"
                  ),
                  RootDirectory = list(
                    CreationInfo = list(
                      OwnerGid = "string",
                      OwnerUid = "string",
                      Permissions = "string"
                    ),
                    Path = "string"
                  )
                ),
                AwsCloudFormationStack = list(
                  Capabilities = list(
                    "string"
                  ),
                  CreationTime = "string",
                  Description = "string",
                  DisableRollback = TRUE|FALSE,
                  DriftInformation = list(
                    StackDriftStatus = "string"
                  ),
                  EnableTerminationProtection = TRUE|FALSE,
                  LastUpdatedTime = "string",
                  NotificationArns = list(
                    "string"
                  ),
                  Outputs = list(
                    list(
                      Description = "string",
                      OutputKey = "string",
                      OutputValue = "string"
                    )
                  ),
                  RoleArn = "string",
                  StackId = "string",
                  StackName = "string",
                  StackStatus = "string",
                  StackStatusReason = "string",
                  TimeoutInMinutes = 123
                ),
                AwsCloudWatchAlarm = list(
                  ActionsEnabled = TRUE|FALSE,
                  AlarmActions = list(
                    "string"
                  ),
                  AlarmArn = "string",
                  AlarmConfigurationUpdatedTimestamp = "string",
                  AlarmDescription = "string",
                  AlarmName = "string",
                  ComparisonOperator = "string",
                  DatapointsToAlarm = 123,
                  Dimensions = list(
                    list(
                      Name = "string",
                      Value = "string"
                    )
                  ),
                  EvaluateLowSampleCountPercentile = "string",
                  EvaluationPeriods = 123,
                  ExtendedStatistic = "string",
                  InsufficientDataActions = list(
                    "string"
                  ),
                  MetricName = "string",
                  Namespace = "string",
                  OkActions = list(
                    "string"
                  ),
                  Period = 123,
                  Statistic = "string",
                  Threshold = 123.0,
                  ThresholdMetricId = "string",
                  TreatMissingData = "string",
                  Unit = "string"
                ),
                AwsEc2VpcPeeringConnection = list(
                  AccepterVpcInfo = list(
                    CidrBlock = "string",
                    CidrBlockSet = list(
                      list(
                        CidrBlock = "string"
                      )
                    ),
                    Ipv6CidrBlockSet = list(
                      list(
                        Ipv6CidrBlock = "string"
                      )
                    ),
                    OwnerId = "string",
                    PeeringOptions = list(
                      AllowDnsResolutionFromRemoteVpc = TRUE|FALSE,
                      AllowEgressFromLocalClassicLinkToRemoteVpc = TRUE|FALSE,
                      AllowEgressFromLocalVpcToRemoteClassicLink = TRUE|FALSE
                    ),
                    Region = "string",
                    VpcId = "string"
                  ),
                  ExpirationTime = "string",
                  RequesterVpcInfo = list(
                    CidrBlock = "string",
                    CidrBlockSet = list(
                      list(
                        CidrBlock = "string"
                      )
                    ),
                    Ipv6CidrBlockSet = list(
                      list(
                        Ipv6CidrBlock = "string"
                      )
                    ),
                    OwnerId = "string",
                    PeeringOptions = list(
                      AllowDnsResolutionFromRemoteVpc = TRUE|FALSE,
                      AllowEgressFromLocalClassicLinkToRemoteVpc = TRUE|FALSE,
                      AllowEgressFromLocalVpcToRemoteClassicLink = TRUE|FALSE
                    ),
                    Region = "string",
                    VpcId = "string"
                  ),
                  Status = list(
                    Code = "string",
                    Message = "string"
                  ),
                  VpcPeeringConnectionId = "string"
                ),
                AwsWafRegionalRuleGroup = list(
                  MetricName = "string",
                  Name = "string",
                  RuleGroupId = "string",
                  Rules = list(
                    list(
                      Action = list(
                        Type = "string"
                      ),
                      Priority = 123,
                      RuleId = "string",
                      Type = "string"
                    )
                  )
                ),
                AwsWafRegionalRule = list(
                  MetricName = "string",
                  Name = "string",
                  PredicateList = list(
                    list(
                      DataId = "string",
                      Negated = TRUE|FALSE,
                      Type = "string"
                    )
                  ),
                  RuleId = "string"
                ),
                AwsWafRegionalWebAcl = list(
                  DefaultAction = "string",
                  MetricName = "string",
                  Name = "string",
                  RulesList = list(
                    list(
                      Action = list(
                        Type = "string"
                      ),
                      OverrideAction = list(
                        Type = "string"
                      ),
                      Priority = 123,
                      RuleId = "string",
                      Type = "string"
                    )
                  ),
                  WebAclId = "string"
                ),
                AwsWafRule = list(
                  MetricName = "string",
                  Name = "string",
                  PredicateList = list(
                    list(
                      DataId = "string",
                      Negated = TRUE|FALSE,
                      Type = "string"
                    )
                  ),
                  RuleId = "string"
                ),
                AwsWafRuleGroup = list(
                  MetricName = "string",
                  Name = "string",
                  RuleGroupId = "string",
                  Rules = list(
                    list(
                      Action = list(
                        Type = "string"
                      ),
                      Priority = 123,
                      RuleId = "string",
                      Type = "string"
                    )
                  )
                ),
                AwsEcsTask = list(
                  ClusterArn = "string",
                  TaskDefinitionArn = "string",
                  Version = "string",
                  CreatedAt = "string",
                  StartedAt = "string",
                  StartedBy = "string",
                  Group = "string",
                  Volumes = list(
                    list(
                      Name = "string",
                      Host = list(
                        SourcePath = "string"
                      )
                    )
                  ),
                  Containers = list(
                    list(
                      Name = "string",
                      Image = "string",
                      MountPoints = list(
                        list(
                          SourceVolume = "string",
                          ContainerPath = "string"
                        )
                      ),
                      Privileged = TRUE|FALSE
                    )
                  )
                ),
                AwsBackupBackupVault = list(
                  BackupVaultArn = "string",
                  BackupVaultName = "string",
                  EncryptionKeyArn = "string",
                  Notifications = list(
                    BackupVaultEvents = list(
                      "string"
                    ),
                    SnsTopicArn = "string"
                  ),
                  AccessPolicy = "string"
                ),
                AwsBackupBackupPlan = list(
                  BackupPlan = list(
                    BackupPlanName = "string",
                    AdvancedBackupSettings = list(
                      list(
                        BackupOptions = list(
                          "string"
                        ),
                        ResourceType = "string"
                      )
                    ),
                    BackupPlanRule = list(
                      list(
                        TargetBackupVault = "string",
                        StartWindowMinutes = 123,
                        ScheduleExpression = "string",
                        RuleName = "string",
                        RuleId = "string",
                        EnableContinuousBackup = TRUE|FALSE,
                        CompletionWindowMinutes = 123,
                        CopyActions = list(
                          list(
                            DestinationBackupVaultArn = "string",
                            Lifecycle = list(
                              DeleteAfterDays = 123,
                              MoveToColdStorageAfterDays = 123
                            )
                          )
                        ),
                        Lifecycle = list(
                          DeleteAfterDays = 123,
                          MoveToColdStorageAfterDays = 123
                        )
                      )
                    )
                  ),
                  BackupPlanArn = "string",
                  BackupPlanId = "string",
                  VersionId = "string"
                ),
                AwsBackupRecoveryPoint = list(
                  BackupSizeInBytes = 123,
                  BackupVaultArn = "string",
                  BackupVaultName = "string",
                  CalculatedLifecycle = list(
                    DeleteAt = "string",
                    MoveToColdStorageAt = "string"
                  ),
                  CompletionDate = "string",
                  CreatedBy = list(
                    BackupPlanArn = "string",
                    BackupPlanId = "string",
                    BackupPlanVersion = "string",
                    BackupRuleId = "string"
                  ),
                  CreationDate = "string",
                  EncryptionKeyArn = "string",
                  IamRoleArn = "string",
                  IsEncrypted = TRUE|FALSE,
                  LastRestoreTime = "string",
                  Lifecycle = list(
                    DeleteAfterDays = 123,
                    MoveToColdStorageAfterDays = 123
                  ),
                  RecoveryPointArn = "string",
                  ResourceArn = "string",
                  ResourceType = "string",
                  SourceBackupVaultArn = "string",
                  Status = "string",
                  StatusMessage = "string",
                  StorageClass = "string"
                ),
                AwsEc2LaunchTemplate = list(
                  LaunchTemplateName = "string",
                  Id = "string",
                  LaunchTemplateData = list(
                    BlockDeviceMappingSet = list(
                      list(
                        DeviceName = "string",
                        Ebs = list(
                          DeleteOnTermination = TRUE|FALSE,
                          Encrypted = TRUE|FALSE,
                          Iops = 123,
                          KmsKeyId = "string",
                          SnapshotId = "string",
                          Throughput = 123,
                          VolumeSize = 123,
                          VolumeType = "string"
                        ),
                        NoDevice = "string",
                        VirtualName = "string"
                      )
                    ),
                    CapacityReservationSpecification = list(
                      CapacityReservationPreference = "string",
                      CapacityReservationTarget = list(
                        CapacityReservationId = "string",
                        CapacityReservationResourceGroupArn = "string"
                      )
                    ),
                    CpuOptions = list(
                      CoreCount = 123,
                      ThreadsPerCore = 123
                    ),
                    CreditSpecification = list(
                      CpuCredits = "string"
                    ),
                    DisableApiStop = TRUE|FALSE,
                    DisableApiTermination = TRUE|FALSE,
                    EbsOptimized = TRUE|FALSE,
                    ElasticGpuSpecificationSet = list(
                      list(
                        Type = "string"
                      )
                    ),
                    ElasticInferenceAcceleratorSet = list(
                      list(
                        Count = 123,
                        Type = "string"
                      )
                    ),
                    EnclaveOptions = list(
                      Enabled = TRUE|FALSE
                    ),
                    HibernationOptions = list(
                      Configured = TRUE|FALSE
                    ),
                    IamInstanceProfile = list(
                      Arn = "string",
                      Name = "string"
                    ),
                    ImageId = "string",
                    InstanceInitiatedShutdownBehavior = "string",
                    InstanceMarketOptions = list(
                      MarketType = "string",
                      SpotOptions = list(
                        BlockDurationMinutes = 123,
                        InstanceInterruptionBehavior = "string",
                        MaxPrice = "string",
                        SpotInstanceType = "string",
                        ValidUntil = "string"
                      )
                    ),
                    InstanceRequirements = list(
                      AcceleratorCount = list(
                        Max = 123,
                        Min = 123
                      ),
                      AcceleratorManufacturers = list(
                        "string"
                      ),
                      AcceleratorNames = list(
                        "string"
                      ),
                      AcceleratorTotalMemoryMiB = list(
                        Max = 123,
                        Min = 123
                      ),
                      AcceleratorTypes = list(
                        "string"
                      ),
                      BareMetal = "string",
                      BaselineEbsBandwidthMbps = list(
                        Max = 123,
                        Min = 123
                      ),
                      BurstablePerformance = "string",
                      CpuManufacturers = list(
                        "string"
                      ),
                      ExcludedInstanceTypes = list(
                        "string"
                      ),
                      InstanceGenerations = list(
                        "string"
                      ),
                      LocalStorage = "string",
                      LocalStorageTypes = list(
                        "string"
                      ),
                      MemoryGiBPerVCpu = list(
                        Max = 123.0,
                        Min = 123.0
                      ),
                      MemoryMiB = list(
                        Max = 123,
                        Min = 123
                      ),
                      NetworkInterfaceCount = list(
                        Max = 123,
                        Min = 123
                      ),
                      OnDemandMaxPricePercentageOverLowestPrice = 123,
                      RequireHibernateSupport = TRUE|FALSE,
                      SpotMaxPricePercentageOverLowestPrice = 123,
                      TotalLocalStorageGB = list(
                        Max = 123.0,
                        Min = 123.0
                      ),
                      VCpuCount = list(
                        Max = 123,
                        Min = 123
                      )
                    ),
                    InstanceType = "string",
                    KernelId = "string",
                    KeyName = "string",
                    LicenseSet = list(
                      list(
                        LicenseConfigurationArn = "string"
                      )
                    ),
                    MaintenanceOptions = list(
                      AutoRecovery = "string"
                    ),
                    MetadataOptions = list(
                      HttpEndpoint = "string",
                      HttpProtocolIpv6 = "string",
                      HttpTokens = "string",
                      HttpPutResponseHopLimit = 123,
                      InstanceMetadataTags = "string"
                    ),
                    Monitoring = list(
                      Enabled = TRUE|FALSE
                    ),
                    NetworkInterfaceSet = list(
                      list(
                        AssociateCarrierIpAddress = TRUE|FALSE,
                        AssociatePublicIpAddress = TRUE|FALSE,
                        DeleteOnTermination = TRUE|FALSE,
                        Description = "string",
                        DeviceIndex = 123,
                        Groups = list(
                          "string"
                        ),
                        InterfaceType = "string",
                        Ipv4PrefixCount = 123,
                        Ipv4Prefixes = list(
                          list(
                            Ipv4Prefix = "string"
                          )
                        ),
                        Ipv6AddressCount = 123,
                        Ipv6Addresses = list(
                          list(
                            Ipv6Address = "string"
                          )
                        ),
                        Ipv6PrefixCount = 123,
                        Ipv6Prefixes = list(
                          list(
                            Ipv6Prefix = "string"
                          )
                        ),
                        NetworkCardIndex = 123,
                        NetworkInterfaceId = "string",
                        PrivateIpAddress = "string",
                        PrivateIpAddresses = list(
                          list(
                            Primary = TRUE|FALSE,
                            PrivateIpAddress = "string"
                          )
                        ),
                        SecondaryPrivateIpAddressCount = 123,
                        SubnetId = "string"
                      )
                    ),
                    Placement = list(
                      Affinity = "string",
                      AvailabilityZone = "string",
                      GroupName = "string",
                      HostId = "string",
                      HostResourceGroupArn = "string",
                      PartitionNumber = 123,
                      SpreadDomain = "string",
                      Tenancy = "string"
                    ),
                    PrivateDnsNameOptions = list(
                      EnableResourceNameDnsAAAARecord = TRUE|FALSE,
                      EnableResourceNameDnsARecord = TRUE|FALSE,
                      HostnameType = "string"
                    ),
                    RamDiskId = "string",
                    SecurityGroupIdSet = list(
                      "string"
                    ),
                    SecurityGroupSet = list(
                      "string"
                    ),
                    UserData = "string"
                  ),
                  DefaultVersionNumber = 123,
                  LatestVersionNumber = 123
                ),
                AwsSageMakerNotebookInstance = list(
                  AcceleratorTypes = list(
                    "string"
                  ),
                  AdditionalCodeRepositories = list(
                    "string"
                  ),
                  DefaultCodeRepository = "string",
                  DirectInternetAccess = "string",
                  FailureReason = "string",
                  InstanceMetadataServiceConfiguration = list(
                    MinimumInstanceMetadataServiceVersion = "string"
                  ),
                  InstanceType = "string",
                  KmsKeyId = "string",
                  NetworkInterfaceId = "string",
                  NotebookInstanceArn = "string",
                  NotebookInstanceLifecycleConfigName = "string",
                  NotebookInstanceName = "string",
                  NotebookInstanceStatus = "string",
                  PlatformIdentifier = "string",
                  RoleArn = "string",
                  RootAccess = "string",
                  SecurityGroups = list(
                    "string"
                  ),
                  SubnetId = "string",
                  Url = "string",
                  VolumeSizeInGB = 123
                ),
                AwsWafv2WebAcl = list(
                  Name = "string",
                  Arn = "string",
                  ManagedbyFirewallManager = TRUE|FALSE,
                  Id = "string",
                  Capacity = 123,
                  CaptchaConfig = list(
                    ImmunityTimeProperty = list(
                      ImmunityTime = 123
                    )
                  ),
                  DefaultAction = list(
                    Allow = list(
                      CustomRequestHandling = list(
                        InsertHeaders = list(
                          list(
                            Name = "string",
                            Value = "string"
                          )
                        )
                      )
                    ),
                    Block = list(
                      CustomResponse = list(
                        CustomResponseBodyKey = "string",
                        ResponseCode = 123,
                        ResponseHeaders = list(
                          list(
                            Name = "string",
                            Value = "string"
                          )
                        )
                      )
                    )
                  ),
                  Description = "string",
                  Rules = list(
                    list(
                      Action = list(
                        Allow = list(
                          CustomRequestHandling = list(
                            InsertHeaders = list(
                              list(
                                Name = "string",
                                Value = "string"
                              )
                            )
                          )
                        ),
                        Block = list(
                          CustomResponse = list(
                            CustomResponseBodyKey = "string",
                            ResponseCode = 123,
                            ResponseHeaders = list(
                              list(
                                Name = "string",
                                Value = "string"
                              )
                            )
                          )
                        ),
                        Captcha = list(
                          CustomRequestHandling = list(
                            InsertHeaders = list(
                              list(
                                Name = "string",
                                Value = "string"
                              )
                            )
                          )
                        ),
                        Count = list(
                          CustomRequestHandling = list(
                            InsertHeaders = list(
                              list(
                                Name = "string",
                                Value = "string"
                              )
                            )
                          )
                        )
                      ),
                      Name = "string",
                      OverrideAction = "string",
                      Priority = 123,
                      VisibilityConfig = list(
                        CloudWatchMetricsEnabled = TRUE|FALSE,
                        MetricName = "string",
                        SampledRequestsEnabled = TRUE|FALSE
                      )
                    )
                  ),
                  VisibilityConfig = list(
                    CloudWatchMetricsEnabled = TRUE|FALSE,
                    MetricName = "string",
                    SampledRequestsEnabled = TRUE|FALSE
                  )
                ),
                AwsWafv2RuleGroup = list(
                  Capacity = 123,
                  Description = "string",
                  Id = "string",
                  Name = "string",
                  Arn = "string",
                  Rules = list(
                    list(
                      Action = list(
                        Allow = list(
                          CustomRequestHandling = list(
                            InsertHeaders = list(
                              list(
                                Name = "string",
                                Value = "string"
                              )
                            )
                          )
                        ),
                        Block = list(
                          CustomResponse = list(
                            CustomResponseBodyKey = "string",
                            ResponseCode = 123,
                            ResponseHeaders = list(
                              list(
                                Name = "string",
                                Value = "string"
                              )
                            )
                          )
                        ),
                        Captcha = list(
                          CustomRequestHandling = list(
                            InsertHeaders = list(
                              list(
                                Name = "string",
                                Value = "string"
                              )
                            )
                          )
                        ),
                        Count = list(
                          CustomRequestHandling = list(
                            InsertHeaders = list(
                              list(
                                Name = "string",
                                Value = "string"
                              )
                            )
                          )
                        )
                      ),
                      Name = "string",
                      OverrideAction = "string",
                      Priority = 123,
                      VisibilityConfig = list(
                        CloudWatchMetricsEnabled = TRUE|FALSE,
                        MetricName = "string",
                        SampledRequestsEnabled = TRUE|FALSE
                      )
                    )
                  ),
                  Scope = "string",
                  VisibilityConfig = list(
                    CloudWatchMetricsEnabled = TRUE|FALSE,
                    MetricName = "string",
                    SampledRequestsEnabled = TRUE|FALSE
                  )
                ),
                AwsEc2RouteTable = list(
                  AssociationSet = list(
                    list(
                      AssociationState = list(
                        State = "string",
                        StatusMessage = "string"
                      ),
                      GatewayId = "string",
                      Main = TRUE|FALSE,
                      RouteTableAssociationId = "string",
                      RouteTableId = "string",
                      SubnetId = "string"
                    )
                  ),
                  OwnerId = "string",
                  PropagatingVgwSet = list(
                    list(
                      GatewayId = "string"
                    )
                  ),
                  RouteTableId = "string",
                  RouteSet = list(
                    list(
                      CarrierGatewayId = "string",
                      CoreNetworkArn = "string",
                      DestinationCidrBlock = "string",
                      DestinationIpv6CidrBlock = "string",
                      DestinationPrefixListId = "string",
                      EgressOnlyInternetGatewayId = "string",
                      GatewayId = "string",
                      InstanceId = "string",
                      InstanceOwnerId = "string",
                      LocalGatewayId = "string",
                      NatGatewayId = "string",
                      NetworkInterfaceId = "string",
                      Origin = "string",
                      State = "string",
                      TransitGatewayId = "string",
                      VpcPeeringConnectionId = "string"
                    )
                  ),
                  VpcId = "string"
                )
              )
            )
          ),
          Compliance = list(
            Status = "PASSED"|"WARNING"|"FAILED"|"NOT_AVAILABLE",
            RelatedRequirements = list(
              "string"
            ),
            StatusReasons = list(
              list(
                ReasonCode = "string",
                Description = "string"
              )
            ),
            SecurityControlId = "string",
            AssociatedStandards = list(
              list(
                StandardsId = "string"
              )
            )
          ),
          VerificationState = "UNKNOWN"|"TRUE_POSITIVE"|"FALSE_POSITIVE"|"BENIGN_POSITIVE",
          WorkflowState = "NEW"|"ASSIGNED"|"IN_PROGRESS"|"DEFERRED"|"RESOLVED",
          Workflow = list(
            Status = "NEW"|"NOTIFIED"|"RESOLVED"|"SUPPRESSED"
          ),
          RecordState = "ACTIVE"|"ARCHIVED",
          RelatedFindings = list(
            list(
              ProductArn = "string",
              Id = "string"
            )
          ),
          Note = list(
            Text = "string",
            UpdatedBy = "string",
            UpdatedAt = "string"
          ),
          Vulnerabilities = list(
            list(
              Id = "string",
              VulnerablePackages = list(
                list(
                  Name = "string",
                  Version = "string",
                  Epoch = "string",
                  Release = "string",
                  Architecture = "string",
                  PackageManager = "string",
                  FilePath = "string",
                  FixedInVersion = "string",
                  Remediation = "string",
                  SourceLayerHash = "string",
                  SourceLayerArn = "string"
                )
              ),
              Cvss = list(
                list(
                  Version = "string",
                  BaseScore = 123.0,
                  BaseVector = "string",
                  Source = "string",
                  Adjustments = list(
                    list(
                      Metric = "string",
                      Reason = "string"
                    )
                  )
                )
              ),
              RelatedVulnerabilities = list(
                "string"
              ),
              Vendor = list(
                Name = "string",
                Url = "string",
                VendorSeverity = "string",
                VendorCreatedAt = "string",
                VendorUpdatedAt = "string"
              ),
              ReferenceUrls = list(
                "string"
              ),
              FixAvailable = "YES"|"NO"|"PARTIAL"
            )
          ),
          PatchSummary = list(
            Id = "string",
            InstalledCount = 123,
            MissingCount = 123,
            FailedCount = 123,
            InstalledOtherCount = 123,
            InstalledRejectedCount = 123,
            InstalledPendingReboot = 123,
            OperationStartTime = "string",
            OperationEndTime = "string",
            RebootOption = "string",
            Operation = "string"
          ),
          Action = list(
            ActionType = "string",
            NetworkConnectionAction = list(
              ConnectionDirection = "string",
              RemoteIpDetails = list(
                IpAddressV4 = "string",
                Organization = list(
                  Asn = 123,
                  AsnOrg = "string",
                  Isp = "string",
                  Org = "string"
                ),
                Country = list(
                  CountryCode = "string",
                  CountryName = "string"
                ),
                City = list(
                  CityName = "string"
                ),
                GeoLocation = list(
                  Lon = 123.0,
                  Lat = 123.0
                )
              ),
              RemotePortDetails = list(
                Port = 123,
                PortName = "string"
              ),
              LocalPortDetails = list(
                Port = 123,
                PortName = "string"
              ),
              Protocol = "string",
              Blocked = TRUE|FALSE
            ),
            AwsApiCallAction = list(
              Api = "string",
              ServiceName = "string",
              CallerType = "string",
              RemoteIpDetails = list(
                IpAddressV4 = "string",
                Organization = list(
                  Asn = 123,
                  AsnOrg = "string",
                  Isp = "string",
                  Org = "string"
                ),
                Country = list(
                  CountryCode = "string",
                  CountryName = "string"
                ),
                City = list(
                  CityName = "string"
                ),
                GeoLocation = list(
                  Lon = 123.0,
                  Lat = 123.0
                )
              ),
              DomainDetails = list(
                Domain = "string"
              ),
              AffectedResources = list(
                "string"
              ),
              FirstSeen = "string",
              LastSeen = "string"
            ),
            DnsRequestAction = list(
              Domain = "string",
              Protocol = "string",
              Blocked = TRUE|FALSE
            ),
            PortProbeAction = list(
              PortProbeDetails = list(
                list(
                  LocalPortDetails = list(
                    Port = 123,
                    PortName = "string"
                  ),
                  LocalIpDetails = list(
                    IpAddressV4 = "string"
                  ),
                  RemoteIpDetails = list(
                    IpAddressV4 = "string",
                    Organization = list(
                      Asn = 123,
                      AsnOrg = "string",
                      Isp = "string",
                      Org = "string"
                    ),
                    Country = list(
                      CountryCode = "string",
                      CountryName = "string"
                    ),
                    City = list(
                      CityName = "string"
                    ),
                    GeoLocation = list(
                      Lon = 123.0,
                      Lat = 123.0
                    )
                  )
                )
              ),
              Blocked = TRUE|FALSE
            )
          ),
          FindingProviderFields = list(
            Confidence = 123,
            Criticality = 123,
            RelatedFindings = list(
              list(
                ProductArn = "string",
                Id = "string"
              )
            ),
            Severity = list(
              Label = "INFORMATIONAL"|"LOW"|"MEDIUM"|"HIGH"|"CRITICAL",
              Original = "string"
            ),
            Types = list(
              "string"
            )
          ),
          Sample = TRUE|FALSE
        )
      ),
      NextToken = "string"
    )

### Request syntax

    svc$get_findings(
      Filters = list(
        ProductArn = list(
          list(
            Value = "string",
            Comparison = "EQUALS"|"PREFIX"|"NOT_EQUALS"|"PREFIX_NOT_EQUALS"
          )
        ),
        AwsAccountId = list(
          list(
            Value = "string",
            Comparison = "EQUALS"|"PREFIX"|"NOT_EQUALS"|"PREFIX_NOT_EQUALS"
          )
        ),
        Id = list(
          list(
            Value = "string",
            Comparison = "EQUALS"|"PREFIX"|"NOT_EQUALS"|"PREFIX_NOT_EQUALS"
          )
        ),
        GeneratorId = list(
          list(
            Value = "string",
            Comparison = "EQUALS"|"PREFIX"|"NOT_EQUALS"|"PREFIX_NOT_EQUALS"
          )
        ),
        Region = list(
          list(
            Value = "string",
            Comparison = "EQUALS"|"PREFIX"|"NOT_EQUALS"|"PREFIX_NOT_EQUALS"
          )
        ),
        Type = list(
          list(
            Value = "string",
            Comparison = "EQUALS"|"PREFIX"|"NOT_EQUALS"|"PREFIX_NOT_EQUALS"
          )
        ),
        FirstObservedAt = list(
          list(
            Start = "string",
            End = "string",
            DateRange = list(
              Value = 123,
              Unit = "DAYS"
            )
          )
        ),
        LastObservedAt = list(
          list(
            Start = "string",
            End = "string",
            DateRange = list(
              Value = 123,
              Unit = "DAYS"
            )
          )
        ),
        CreatedAt = list(
          list(
            Start = "string",
            End = "string",
            DateRange = list(
              Value = 123,
              Unit = "DAYS"
            )
          )
        ),
        UpdatedAt = list(
          list(
            Start = "string",
            End = "string",
            DateRange = list(
              Value = 123,
              Unit = "DAYS"
            )
          )
        ),
        SeverityProduct = list(
          list(
            Gte = 123.0,
            Lte = 123.0,
            Eq = 123.0
          )
        ),
        SeverityNormalized = list(
          list(
            Gte = 123.0,
            Lte = 123.0,
            Eq = 123.0
          )
        ),
        SeverityLabel = list(
          list(
            Value = "string",
            Comparison = "EQUALS"|"PREFIX"|"NOT_EQUALS"|"PREFIX_NOT_EQUALS"
          )
        ),
        Confidence = list(
          list(
            Gte = 123.0,
            Lte = 123.0,
            Eq = 123.0
          )
        ),
        Criticality = list(
          list(
            Gte = 123.0,
            Lte = 123.0,
            Eq = 123.0
          )
        ),
        Title = list(
          list(
            Value = "string",
            Comparison = "EQUALS"|"PREFIX"|"NOT_EQUALS"|"PREFIX_NOT_EQUALS"
          )
        ),
        Description = list(
          list(
            Value = "string",
            Comparison = "EQUALS"|"PREFIX"|"NOT_EQUALS"|"PREFIX_NOT_EQUALS"
          )
        ),
        RecommendationText = list(
          list(
            Value = "string",
            Comparison = "EQUALS"|"PREFIX"|"NOT_EQUALS"|"PREFIX_NOT_EQUALS"
          )
        ),
        SourceUrl = list(
          list(
            Value = "string",
            Comparison = "EQUALS"|"PREFIX"|"NOT_EQUALS"|"PREFIX_NOT_EQUALS"
          )
        ),
        ProductFields = list(
          list(
            Key = "string",
            Value = "string",
            Comparison = "EQUALS"|"NOT_EQUALS"
          )
        ),
        ProductName = list(
          list(
            Value = "string",
            Comparison = "EQUALS"|"PREFIX"|"NOT_EQUALS"|"PREFIX_NOT_EQUALS"
          )
        ),
        CompanyName = list(
          list(
            Value = "string",
            Comparison = "EQUALS"|"PREFIX"|"NOT_EQUALS"|"PREFIX_NOT_EQUALS"
          )
        ),
        UserDefinedFields = list(
          list(
            Key = "string",
            Value = "string",
            Comparison = "EQUALS"|"NOT_EQUALS"
          )
        ),
        MalwareName = list(
          list(
            Value = "string",
            Comparison = "EQUALS"|"PREFIX"|"NOT_EQUALS"|"PREFIX_NOT_EQUALS"
          )
        ),
        MalwareType = list(
          list(
            Value = "string",
            Comparison = "EQUALS"|"PREFIX"|"NOT_EQUALS"|"PREFIX_NOT_EQUALS"
          )
        ),
        MalwarePath = list(
          list(
            Value = "string",
            Comparison = "EQUALS"|"PREFIX"|"NOT_EQUALS"|"PREFIX_NOT_EQUALS"
          )
        ),
        MalwareState = list(
          list(
            Value = "string",
            Comparison = "EQUALS"|"PREFIX"|"NOT_EQUALS"|"PREFIX_NOT_EQUALS"
          )
        ),
        NetworkDirection = list(
          list(
            Value = "string",
            Comparison = "EQUALS"|"PREFIX"|"NOT_EQUALS"|"PREFIX_NOT_EQUALS"
          )
        ),
        NetworkProtocol = list(
          list(
            Value = "string",
            Comparison = "EQUALS"|"PREFIX"|"NOT_EQUALS"|"PREFIX_NOT_EQUALS"
          )
        ),
        NetworkSourceIpV4 = list(
          list(
            Cidr = "string"
          )
        ),
        NetworkSourceIpV6 = list(
          list(
            Cidr = "string"
          )
        ),
        NetworkSourcePort = list(
          list(
            Gte = 123.0,
            Lte = 123.0,
            Eq = 123.0
          )
        ),
        NetworkSourceDomain = list(
          list(
            Value = "string",
            Comparison = "EQUALS"|"PREFIX"|"NOT_EQUALS"|"PREFIX_NOT_EQUALS"
          )
        ),
        NetworkSourceMac = list(
          list(
            Value = "string",
            Comparison = "EQUALS"|"PREFIX"|"NOT_EQUALS"|"PREFIX_NOT_EQUALS"
          )
        ),
        NetworkDestinationIpV4 = list(
          list(
            Cidr = "string"
          )
        ),
        NetworkDestinationIpV6 = list(
          list(
            Cidr = "string"
          )
        ),
        NetworkDestinationPort = list(
          list(
            Gte = 123.0,
            Lte = 123.0,
            Eq = 123.0
          )
        ),
        NetworkDestinationDomain = list(
          list(
            Value = "string",
            Comparison = "EQUALS"|"PREFIX"|"NOT_EQUALS"|"PREFIX_NOT_EQUALS"
          )
        ),
        ProcessName = list(
          list(
            Value = "string",
            Comparison = "EQUALS"|"PREFIX"|"NOT_EQUALS"|"PREFIX_NOT_EQUALS"
          )
        ),
        ProcessPath = list(
          list(
            Value = "string",
            Comparison = "EQUALS"|"PREFIX"|"NOT_EQUALS"|"PREFIX_NOT_EQUALS"
          )
        ),
        ProcessPid = list(
          list(
            Gte = 123.0,
            Lte = 123.0,
            Eq = 123.0
          )
        ),
        ProcessParentPid = list(
          list(
            Gte = 123.0,
            Lte = 123.0,
            Eq = 123.0
          )
        ),
        ProcessLaunchedAt = list(
          list(
            Start = "string",
            End = "string",
            DateRange = list(
              Value = 123,
              Unit = "DAYS"
            )
          )
        ),
        ProcessTerminatedAt = list(
          list(
            Start = "string",
            End = "string",
            DateRange = list(
              Value = 123,
              Unit = "DAYS"
            )
          )
        ),
        ThreatIntelIndicatorType = list(
          list(
            Value = "string",
            Comparison = "EQUALS"|"PREFIX"|"NOT_EQUALS"|"PREFIX_NOT_EQUALS"
          )
        ),
        ThreatIntelIndicatorValue = list(
          list(
            Value = "string",
            Comparison = "EQUALS"|"PREFIX"|"NOT_EQUALS"|"PREFIX_NOT_EQUALS"
          )
        ),
        ThreatIntelIndicatorCategory = list(
          list(
            Value = "string",
            Comparison = "EQUALS"|"PREFIX"|"NOT_EQUALS"|"PREFIX_NOT_EQUALS"
          )
        ),
        ThreatIntelIndicatorLastObservedAt = list(
          list(
            Start = "string",
            End = "string",
            DateRange = list(
              Value = 123,
              Unit = "DAYS"
            )
          )
        ),
        ThreatIntelIndicatorSource = list(
          list(
            Value = "string",
            Comparison = "EQUALS"|"PREFIX"|"NOT_EQUALS"|"PREFIX_NOT_EQUALS"
          )
        ),
        ThreatIntelIndicatorSourceUrl = list(
          list(
            Value = "string",
            Comparison = "EQUALS"|"PREFIX"|"NOT_EQUALS"|"PREFIX_NOT_EQUALS"
          )
        ),
        ResourceType = list(
          list(
            Value = "string",
            Comparison = "EQUALS"|"PREFIX"|"NOT_EQUALS"|"PREFIX_NOT_EQUALS"
          )
        ),
        ResourceId = list(
          list(
            Value = "string",
            Comparison = "EQUALS"|"PREFIX"|"NOT_EQUALS"|"PREFIX_NOT_EQUALS"
          )
        ),
        ResourcePartition = list(
          list(
            Value = "string",
            Comparison = "EQUALS"|"PREFIX"|"NOT_EQUALS"|"PREFIX_NOT_EQUALS"
          )
        ),
        ResourceRegion = list(
          list(
            Value = "string",
            Comparison = "EQUALS"|"PREFIX"|"NOT_EQUALS"|"PREFIX_NOT_EQUALS"
          )
        ),
        ResourceTags = list(
          list(
            Key = "string",
            Value = "string",
            Comparison = "EQUALS"|"NOT_EQUALS"
          )
        ),
        ResourceAwsEc2InstanceType = list(
          list(
            Value = "string",
            Comparison = "EQUALS"|"PREFIX"|"NOT_EQUALS"|"PREFIX_NOT_EQUALS"
          )
        ),
        ResourceAwsEc2InstanceImageId = list(
          list(
            Value = "string",
            Comparison = "EQUALS"|"PREFIX"|"NOT_EQUALS"|"PREFIX_NOT_EQUALS"
          )
        ),
        ResourceAwsEc2InstanceIpV4Addresses = list(
          list(
            Cidr = "string"
          )
        ),
        ResourceAwsEc2InstanceIpV6Addresses = list(
          list(
            Cidr = "string"
          )
        ),
        ResourceAwsEc2InstanceKeyName = list(
          list(
            Value = "string",
            Comparison = "EQUALS"|"PREFIX"|"NOT_EQUALS"|"PREFIX_NOT_EQUALS"
          )
        ),
        ResourceAwsEc2InstanceIamInstanceProfileArn = list(
          list(
            Value = "string",
            Comparison = "EQUALS"|"PREFIX"|"NOT_EQUALS"|"PREFIX_NOT_EQUALS"
          )
        ),
        ResourceAwsEc2InstanceVpcId = list(
          list(
            Value = "string",
            Comparison = "EQUALS"|"PREFIX"|"NOT_EQUALS"|"PREFIX_NOT_EQUALS"
          )
        ),
        ResourceAwsEc2InstanceSubnetId = list(
          list(
            Value = "string",
            Comparison = "EQUALS"|"PREFIX"|"NOT_EQUALS"|"PREFIX_NOT_EQUALS"
          )
        ),
        ResourceAwsEc2InstanceLaunchedAt = list(
          list(
            Start = "string",
            End = "string",
            DateRange = list(
              Value = 123,
              Unit = "DAYS"
            )
          )
        ),
        ResourceAwsS3BucketOwnerId = list(
          list(
            Value = "string",
            Comparison = "EQUALS"|"PREFIX"|"NOT_EQUALS"|"PREFIX_NOT_EQUALS"
          )
        ),
        ResourceAwsS3BucketOwnerName = list(
          list(
            Value = "string",
            Comparison = "EQUALS"|"PREFIX"|"NOT_EQUALS"|"PREFIX_NOT_EQUALS"
          )
        ),
        ResourceAwsIamAccessKeyUserName = list(
          list(
            Value = "string",
            Comparison = "EQUALS"|"PREFIX"|"NOT_EQUALS"|"PREFIX_NOT_EQUALS"
          )
        ),
        ResourceAwsIamAccessKeyPrincipalName = list(
          list(
            Value = "string",
            Comparison = "EQUALS"|"PREFIX"|"NOT_EQUALS"|"PREFIX_NOT_EQUALS"
          )
        ),
        ResourceAwsIamAccessKeyStatus = list(
          list(
            Value = "string",
            Comparison = "EQUALS"|"PREFIX"|"NOT_EQUALS"|"PREFIX_NOT_EQUALS"
          )
        ),
        ResourceAwsIamAccessKeyCreatedAt = list(
          list(
            Start = "string",
            End = "string",
            DateRange = list(
              Value = 123,
              Unit = "DAYS"
            )
          )
        ),
        ResourceAwsIamUserUserName = list(
          list(
            Value = "string",
            Comparison = "EQUALS"|"PREFIX"|"NOT_EQUALS"|"PREFIX_NOT_EQUALS"
          )
        ),
        ResourceContainerName = list(
          list(
            Value = "string",
            Comparison = "EQUALS"|"PREFIX"|"NOT_EQUALS"|"PREFIX_NOT_EQUALS"
          )
        ),
        ResourceContainerImageId = list(
          list(
            Value = "string",
            Comparison = "EQUALS"|"PREFIX"|"NOT_EQUALS"|"PREFIX_NOT_EQUALS"
          )
        ),
        ResourceContainerImageName = list(
          list(
            Value = "string",
            Comparison = "EQUALS"|"PREFIX"|"NOT_EQUALS"|"PREFIX_NOT_EQUALS"
          )
        ),
        ResourceContainerLaunchedAt = list(
          list(
            Start = "string",
            End = "string",
            DateRange = list(
              Value = 123,
              Unit = "DAYS"
            )
          )
        ),
        ResourceDetailsOther = list(
          list(
            Key = "string",
            Value = "string",
            Comparison = "EQUALS"|"NOT_EQUALS"
          )
        ),
        ComplianceStatus = list(
          list(
            Value = "string",
            Comparison = "EQUALS"|"PREFIX"|"NOT_EQUALS"|"PREFIX_NOT_EQUALS"
          )
        ),
        VerificationState = list(
          list(
            Value = "string",
            Comparison = "EQUALS"|"PREFIX"|"NOT_EQUALS"|"PREFIX_NOT_EQUALS"
          )
        ),
        WorkflowState = list(
          list(
            Value = "string",
            Comparison = "EQUALS"|"PREFIX"|"NOT_EQUALS"|"PREFIX_NOT_EQUALS"
          )
        ),
        WorkflowStatus = list(
          list(
            Value = "string",
            Comparison = "EQUALS"|"PREFIX"|"NOT_EQUALS"|"PREFIX_NOT_EQUALS"
          )
        ),
        RecordState = list(
          list(
            Value = "string",
            Comparison = "EQUALS"|"PREFIX"|"NOT_EQUALS"|"PREFIX_NOT_EQUALS"
          )
        ),
        RelatedFindingsProductArn = list(
          list(
            Value = "string",
            Comparison = "EQUALS"|"PREFIX"|"NOT_EQUALS"|"PREFIX_NOT_EQUALS"
          )
        ),
        RelatedFindingsId = list(
          list(
            Value = "string",
            Comparison = "EQUALS"|"PREFIX"|"NOT_EQUALS"|"PREFIX_NOT_EQUALS"
          )
        ),
        NoteText = list(
          list(
            Value = "string",
            Comparison = "EQUALS"|"PREFIX"|"NOT_EQUALS"|"PREFIX_NOT_EQUALS"
          )
        ),
        NoteUpdatedAt = list(
          list(
            Start = "string",
            End = "string",
            DateRange = list(
              Value = 123,
              Unit = "DAYS"
            )
          )
        ),
        NoteUpdatedBy = list(
          list(
            Value = "string",
            Comparison = "EQUALS"|"PREFIX"|"NOT_EQUALS"|"PREFIX_NOT_EQUALS"
          )
        ),
        Keyword = list(
          list(
            Value = "string"
          )
        ),
        FindingProviderFieldsConfidence = list(
          list(
            Gte = 123.0,
            Lte = 123.0,
            Eq = 123.0
          )
        ),
        FindingProviderFieldsCriticality = list(
          list(
            Gte = 123.0,
            Lte = 123.0,
            Eq = 123.0
          )
        ),
        FindingProviderFieldsRelatedFindingsId = list(
          list(
            Value = "string",
            Comparison = "EQUALS"|"PREFIX"|"NOT_EQUALS"|"PREFIX_NOT_EQUALS"
          )
        ),
        FindingProviderFieldsRelatedFindingsProductArn = list(
          list(
            Value = "string",
            Comparison = "EQUALS"|"PREFIX"|"NOT_EQUALS"|"PREFIX_NOT_EQUALS"
          )
        ),
        FindingProviderFieldsSeverityLabel = list(
          list(
            Value = "string",
            Comparison = "EQUALS"|"PREFIX"|"NOT_EQUALS"|"PREFIX_NOT_EQUALS"
          )
        ),
        FindingProviderFieldsSeverityOriginal = list(
          list(
            Value = "string",
            Comparison = "EQUALS"|"PREFIX"|"NOT_EQUALS"|"PREFIX_NOT_EQUALS"
          )
        ),
        FindingProviderFieldsTypes = list(
          list(
            Value = "string",
            Comparison = "EQUALS"|"PREFIX"|"NOT_EQUALS"|"PREFIX_NOT_EQUALS"
          )
        ),
        Sample = list(
          list(
            Value = TRUE|FALSE
          )
        ),
        ComplianceSecurityControlId = list(
          list(
            Value = "string",
            Comparison = "EQUALS"|"PREFIX"|"NOT_EQUALS"|"PREFIX_NOT_EQUALS"
          )
        ),
        ComplianceAssociatedStandardsId = list(
          list(
            Value = "string",
            Comparison = "EQUALS"|"PREFIX"|"NOT_EQUALS"|"PREFIX_NOT_EQUALS"
          )
        )
      ),
      SortCriteria = list(
        list(
          Field = "string",
          SortOrder = "asc"|"desc"
        )
      ),
      NextToken = "string",
      MaxResults = 123
    )

### Examples

    ## Not run: 
    # The following example returns a filtered and sorted list of Security Hub
    # findings.
    svc$get_findings(
      Filters = list(
        AwsAccountId = list(
          list(
            Comparison = "PREFIX",
            Value = "123456789012"
          )
        )
      ),
      MaxResults = 1L
    )

    ## End(Not run)