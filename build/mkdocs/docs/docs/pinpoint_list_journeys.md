<table style="width: 100%;">
<tbody>
<tr class="odd">
<td>pinpoint_list_journeys</td>
<td style="text-align: right;">R Documentation</td>
</tr>
</tbody>
</table>

## Retrieves information about the status, configuration, and other settings for all the journeys that are associated with an application

### Description

Retrieves information about the status, configuration, and other
settings for all the journeys that are associated with an application.

### Usage

    pinpoint_list_journeys(ApplicationId, PageSize, Token)

### Arguments

<table>
<colgroup>
<col style="width: 35%" />
<col style="width: 65%" />
</colgroup>
<tbody>
<tr class="odd">
<td><code
id="pinpoint_list_journeys_:_ApplicationId">ApplicationId</code></td>
<td><p>[required] The unique identifier for the application. This
identifier is displayed as the <strong>Project ID</strong> on the Amazon
Pinpoint console.</p></td>
</tr>
<tr class="even">
<td><code id="pinpoint_list_journeys_:_PageSize">PageSize</code></td>
<td><p>The maximum number of items to include in each page of a
paginated response. This parameter is not supported for application,
campaign, and journey metrics.</p></td>
</tr>
<tr class="odd">
<td><code id="pinpoint_list_journeys_:_Token">Token</code></td>
<td><p>The NextToken string that specifies which page of results to
return in a paginated response.</p></td>
</tr>
</tbody>
</table>

### Value

A list with the following syntax:

    list(
      JourneysResponse = list(
        Item = list(
          list(
            Activities = list(
              list(
                CUSTOM = list(
                  DeliveryUri = "string",
                  EndpointTypes = list(
                    "PUSH"|"GCM"|"APNS"|"APNS_SANDBOX"|"APNS_VOIP"|"APNS_VOIP_SANDBOX"|"ADM"|"SMS"|"VOICE"|"EMAIL"|"BAIDU"|"CUSTOM"|"IN_APP"
                  ),
                  MessageConfig = list(
                    Data = "string"
                  ),
                  NextActivity = "string",
                  TemplateName = "string",
                  TemplateVersion = "string"
                ),
                ConditionalSplit = list(
                  Condition = list(
                    Conditions = list(
                      list(
                        EventCondition = list(
                          Dimensions = list(
                            Attributes = list(
                              list(
                                AttributeType = "INCLUSIVE"|"EXCLUSIVE"|"CONTAINS"|"BEFORE"|"AFTER"|"ON"|"BETWEEN",
                                Values = list(
                                  "string"
                                )
                              )
                            ),
                            EventType = list(
                              DimensionType = "INCLUSIVE"|"EXCLUSIVE",
                              Values = list(
                                "string"
                              )
                            ),
                            Metrics = list(
                              list(
                                ComparisonOperator = "string",
                                Value = 123.0
                              )
                            )
                          ),
                          MessageActivity = "string"
                        ),
                        SegmentCondition = list(
                          SegmentId = "string"
                        ),
                        SegmentDimensions = list(
                          Attributes = list(
                            list(
                              AttributeType = "INCLUSIVE"|"EXCLUSIVE"|"CONTAINS"|"BEFORE"|"AFTER"|"ON"|"BETWEEN",
                              Values = list(
                                "string"
                              )
                            )
                          ),
                          Behavior = list(
                            Recency = list(
                              Duration = "HR_24"|"DAY_7"|"DAY_14"|"DAY_30",
                              RecencyType = "ACTIVE"|"INACTIVE"
                            )
                          ),
                          Demographic = list(
                            AppVersion = list(
                              DimensionType = "INCLUSIVE"|"EXCLUSIVE",
                              Values = list(
                                "string"
                              )
                            ),
                            Channel = list(
                              DimensionType = "INCLUSIVE"|"EXCLUSIVE",
                              Values = list(
                                "string"
                              )
                            ),
                            DeviceType = list(
                              DimensionType = "INCLUSIVE"|"EXCLUSIVE",
                              Values = list(
                                "string"
                              )
                            ),
                            Make = list(
                              DimensionType = "INCLUSIVE"|"EXCLUSIVE",
                              Values = list(
                                "string"
                              )
                            ),
                            Model = list(
                              DimensionType = "INCLUSIVE"|"EXCLUSIVE",
                              Values = list(
                                "string"
                              )
                            ),
                            Platform = list(
                              DimensionType = "INCLUSIVE"|"EXCLUSIVE",
                              Values = list(
                                "string"
                              )
                            )
                          ),
                          Location = list(
                            Country = list(
                              DimensionType = "INCLUSIVE"|"EXCLUSIVE",
                              Values = list(
                                "string"
                              )
                            ),
                            GPSPoint = list(
                              Coordinates = list(
                                Latitude = 123.0,
                                Longitude = 123.0
                              ),
                              RangeInKilometers = 123.0
                            )
                          ),
                          Metrics = list(
                            list(
                              ComparisonOperator = "string",
                              Value = 123.0
                            )
                          ),
                          UserAttributes = list(
                            list(
                              AttributeType = "INCLUSIVE"|"EXCLUSIVE"|"CONTAINS"|"BEFORE"|"AFTER"|"ON"|"BETWEEN",
                              Values = list(
                                "string"
                              )
                            )
                          )
                        )
                      )
                    ),
                    Operator = "ALL"|"ANY"
                  ),
                  EvaluationWaitTime = list(
                    WaitFor = "string",
                    WaitUntil = "string"
                  ),
                  FalseActivity = "string",
                  TrueActivity = "string"
                ),
                Description = "string",
                EMAIL = list(
                  MessageConfig = list(
                    FromAddress = "string"
                  ),
                  NextActivity = "string",
                  TemplateName = "string",
                  TemplateVersion = "string"
                ),
                Holdout = list(
                  NextActivity = "string",
                  Percentage = 123
                ),
                MultiCondition = list(
                  Branches = list(
                    list(
                      Condition = list(
                        EventCondition = list(
                          Dimensions = list(
                            Attributes = list(
                              list(
                                AttributeType = "INCLUSIVE"|"EXCLUSIVE"|"CONTAINS"|"BEFORE"|"AFTER"|"ON"|"BETWEEN",
                                Values = list(
                                  "string"
                                )
                              )
                            ),
                            EventType = list(
                              DimensionType = "INCLUSIVE"|"EXCLUSIVE",
                              Values = list(
                                "string"
                              )
                            ),
                            Metrics = list(
                              list(
                                ComparisonOperator = "string",
                                Value = 123.0
                              )
                            )
                          ),
                          MessageActivity = "string"
                        ),
                        SegmentCondition = list(
                          SegmentId = "string"
                        ),
                        SegmentDimensions = list(
                          Attributes = list(
                            list(
                              AttributeType = "INCLUSIVE"|"EXCLUSIVE"|"CONTAINS"|"BEFORE"|"AFTER"|"ON"|"BETWEEN",
                              Values = list(
                                "string"
                              )
                            )
                          ),
                          Behavior = list(
                            Recency = list(
                              Duration = "HR_24"|"DAY_7"|"DAY_14"|"DAY_30",
                              RecencyType = "ACTIVE"|"INACTIVE"
                            )
                          ),
                          Demographic = list(
                            AppVersion = list(
                              DimensionType = "INCLUSIVE"|"EXCLUSIVE",
                              Values = list(
                                "string"
                              )
                            ),
                            Channel = list(
                              DimensionType = "INCLUSIVE"|"EXCLUSIVE",
                              Values = list(
                                "string"
                              )
                            ),
                            DeviceType = list(
                              DimensionType = "INCLUSIVE"|"EXCLUSIVE",
                              Values = list(
                                "string"
                              )
                            ),
                            Make = list(
                              DimensionType = "INCLUSIVE"|"EXCLUSIVE",
                              Values = list(
                                "string"
                              )
                            ),
                            Model = list(
                              DimensionType = "INCLUSIVE"|"EXCLUSIVE",
                              Values = list(
                                "string"
                              )
                            ),
                            Platform = list(
                              DimensionType = "INCLUSIVE"|"EXCLUSIVE",
                              Values = list(
                                "string"
                              )
                            )
                          ),
                          Location = list(
                            Country = list(
                              DimensionType = "INCLUSIVE"|"EXCLUSIVE",
                              Values = list(
                                "string"
                              )
                            ),
                            GPSPoint = list(
                              Coordinates = list(
                                Latitude = 123.0,
                                Longitude = 123.0
                              ),
                              RangeInKilometers = 123.0
                            )
                          ),
                          Metrics = list(
                            list(
                              ComparisonOperator = "string",
                              Value = 123.0
                            )
                          ),
                          UserAttributes = list(
                            list(
                              AttributeType = "INCLUSIVE"|"EXCLUSIVE"|"CONTAINS"|"BEFORE"|"AFTER"|"ON"|"BETWEEN",
                              Values = list(
                                "string"
                              )
                            )
                          )
                        )
                      ),
                      NextActivity = "string"
                    )
                  ),
                  DefaultActivity = "string",
                  EvaluationWaitTime = list(
                    WaitFor = "string",
                    WaitUntil = "string"
                  )
                ),
                PUSH = list(
                  MessageConfig = list(
                    TimeToLive = "string"
                  ),
                  NextActivity = "string",
                  TemplateName = "string",
                  TemplateVersion = "string"
                ),
                RandomSplit = list(
                  Branches = list(
                    list(
                      NextActivity = "string",
                      Percentage = 123
                    )
                  )
                ),
                SMS = list(
                  MessageConfig = list(
                    MessageType = "TRANSACTIONAL"|"PROMOTIONAL",
                    OriginationNumber = "string",
                    SenderId = "string",
                    EntityId = "string",
                    TemplateId = "string"
                  ),
                  NextActivity = "string",
                  TemplateName = "string",
                  TemplateVersion = "string"
                ),
                Wait = list(
                  NextActivity = "string",
                  WaitTime = list(
                    WaitFor = "string",
                    WaitUntil = "string"
                  )
                ),
                ContactCenter = list(
                  NextActivity = "string"
                )
              )
            ),
            ApplicationId = "string",
            CreationDate = "string",
            Id = "string",
            LastModifiedDate = "string",
            Limits = list(
              DailyCap = 123,
              EndpointReentryCap = 123,
              MessagesPerSecond = 123,
              EndpointReentryInterval = "string"
            ),
            LocalTime = TRUE|FALSE,
            Name = "string",
            QuietTime = list(
              End = "string",
              Start = "string"
            ),
            RefreshFrequency = "string",
            Schedule = list(
              EndTime = as.POSIXct(
                "2015-01-01"
              ),
              StartTime = as.POSIXct(
                "2015-01-01"
              ),
              Timezone = "string"
            ),
            StartActivity = "string",
            StartCondition = list(
              Description = "string",
              EventStartCondition = list(
                EventFilter = list(
                  Dimensions = list(
                    Attributes = list(
                      list(
                        AttributeType = "INCLUSIVE"|"EXCLUSIVE"|"CONTAINS"|"BEFORE"|"AFTER"|"ON"|"BETWEEN",
                        Values = list(
                          "string"
                        )
                      )
                    ),
                    EventType = list(
                      DimensionType = "INCLUSIVE"|"EXCLUSIVE",
                      Values = list(
                        "string"
                      )
                    ),
                    Metrics = list(
                      list(
                        ComparisonOperator = "string",
                        Value = 123.0
                      )
                    )
                  ),
                  FilterType = "SYSTEM"|"ENDPOINT"
                ),
                SegmentId = "string"
              ),
              SegmentStartCondition = list(
                SegmentId = "string"
              )
            ),
            State = "DRAFT"|"ACTIVE"|"COMPLETED"|"CANCELLED"|"CLOSED"|"PAUSED",
            tags = list(
              "string"
            ),
            WaitForQuietTime = TRUE|FALSE,
            RefreshOnSegmentUpdate = TRUE|FALSE,
            JourneyChannelSettings = list(
              ConnectCampaignArn = "string",
              ConnectCampaignExecutionRoleArn = "string"
            ),
            SendingSchedule = TRUE|FALSE,
            OpenHours = list(
              EMAIL = list(
                list(
                  list(
                    StartTime = "string",
                    EndTime = "string"
                  )
                )
              ),
              SMS = list(
                list(
                  list(
                    StartTime = "string",
                    EndTime = "string"
                  )
                )
              ),
              PUSH = list(
                list(
                  list(
                    StartTime = "string",
                    EndTime = "string"
                  )
                )
              ),
              VOICE = list(
                list(
                  list(
                    StartTime = "string",
                    EndTime = "string"
                  )
                )
              ),
              CUSTOM = list(
                list(
                  list(
                    StartTime = "string",
                    EndTime = "string"
                  )
                )
              )
            ),
            ClosedDays = list(
              EMAIL = list(
                list(
                  Name = "string",
                  StartDateTime = "string",
                  EndDateTime = "string"
                )
              ),
              SMS = list(
                list(
                  Name = "string",
                  StartDateTime = "string",
                  EndDateTime = "string"
                )
              ),
              PUSH = list(
                list(
                  Name = "string",
                  StartDateTime = "string",
                  EndDateTime = "string"
                )
              ),
              VOICE = list(
                list(
                  Name = "string",
                  StartDateTime = "string",
                  EndDateTime = "string"
                )
              ),
              CUSTOM = list(
                list(
                  Name = "string",
                  StartDateTime = "string",
                  EndDateTime = "string"
                )
              )
            )
          )
        ),
        NextToken = "string"
      )
    )

### Request syntax

    svc$list_journeys(
      ApplicationId = "string",
      PageSize = "string",
      Token = "string"
    )