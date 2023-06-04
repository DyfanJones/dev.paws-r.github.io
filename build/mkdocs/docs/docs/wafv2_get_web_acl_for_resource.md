<table style="width: 100%;">
<tbody>
<tr class="odd">
<td>wafv2_get_web_acl_for_resource</td>
<td style="text-align: right;">R Documentation</td>
</tr>
</tbody>
</table>

## Retrieves the WebACL for the specified resource

### Description

Retrieves the WebACL for the specified resource.

### Usage

    wafv2_get_web_acl_for_resource(ResourceArn)

### Arguments

<table>
<colgroup>
<col style="width: 35%" />
<col style="width: 65%" />
</colgroup>
<tbody>
<tr class="odd">
<td><code
id="wafv2_get_web_acl_for_resource_:_ResourceArn">ResourceArn</code></td>
<td><p>[required] The Amazon Resource Name (ARN) of the resource whose
web ACL you want to retrieve.</p>
<p>The ARN must be in one of the following formats:</p>
<ul>
<li><p>For an Application Load Balancer:
<code>arn:partition:elasticloadbalancing:region:account-id:loadbalancer/app/load-balancer-name/load-balancer-id </code></p></li>
<li><p>For an Amazon API Gateway REST API: <code
style="white-space: pre;">⁠arn:partition:apigateway:region::/restapis/api-id/stages/stage-name ⁠</code></p></li>
<li><p>For an AppSync GraphQL API:
<code>arn:partition:appsync:region:account-id:apis/GraphQLApiId </code></p></li>
<li><p>For an Amazon Cognito user pool:
<code>arn:partition:cognito-idp:region:account-id:userpool/user-pool-id </code></p></li>
<li><p>For an App Runner service:
<code>arn:partition:apprunner:region:account-id:service/apprunner-service-name/apprunner-service-id </code></p></li>
<li><p>For an Amazon Web Services Verified Access instance:
<code>arn:partition:ec2:region:account-id:verified-access-instance/instance-id </code></p></li>
</ul></td>
</tr>
</tbody>
</table>

### Value

A list with the following syntax:

    list(
      WebACL = list(
        Name = "string",
        Id = "string",
        ARN = "string",
        DefaultAction = list(
          Block = list(
            CustomResponse = list(
              ResponseCode = 123,
              CustomResponseBodyKey = "string",
              ResponseHeaders = list(
                list(
                  Name = "string",
                  Value = "string"
                )
              )
            )
          ),
          Allow = list(
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
        Description = "string",
        Rules = list(
          list(
            Name = "string",
            Priority = 123,
            Statement = list(
              ByteMatchStatement = list(
                SearchString = raw,
                FieldToMatch = list(
                  SingleHeader = list(
                    Name = "string"
                  ),
                  SingleQueryArgument = list(
                    Name = "string"
                  ),
                  AllQueryArguments = list(),
                  UriPath = list(),
                  QueryString = list(),
                  Body = list(
                    OversizeHandling = "CONTINUE"|"MATCH"|"NO_MATCH"
                  ),
                  Method = list(),
                  JsonBody = list(
                    MatchPattern = list(
                      All = list(),
                      IncludedPaths = list(
                        "string"
                      )
                    ),
                    MatchScope = "ALL"|"KEY"|"VALUE",
                    InvalidFallbackBehavior = "MATCH"|"NO_MATCH"|"EVALUATE_AS_STRING",
                    OversizeHandling = "CONTINUE"|"MATCH"|"NO_MATCH"
                  ),
                  Headers = list(
                    MatchPattern = list(
                      All = list(),
                      IncludedHeaders = list(
                        "string"
                      ),
                      ExcludedHeaders = list(
                        "string"
                      )
                    ),
                    MatchScope = "ALL"|"KEY"|"VALUE",
                    OversizeHandling = "CONTINUE"|"MATCH"|"NO_MATCH"
                  ),
                  Cookies = list(
                    MatchPattern = list(
                      All = list(),
                      IncludedCookies = list(
                        "string"
                      ),
                      ExcludedCookies = list(
                        "string"
                      )
                    ),
                    MatchScope = "ALL"|"KEY"|"VALUE",
                    OversizeHandling = "CONTINUE"|"MATCH"|"NO_MATCH"
                  )
                ),
                TextTransformations = list(
                  list(
                    Priority = 123,
                    Type = "NONE"|"COMPRESS_WHITE_SPACE"|"HTML_ENTITY_DECODE"|"LOWERCASE"|"CMD_LINE"|"URL_DECODE"|"BASE64_DECODE"|"HEX_DECODE"|"MD5"|"REPLACE_COMMENTS"|"ESCAPE_SEQ_DECODE"|"SQL_HEX_DECODE"|"CSS_DECODE"|"JS_DECODE"|"NORMALIZE_PATH"|"NORMALIZE_PATH_WIN"|"REMOVE_NULLS"|"REPLACE_NULLS"|"BASE64_DECODE_EXT"|"URL_DECODE_UNI"|"UTF8_TO_UNICODE"
                  )
                ),
                PositionalConstraint = "EXACTLY"|"STARTS_WITH"|"ENDS_WITH"|"CONTAINS"|"CONTAINS_WORD"
              ),
              SqliMatchStatement = list(
                FieldToMatch = list(
                  SingleHeader = list(
                    Name = "string"
                  ),
                  SingleQueryArgument = list(
                    Name = "string"
                  ),
                  AllQueryArguments = list(),
                  UriPath = list(),
                  QueryString = list(),
                  Body = list(
                    OversizeHandling = "CONTINUE"|"MATCH"|"NO_MATCH"
                  ),
                  Method = list(),
                  JsonBody = list(
                    MatchPattern = list(
                      All = list(),
                      IncludedPaths = list(
                        "string"
                      )
                    ),
                    MatchScope = "ALL"|"KEY"|"VALUE",
                    InvalidFallbackBehavior = "MATCH"|"NO_MATCH"|"EVALUATE_AS_STRING",
                    OversizeHandling = "CONTINUE"|"MATCH"|"NO_MATCH"
                  ),
                  Headers = list(
                    MatchPattern = list(
                      All = list(),
                      IncludedHeaders = list(
                        "string"
                      ),
                      ExcludedHeaders = list(
                        "string"
                      )
                    ),
                    MatchScope = "ALL"|"KEY"|"VALUE",
                    OversizeHandling = "CONTINUE"|"MATCH"|"NO_MATCH"
                  ),
                  Cookies = list(
                    MatchPattern = list(
                      All = list(),
                      IncludedCookies = list(
                        "string"
                      ),
                      ExcludedCookies = list(
                        "string"
                      )
                    ),
                    MatchScope = "ALL"|"KEY"|"VALUE",
                    OversizeHandling = "CONTINUE"|"MATCH"|"NO_MATCH"
                  )
                ),
                TextTransformations = list(
                  list(
                    Priority = 123,
                    Type = "NONE"|"COMPRESS_WHITE_SPACE"|"HTML_ENTITY_DECODE"|"LOWERCASE"|"CMD_LINE"|"URL_DECODE"|"BASE64_DECODE"|"HEX_DECODE"|"MD5"|"REPLACE_COMMENTS"|"ESCAPE_SEQ_DECODE"|"SQL_HEX_DECODE"|"CSS_DECODE"|"JS_DECODE"|"NORMALIZE_PATH"|"NORMALIZE_PATH_WIN"|"REMOVE_NULLS"|"REPLACE_NULLS"|"BASE64_DECODE_EXT"|"URL_DECODE_UNI"|"UTF8_TO_UNICODE"
                  )
                ),
                SensitivityLevel = "LOW"|"HIGH"
              ),
              XssMatchStatement = list(
                FieldToMatch = list(
                  SingleHeader = list(
                    Name = "string"
                  ),
                  SingleQueryArgument = list(
                    Name = "string"
                  ),
                  AllQueryArguments = list(),
                  UriPath = list(),
                  QueryString = list(),
                  Body = list(
                    OversizeHandling = "CONTINUE"|"MATCH"|"NO_MATCH"
                  ),
                  Method = list(),
                  JsonBody = list(
                    MatchPattern = list(
                      All = list(),
                      IncludedPaths = list(
                        "string"
                      )
                    ),
                    MatchScope = "ALL"|"KEY"|"VALUE",
                    InvalidFallbackBehavior = "MATCH"|"NO_MATCH"|"EVALUATE_AS_STRING",
                    OversizeHandling = "CONTINUE"|"MATCH"|"NO_MATCH"
                  ),
                  Headers = list(
                    MatchPattern = list(
                      All = list(),
                      IncludedHeaders = list(
                        "string"
                      ),
                      ExcludedHeaders = list(
                        "string"
                      )
                    ),
                    MatchScope = "ALL"|"KEY"|"VALUE",
                    OversizeHandling = "CONTINUE"|"MATCH"|"NO_MATCH"
                  ),
                  Cookies = list(
                    MatchPattern = list(
                      All = list(),
                      IncludedCookies = list(
                        "string"
                      ),
                      ExcludedCookies = list(
                        "string"
                      )
                    ),
                    MatchScope = "ALL"|"KEY"|"VALUE",
                    OversizeHandling = "CONTINUE"|"MATCH"|"NO_MATCH"
                  )
                ),
                TextTransformations = list(
                  list(
                    Priority = 123,
                    Type = "NONE"|"COMPRESS_WHITE_SPACE"|"HTML_ENTITY_DECODE"|"LOWERCASE"|"CMD_LINE"|"URL_DECODE"|"BASE64_DECODE"|"HEX_DECODE"|"MD5"|"REPLACE_COMMENTS"|"ESCAPE_SEQ_DECODE"|"SQL_HEX_DECODE"|"CSS_DECODE"|"JS_DECODE"|"NORMALIZE_PATH"|"NORMALIZE_PATH_WIN"|"REMOVE_NULLS"|"REPLACE_NULLS"|"BASE64_DECODE_EXT"|"URL_DECODE_UNI"|"UTF8_TO_UNICODE"
                  )
                )
              ),
              SizeConstraintStatement = list(
                FieldToMatch = list(
                  SingleHeader = list(
                    Name = "string"
                  ),
                  SingleQueryArgument = list(
                    Name = "string"
                  ),
                  AllQueryArguments = list(),
                  UriPath = list(),
                  QueryString = list(),
                  Body = list(
                    OversizeHandling = "CONTINUE"|"MATCH"|"NO_MATCH"
                  ),
                  Method = list(),
                  JsonBody = list(
                    MatchPattern = list(
                      All = list(),
                      IncludedPaths = list(
                        "string"
                      )
                    ),
                    MatchScope = "ALL"|"KEY"|"VALUE",
                    InvalidFallbackBehavior = "MATCH"|"NO_MATCH"|"EVALUATE_AS_STRING",
                    OversizeHandling = "CONTINUE"|"MATCH"|"NO_MATCH"
                  ),
                  Headers = list(
                    MatchPattern = list(
                      All = list(),
                      IncludedHeaders = list(
                        "string"
                      ),
                      ExcludedHeaders = list(
                        "string"
                      )
                    ),
                    MatchScope = "ALL"|"KEY"|"VALUE",
                    OversizeHandling = "CONTINUE"|"MATCH"|"NO_MATCH"
                  ),
                  Cookies = list(
                    MatchPattern = list(
                      All = list(),
                      IncludedCookies = list(
                        "string"
                      ),
                      ExcludedCookies = list(
                        "string"
                      )
                    ),
                    MatchScope = "ALL"|"KEY"|"VALUE",
                    OversizeHandling = "CONTINUE"|"MATCH"|"NO_MATCH"
                  )
                ),
                ComparisonOperator = "EQ"|"NE"|"LE"|"LT"|"GE"|"GT",
                Size = 123,
                TextTransformations = list(
                  list(
                    Priority = 123,
                    Type = "NONE"|"COMPRESS_WHITE_SPACE"|"HTML_ENTITY_DECODE"|"LOWERCASE"|"CMD_LINE"|"URL_DECODE"|"BASE64_DECODE"|"HEX_DECODE"|"MD5"|"REPLACE_COMMENTS"|"ESCAPE_SEQ_DECODE"|"SQL_HEX_DECODE"|"CSS_DECODE"|"JS_DECODE"|"NORMALIZE_PATH"|"NORMALIZE_PATH_WIN"|"REMOVE_NULLS"|"REPLACE_NULLS"|"BASE64_DECODE_EXT"|"URL_DECODE_UNI"|"UTF8_TO_UNICODE"
                  )
                )
              ),
              GeoMatchStatement = list(
                CountryCodes = list(
                  "AF"|"AX"|"AL"|"DZ"|"AS"|"AD"|"AO"|"AI"|"AQ"|"AG"|"AR"|"AM"|"AW"|"AU"|"AT"|"AZ"|"BS"|"BH"|"BD"|"BB"|"BY"|"BE"|"BZ"|"BJ"|"BM"|"BT"|"BO"|"BQ"|"BA"|"BW"|"BV"|"BR"|"IO"|"BN"|"BG"|"BF"|"BI"|"KH"|"CM"|"CA"|"CV"|"KY"|"CF"|"TD"|"CL"|"CN"|"CX"|"CC"|"CO"|"KM"|"CG"|"CD"|"CK"|"CR"|"CI"|"HR"|"CU"|"CW"|"CY"|"CZ"|"DK"|"DJ"|"DM"|"DO"|"EC"|"EG"|"SV"|"GQ"|"ER"|"EE"|"ET"|"FK"|"FO"|"FJ"|"FI"|"FR"|"GF"|"PF"|"TF"|"GA"|"GM"|"GE"|"DE"|"GH"|"GI"|"GR"|"GL"|"GD"|"GP"|"GU"|"GT"|"GG"|"GN"|"GW"|"GY"|"HT"|"HM"|"VA"|"HN"|"HK"|"HU"|"IS"|"IN"|"ID"|"IR"|"IQ"|"IE"|"IM"|"IL"|"IT"|"JM"|"JP"|"JE"|"JO"|"KZ"|"KE"|"KI"|"KP"|"KR"|"KW"|"KG"|"LA"|"LV"|"LB"|"LS"|"LR"|"LY"|"LI"|"LT"|"LU"|"MO"|"MK"|"MG"|"MW"|"MY"|"MV"|"ML"|"MT"|"MH"|"MQ"|"MR"|"MU"|"YT"|"MX"|"FM"|"MD"|"MC"|"MN"|"ME"|"MS"|"MA"|"MZ"|"MM"|"NA"|"NR"|"NP"|"NL"|"NC"|"NZ"|"NI"|"NE"|"NG"|"NU"|"NF"|"MP"|"NO"|"OM"|"PK"|"PW"|"PS"|"PA"|"PG"|"PY"|"PE"|"PH"|"PN"|"PL"|"PT"|"PR"|"QA"|"RE"|"RO"|"RU"|"RW"|"BL"|"SH"|"KN"|"LC"|"MF"|"PM"|"VC"|"WS"|"SM"|"ST"|"SA"|"SN"|"RS"|"SC"|"SL"|"SG"|"SX"|"SK"|"SI"|"SB"|"SO"|"ZA"|"GS"|"SS"|"ES"|"LK"|"SD"|"SR"|"SJ"|"SZ"|"SE"|"CH"|"SY"|"TW"|"TJ"|"TZ"|"TH"|"TL"|"TG"|"TK"|"TO"|"TT"|"TN"|"TR"|"TM"|"TC"|"TV"|"UG"|"UA"|"AE"|"GB"|"US"|"UM"|"UY"|"UZ"|"VU"|"VE"|"VN"|"VG"|"VI"|"WF"|"EH"|"YE"|"ZM"|"ZW"|"XK"
                ),
                ForwardedIPConfig = list(
                  HeaderName = "string",
                  FallbackBehavior = "MATCH"|"NO_MATCH"
                )
              ),
              RuleGroupReferenceStatement = list(
                ARN = "string",
                ExcludedRules = list(
                  list(
                    Name = "string"
                  )
                ),
                RuleActionOverrides = list(
                  list(
                    Name = "string",
                    ActionToUse = list(
                      Block = list(
                        CustomResponse = list(
                          ResponseCode = 123,
                          CustomResponseBodyKey = "string",
                          ResponseHeaders = list(
                            list(
                              Name = "string",
                              Value = "string"
                            )
                          )
                        )
                      ),
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
                      Count = list(
                        CustomRequestHandling = list(
                          InsertHeaders = list(
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
                      Challenge = list(
                        CustomRequestHandling = list(
                          InsertHeaders = list(
                            list(
                              Name = "string",
                              Value = "string"
                            )
                          )
                        )
                      )
                    )
                  )
                )
              ),
              IPSetReferenceStatement = list(
                ARN = "string",
                IPSetForwardedIPConfig = list(
                  HeaderName = "string",
                  FallbackBehavior = "MATCH"|"NO_MATCH",
                  Position = "FIRST"|"LAST"|"ANY"
                )
              ),
              RegexPatternSetReferenceStatement = list(
                ARN = "string",
                FieldToMatch = list(
                  SingleHeader = list(
                    Name = "string"
                  ),
                  SingleQueryArgument = list(
                    Name = "string"
                  ),
                  AllQueryArguments = list(),
                  UriPath = list(),
                  QueryString = list(),
                  Body = list(
                    OversizeHandling = "CONTINUE"|"MATCH"|"NO_MATCH"
                  ),
                  Method = list(),
                  JsonBody = list(
                    MatchPattern = list(
                      All = list(),
                      IncludedPaths = list(
                        "string"
                      )
                    ),
                    MatchScope = "ALL"|"KEY"|"VALUE",
                    InvalidFallbackBehavior = "MATCH"|"NO_MATCH"|"EVALUATE_AS_STRING",
                    OversizeHandling = "CONTINUE"|"MATCH"|"NO_MATCH"
                  ),
                  Headers = list(
                    MatchPattern = list(
                      All = list(),
                      IncludedHeaders = list(
                        "string"
                      ),
                      ExcludedHeaders = list(
                        "string"
                      )
                    ),
                    MatchScope = "ALL"|"KEY"|"VALUE",
                    OversizeHandling = "CONTINUE"|"MATCH"|"NO_MATCH"
                  ),
                  Cookies = list(
                    MatchPattern = list(
                      All = list(),
                      IncludedCookies = list(
                        "string"
                      ),
                      ExcludedCookies = list(
                        "string"
                      )
                    ),
                    MatchScope = "ALL"|"KEY"|"VALUE",
                    OversizeHandling = "CONTINUE"|"MATCH"|"NO_MATCH"
                  )
                ),
                TextTransformations = list(
                  list(
                    Priority = 123,
                    Type = "NONE"|"COMPRESS_WHITE_SPACE"|"HTML_ENTITY_DECODE"|"LOWERCASE"|"CMD_LINE"|"URL_DECODE"|"BASE64_DECODE"|"HEX_DECODE"|"MD5"|"REPLACE_COMMENTS"|"ESCAPE_SEQ_DECODE"|"SQL_HEX_DECODE"|"CSS_DECODE"|"JS_DECODE"|"NORMALIZE_PATH"|"NORMALIZE_PATH_WIN"|"REMOVE_NULLS"|"REPLACE_NULLS"|"BASE64_DECODE_EXT"|"URL_DECODE_UNI"|"UTF8_TO_UNICODE"
                  )
                )
              ),
              RateBasedStatement = list(
                Limit = 123,
                AggregateKeyType = "IP"|"FORWARDED_IP"|"CUSTOM_KEYS"|"CONSTANT",
                ScopeDownStatement = list(),
                ForwardedIPConfig = list(
                  HeaderName = "string",
                  FallbackBehavior = "MATCH"|"NO_MATCH"
                ),
                CustomKeys = list(
                  list(
                    Header = list(
                      Name = "string",
                      TextTransformations = list(
                        list(
                          Priority = 123,
                          Type = "NONE"|"COMPRESS_WHITE_SPACE"|"HTML_ENTITY_DECODE"|"LOWERCASE"|"CMD_LINE"|"URL_DECODE"|"BASE64_DECODE"|"HEX_DECODE"|"MD5"|"REPLACE_COMMENTS"|"ESCAPE_SEQ_DECODE"|"SQL_HEX_DECODE"|"CSS_DECODE"|"JS_DECODE"|"NORMALIZE_PATH"|"NORMALIZE_PATH_WIN"|"REMOVE_NULLS"|"REPLACE_NULLS"|"BASE64_DECODE_EXT"|"URL_DECODE_UNI"|"UTF8_TO_UNICODE"
                        )
                      )
                    ),
                    Cookie = list(
                      Name = "string",
                      TextTransformations = list(
                        list(
                          Priority = 123,
                          Type = "NONE"|"COMPRESS_WHITE_SPACE"|"HTML_ENTITY_DECODE"|"LOWERCASE"|"CMD_LINE"|"URL_DECODE"|"BASE64_DECODE"|"HEX_DECODE"|"MD5"|"REPLACE_COMMENTS"|"ESCAPE_SEQ_DECODE"|"SQL_HEX_DECODE"|"CSS_DECODE"|"JS_DECODE"|"NORMALIZE_PATH"|"NORMALIZE_PATH_WIN"|"REMOVE_NULLS"|"REPLACE_NULLS"|"BASE64_DECODE_EXT"|"URL_DECODE_UNI"|"UTF8_TO_UNICODE"
                        )
                      )
                    ),
                    QueryArgument = list(
                      Name = "string",
                      TextTransformations = list(
                        list(
                          Priority = 123,
                          Type = "NONE"|"COMPRESS_WHITE_SPACE"|"HTML_ENTITY_DECODE"|"LOWERCASE"|"CMD_LINE"|"URL_DECODE"|"BASE64_DECODE"|"HEX_DECODE"|"MD5"|"REPLACE_COMMENTS"|"ESCAPE_SEQ_DECODE"|"SQL_HEX_DECODE"|"CSS_DECODE"|"JS_DECODE"|"NORMALIZE_PATH"|"NORMALIZE_PATH_WIN"|"REMOVE_NULLS"|"REPLACE_NULLS"|"BASE64_DECODE_EXT"|"URL_DECODE_UNI"|"UTF8_TO_UNICODE"
                        )
                      )
                    ),
                    QueryString = list(
                      TextTransformations = list(
                        list(
                          Priority = 123,
                          Type = "NONE"|"COMPRESS_WHITE_SPACE"|"HTML_ENTITY_DECODE"|"LOWERCASE"|"CMD_LINE"|"URL_DECODE"|"BASE64_DECODE"|"HEX_DECODE"|"MD5"|"REPLACE_COMMENTS"|"ESCAPE_SEQ_DECODE"|"SQL_HEX_DECODE"|"CSS_DECODE"|"JS_DECODE"|"NORMALIZE_PATH"|"NORMALIZE_PATH_WIN"|"REMOVE_NULLS"|"REPLACE_NULLS"|"BASE64_DECODE_EXT"|"URL_DECODE_UNI"|"UTF8_TO_UNICODE"
                        )
                      )
                    ),
                    HTTPMethod = list(),
                    ForwardedIP = list(),
                    IP = list(),
                    LabelNamespace = list(
                      Namespace = "string"
                    )
                  )
                )
              ),
              AndStatement = list(
                Statements = list(
                  list()
                )
              ),
              OrStatement = list(
                Statements = list(
                  list()
                )
              ),
              NotStatement = list(
                Statement = list()
              ),
              ManagedRuleGroupStatement = list(
                VendorName = "string",
                Name = "string",
                Version = "string",
                ExcludedRules = list(
                  list(
                    Name = "string"
                  )
                ),
                ScopeDownStatement = list(),
                ManagedRuleGroupConfigs = list(
                  list(
                    LoginPath = "string",
                    PayloadType = "JSON"|"FORM_ENCODED",
                    UsernameField = list(
                      Identifier = "string"
                    ),
                    PasswordField = list(
                      Identifier = "string"
                    ),
                    AWSManagedRulesBotControlRuleSet = list(
                      InspectionLevel = "COMMON"|"TARGETED"
                    ),
                    AWSManagedRulesATPRuleSet = list(
                      LoginPath = "string",
                      RequestInspection = list(
                        PayloadType = "JSON"|"FORM_ENCODED",
                        UsernameField = list(
                          Identifier = "string"
                        ),
                        PasswordField = list(
                          Identifier = "string"
                        )
                      ),
                      ResponseInspection = list(
                        StatusCode = list(
                          SuccessCodes = list(
                            123
                          ),
                          FailureCodes = list(
                            123
                          )
                        ),
                        Header = list(
                          Name = "string",
                          SuccessValues = list(
                            "string"
                          ),
                          FailureValues = list(
                            "string"
                          )
                        ),
                        BodyContains = list(
                          SuccessStrings = list(
                            "string"
                          ),
                          FailureStrings = list(
                            "string"
                          )
                        ),
                        Json = list(
                          Identifier = "string",
                          SuccessValues = list(
                            "string"
                          ),
                          FailureValues = list(
                            "string"
                          )
                        )
                      )
                    )
                  )
                ),
                RuleActionOverrides = list(
                  list(
                    Name = "string",
                    ActionToUse = list(
                      Block = list(
                        CustomResponse = list(
                          ResponseCode = 123,
                          CustomResponseBodyKey = "string",
                          ResponseHeaders = list(
                            list(
                              Name = "string",
                              Value = "string"
                            )
                          )
                        )
                      ),
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
                      Count = list(
                        CustomRequestHandling = list(
                          InsertHeaders = list(
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
                      Challenge = list(
                        CustomRequestHandling = list(
                          InsertHeaders = list(
                            list(
                              Name = "string",
                              Value = "string"
                            )
                          )
                        )
                      )
                    )
                  )
                )
              ),
              LabelMatchStatement = list(
                Scope = "LABEL"|"NAMESPACE",
                Key = "string"
              ),
              RegexMatchStatement = list(
                RegexString = "string",
                FieldToMatch = list(
                  SingleHeader = list(
                    Name = "string"
                  ),
                  SingleQueryArgument = list(
                    Name = "string"
                  ),
                  AllQueryArguments = list(),
                  UriPath = list(),
                  QueryString = list(),
                  Body = list(
                    OversizeHandling = "CONTINUE"|"MATCH"|"NO_MATCH"
                  ),
                  Method = list(),
                  JsonBody = list(
                    MatchPattern = list(
                      All = list(),
                      IncludedPaths = list(
                        "string"
                      )
                    ),
                    MatchScope = "ALL"|"KEY"|"VALUE",
                    InvalidFallbackBehavior = "MATCH"|"NO_MATCH"|"EVALUATE_AS_STRING",
                    OversizeHandling = "CONTINUE"|"MATCH"|"NO_MATCH"
                  ),
                  Headers = list(
                    MatchPattern = list(
                      All = list(),
                      IncludedHeaders = list(
                        "string"
                      ),
                      ExcludedHeaders = list(
                        "string"
                      )
                    ),
                    MatchScope = "ALL"|"KEY"|"VALUE",
                    OversizeHandling = "CONTINUE"|"MATCH"|"NO_MATCH"
                  ),
                  Cookies = list(
                    MatchPattern = list(
                      All = list(),
                      IncludedCookies = list(
                        "string"
                      ),
                      ExcludedCookies = list(
                        "string"
                      )
                    ),
                    MatchScope = "ALL"|"KEY"|"VALUE",
                    OversizeHandling = "CONTINUE"|"MATCH"|"NO_MATCH"
                  )
                ),
                TextTransformations = list(
                  list(
                    Priority = 123,
                    Type = "NONE"|"COMPRESS_WHITE_SPACE"|"HTML_ENTITY_DECODE"|"LOWERCASE"|"CMD_LINE"|"URL_DECODE"|"BASE64_DECODE"|"HEX_DECODE"|"MD5"|"REPLACE_COMMENTS"|"ESCAPE_SEQ_DECODE"|"SQL_HEX_DECODE"|"CSS_DECODE"|"JS_DECODE"|"NORMALIZE_PATH"|"NORMALIZE_PATH_WIN"|"REMOVE_NULLS"|"REPLACE_NULLS"|"BASE64_DECODE_EXT"|"URL_DECODE_UNI"|"UTF8_TO_UNICODE"
                  )
                )
              )
            ),
            Action = list(
              Block = list(
                CustomResponse = list(
                  ResponseCode = 123,
                  CustomResponseBodyKey = "string",
                  ResponseHeaders = list(
                    list(
                      Name = "string",
                      Value = "string"
                    )
                  )
                )
              ),
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
              Count = list(
                CustomRequestHandling = list(
                  InsertHeaders = list(
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
              Challenge = list(
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
            OverrideAction = list(
              Count = list(
                CustomRequestHandling = list(
                  InsertHeaders = list(
                    list(
                      Name = "string",
                      Value = "string"
                    )
                  )
                )
              ),
              None = list()
            ),
            RuleLabels = list(
              list(
                Name = "string"
              )
            ),
            VisibilityConfig = list(
              SampledRequestsEnabled = TRUE|FALSE,
              CloudWatchMetricsEnabled = TRUE|FALSE,
              MetricName = "string"
            ),
            CaptchaConfig = list(
              ImmunityTimeProperty = list(
                ImmunityTime = 123
              )
            ),
            ChallengeConfig = list(
              ImmunityTimeProperty = list(
                ImmunityTime = 123
              )
            )
          )
        ),
        VisibilityConfig = list(
          SampledRequestsEnabled = TRUE|FALSE,
          CloudWatchMetricsEnabled = TRUE|FALSE,
          MetricName = "string"
        ),
        Capacity = 123,
        PreProcessFirewallManagerRuleGroups = list(
          list(
            Name = "string",
            Priority = 123,
            FirewallManagerStatement = list(
              ManagedRuleGroupStatement = list(
                VendorName = "string",
                Name = "string",
                Version = "string",
                ExcludedRules = list(
                  list(
                    Name = "string"
                  )
                ),
                ScopeDownStatement = list(
                  ByteMatchStatement = list(
                    SearchString = raw,
                    FieldToMatch = list(
                      SingleHeader = list(
                        Name = "string"
                      ),
                      SingleQueryArgument = list(
                        Name = "string"
                      ),
                      AllQueryArguments = list(),
                      UriPath = list(),
                      QueryString = list(),
                      Body = list(
                        OversizeHandling = "CONTINUE"|"MATCH"|"NO_MATCH"
                      ),
                      Method = list(),
                      JsonBody = list(
                        MatchPattern = list(
                          All = list(),
                          IncludedPaths = list(
                            "string"
                          )
                        ),
                        MatchScope = "ALL"|"KEY"|"VALUE",
                        InvalidFallbackBehavior = "MATCH"|"NO_MATCH"|"EVALUATE_AS_STRING",
                        OversizeHandling = "CONTINUE"|"MATCH"|"NO_MATCH"
                      ),
                      Headers = list(
                        MatchPattern = list(
                          All = list(),
                          IncludedHeaders = list(
                            "string"
                          ),
                          ExcludedHeaders = list(
                            "string"
                          )
                        ),
                        MatchScope = "ALL"|"KEY"|"VALUE",
                        OversizeHandling = "CONTINUE"|"MATCH"|"NO_MATCH"
                      ),
                      Cookies = list(
                        MatchPattern = list(
                          All = list(),
                          IncludedCookies = list(
                            "string"
                          ),
                          ExcludedCookies = list(
                            "string"
                          )
                        ),
                        MatchScope = "ALL"|"KEY"|"VALUE",
                        OversizeHandling = "CONTINUE"|"MATCH"|"NO_MATCH"
                      )
                    ),
                    TextTransformations = list(
                      list(
                        Priority = 123,
                        Type = "NONE"|"COMPRESS_WHITE_SPACE"|"HTML_ENTITY_DECODE"|"LOWERCASE"|"CMD_LINE"|"URL_DECODE"|"BASE64_DECODE"|"HEX_DECODE"|"MD5"|"REPLACE_COMMENTS"|"ESCAPE_SEQ_DECODE"|"SQL_HEX_DECODE"|"CSS_DECODE"|"JS_DECODE"|"NORMALIZE_PATH"|"NORMALIZE_PATH_WIN"|"REMOVE_NULLS"|"REPLACE_NULLS"|"BASE64_DECODE_EXT"|"URL_DECODE_UNI"|"UTF8_TO_UNICODE"
                      )
                    ),
                    PositionalConstraint = "EXACTLY"|"STARTS_WITH"|"ENDS_WITH"|"CONTAINS"|"CONTAINS_WORD"
                  ),
                  SqliMatchStatement = list(
                    FieldToMatch = list(
                      SingleHeader = list(
                        Name = "string"
                      ),
                      SingleQueryArgument = list(
                        Name = "string"
                      ),
                      AllQueryArguments = list(),
                      UriPath = list(),
                      QueryString = list(),
                      Body = list(
                        OversizeHandling = "CONTINUE"|"MATCH"|"NO_MATCH"
                      ),
                      Method = list(),
                      JsonBody = list(
                        MatchPattern = list(
                          All = list(),
                          IncludedPaths = list(
                            "string"
                          )
                        ),
                        MatchScope = "ALL"|"KEY"|"VALUE",
                        InvalidFallbackBehavior = "MATCH"|"NO_MATCH"|"EVALUATE_AS_STRING",
                        OversizeHandling = "CONTINUE"|"MATCH"|"NO_MATCH"
                      ),
                      Headers = list(
                        MatchPattern = list(
                          All = list(),
                          IncludedHeaders = list(
                            "string"
                          ),
                          ExcludedHeaders = list(
                            "string"
                          )
                        ),
                        MatchScope = "ALL"|"KEY"|"VALUE",
                        OversizeHandling = "CONTINUE"|"MATCH"|"NO_MATCH"
                      ),
                      Cookies = list(
                        MatchPattern = list(
                          All = list(),
                          IncludedCookies = list(
                            "string"
                          ),
                          ExcludedCookies = list(
                            "string"
                          )
                        ),
                        MatchScope = "ALL"|"KEY"|"VALUE",
                        OversizeHandling = "CONTINUE"|"MATCH"|"NO_MATCH"
                      )
                    ),
                    TextTransformations = list(
                      list(
                        Priority = 123,
                        Type = "NONE"|"COMPRESS_WHITE_SPACE"|"HTML_ENTITY_DECODE"|"LOWERCASE"|"CMD_LINE"|"URL_DECODE"|"BASE64_DECODE"|"HEX_DECODE"|"MD5"|"REPLACE_COMMENTS"|"ESCAPE_SEQ_DECODE"|"SQL_HEX_DECODE"|"CSS_DECODE"|"JS_DECODE"|"NORMALIZE_PATH"|"NORMALIZE_PATH_WIN"|"REMOVE_NULLS"|"REPLACE_NULLS"|"BASE64_DECODE_EXT"|"URL_DECODE_UNI"|"UTF8_TO_UNICODE"
                      )
                    ),
                    SensitivityLevel = "LOW"|"HIGH"
                  ),
                  XssMatchStatement = list(
                    FieldToMatch = list(
                      SingleHeader = list(
                        Name = "string"
                      ),
                      SingleQueryArgument = list(
                        Name = "string"
                      ),
                      AllQueryArguments = list(),
                      UriPath = list(),
                      QueryString = list(),
                      Body = list(
                        OversizeHandling = "CONTINUE"|"MATCH"|"NO_MATCH"
                      ),
                      Method = list(),
                      JsonBody = list(
                        MatchPattern = list(
                          All = list(),
                          IncludedPaths = list(
                            "string"
                          )
                        ),
                        MatchScope = "ALL"|"KEY"|"VALUE",
                        InvalidFallbackBehavior = "MATCH"|"NO_MATCH"|"EVALUATE_AS_STRING",
                        OversizeHandling = "CONTINUE"|"MATCH"|"NO_MATCH"
                      ),
                      Headers = list(
                        MatchPattern = list(
                          All = list(),
                          IncludedHeaders = list(
                            "string"
                          ),
                          ExcludedHeaders = list(
                            "string"
                          )
                        ),
                        MatchScope = "ALL"|"KEY"|"VALUE",
                        OversizeHandling = "CONTINUE"|"MATCH"|"NO_MATCH"
                      ),
                      Cookies = list(
                        MatchPattern = list(
                          All = list(),
                          IncludedCookies = list(
                            "string"
                          ),
                          ExcludedCookies = list(
                            "string"
                          )
                        ),
                        MatchScope = "ALL"|"KEY"|"VALUE",
                        OversizeHandling = "CONTINUE"|"MATCH"|"NO_MATCH"
                      )
                    ),
                    TextTransformations = list(
                      list(
                        Priority = 123,
                        Type = "NONE"|"COMPRESS_WHITE_SPACE"|"HTML_ENTITY_DECODE"|"LOWERCASE"|"CMD_LINE"|"URL_DECODE"|"BASE64_DECODE"|"HEX_DECODE"|"MD5"|"REPLACE_COMMENTS"|"ESCAPE_SEQ_DECODE"|"SQL_HEX_DECODE"|"CSS_DECODE"|"JS_DECODE"|"NORMALIZE_PATH"|"NORMALIZE_PATH_WIN"|"REMOVE_NULLS"|"REPLACE_NULLS"|"BASE64_DECODE_EXT"|"URL_DECODE_UNI"|"UTF8_TO_UNICODE"
                      )
                    )
                  ),
                  SizeConstraintStatement = list(
                    FieldToMatch = list(
                      SingleHeader = list(
                        Name = "string"
                      ),
                      SingleQueryArgument = list(
                        Name = "string"
                      ),
                      AllQueryArguments = list(),
                      UriPath = list(),
                      QueryString = list(),
                      Body = list(
                        OversizeHandling = "CONTINUE"|"MATCH"|"NO_MATCH"
                      ),
                      Method = list(),
                      JsonBody = list(
                        MatchPattern = list(
                          All = list(),
                          IncludedPaths = list(
                            "string"
                          )
                        ),
                        MatchScope = "ALL"|"KEY"|"VALUE",
                        InvalidFallbackBehavior = "MATCH"|"NO_MATCH"|"EVALUATE_AS_STRING",
                        OversizeHandling = "CONTINUE"|"MATCH"|"NO_MATCH"
                      ),
                      Headers = list(
                        MatchPattern = list(
                          All = list(),
                          IncludedHeaders = list(
                            "string"
                          ),
                          ExcludedHeaders = list(
                            "string"
                          )
                        ),
                        MatchScope = "ALL"|"KEY"|"VALUE",
                        OversizeHandling = "CONTINUE"|"MATCH"|"NO_MATCH"
                      ),
                      Cookies = list(
                        MatchPattern = list(
                          All = list(),
                          IncludedCookies = list(
                            "string"
                          ),
                          ExcludedCookies = list(
                            "string"
                          )
                        ),
                        MatchScope = "ALL"|"KEY"|"VALUE",
                        OversizeHandling = "CONTINUE"|"MATCH"|"NO_MATCH"
                      )
                    ),
                    ComparisonOperator = "EQ"|"NE"|"LE"|"LT"|"GE"|"GT",
                    Size = 123,
                    TextTransformations = list(
                      list(
                        Priority = 123,
                        Type = "NONE"|"COMPRESS_WHITE_SPACE"|"HTML_ENTITY_DECODE"|"LOWERCASE"|"CMD_LINE"|"URL_DECODE"|"BASE64_DECODE"|"HEX_DECODE"|"MD5"|"REPLACE_COMMENTS"|"ESCAPE_SEQ_DECODE"|"SQL_HEX_DECODE"|"CSS_DECODE"|"JS_DECODE"|"NORMALIZE_PATH"|"NORMALIZE_PATH_WIN"|"REMOVE_NULLS"|"REPLACE_NULLS"|"BASE64_DECODE_EXT"|"URL_DECODE_UNI"|"UTF8_TO_UNICODE"
                      )
                    )
                  ),
                  GeoMatchStatement = list(
                    CountryCodes = list(
                      "AF"|"AX"|"AL"|"DZ"|"AS"|"AD"|"AO"|"AI"|"AQ"|"AG"|"AR"|"AM"|"AW"|"AU"|"AT"|"AZ"|"BS"|"BH"|"BD"|"BB"|"BY"|"BE"|"BZ"|"BJ"|"BM"|"BT"|"BO"|"BQ"|"BA"|"BW"|"BV"|"BR"|"IO"|"BN"|"BG"|"BF"|"BI"|"KH"|"CM"|"CA"|"CV"|"KY"|"CF"|"TD"|"CL"|"CN"|"CX"|"CC"|"CO"|"KM"|"CG"|"CD"|"CK"|"CR"|"CI"|"HR"|"CU"|"CW"|"CY"|"CZ"|"DK"|"DJ"|"DM"|"DO"|"EC"|"EG"|"SV"|"GQ"|"ER"|"EE"|"ET"|"FK"|"FO"|"FJ"|"FI"|"FR"|"GF"|"PF"|"TF"|"GA"|"GM"|"GE"|"DE"|"GH"|"GI"|"GR"|"GL"|"GD"|"GP"|"GU"|"GT"|"GG"|"GN"|"GW"|"GY"|"HT"|"HM"|"VA"|"HN"|"HK"|"HU"|"IS"|"IN"|"ID"|"IR"|"IQ"|"IE"|"IM"|"IL"|"IT"|"JM"|"JP"|"JE"|"JO"|"KZ"|"KE"|"KI"|"KP"|"KR"|"KW"|"KG"|"LA"|"LV"|"LB"|"LS"|"LR"|"LY"|"LI"|"LT"|"LU"|"MO"|"MK"|"MG"|"MW"|"MY"|"MV"|"ML"|"MT"|"MH"|"MQ"|"MR"|"MU"|"YT"|"MX"|"FM"|"MD"|"MC"|"MN"|"ME"|"MS"|"MA"|"MZ"|"MM"|"NA"|"NR"|"NP"|"NL"|"NC"|"NZ"|"NI"|"NE"|"NG"|"NU"|"NF"|"MP"|"NO"|"OM"|"PK"|"PW"|"PS"|"PA"|"PG"|"PY"|"PE"|"PH"|"PN"|"PL"|"PT"|"PR"|"QA"|"RE"|"RO"|"RU"|"RW"|"BL"|"SH"|"KN"|"LC"|"MF"|"PM"|"VC"|"WS"|"SM"|"ST"|"SA"|"SN"|"RS"|"SC"|"SL"|"SG"|"SX"|"SK"|"SI"|"SB"|"SO"|"ZA"|"GS"|"SS"|"ES"|"LK"|"SD"|"SR"|"SJ"|"SZ"|"SE"|"CH"|"SY"|"TW"|"TJ"|"TZ"|"TH"|"TL"|"TG"|"TK"|"TO"|"TT"|"TN"|"TR"|"TM"|"TC"|"TV"|"UG"|"UA"|"AE"|"GB"|"US"|"UM"|"UY"|"UZ"|"VU"|"VE"|"VN"|"VG"|"VI"|"WF"|"EH"|"YE"|"ZM"|"ZW"|"XK"
                    ),
                    ForwardedIPConfig = list(
                      HeaderName = "string",
                      FallbackBehavior = "MATCH"|"NO_MATCH"
                    )
                  ),
                  RuleGroupReferenceStatement = list(
                    ARN = "string",
                    ExcludedRules = list(
                      list(
                        Name = "string"
                      )
                    ),
                    RuleActionOverrides = list(
                      list(
                        Name = "string",
                        ActionToUse = list(
                          Block = list(
                            CustomResponse = list(
                              ResponseCode = 123,
                              CustomResponseBodyKey = "string",
                              ResponseHeaders = list(
                                list(
                                  Name = "string",
                                  Value = "string"
                                )
                              )
                            )
                          ),
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
                          Count = list(
                            CustomRequestHandling = list(
                              InsertHeaders = list(
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
                          Challenge = list(
                            CustomRequestHandling = list(
                              InsertHeaders = list(
                                list(
                                  Name = "string",
                                  Value = "string"
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  ),
                  IPSetReferenceStatement = list(
                    ARN = "string",
                    IPSetForwardedIPConfig = list(
                      HeaderName = "string",
                      FallbackBehavior = "MATCH"|"NO_MATCH",
                      Position = "FIRST"|"LAST"|"ANY"
                    )
                  ),
                  RegexPatternSetReferenceStatement = list(
                    ARN = "string",
                    FieldToMatch = list(
                      SingleHeader = list(
                        Name = "string"
                      ),
                      SingleQueryArgument = list(
                        Name = "string"
                      ),
                      AllQueryArguments = list(),
                      UriPath = list(),
                      QueryString = list(),
                      Body = list(
                        OversizeHandling = "CONTINUE"|"MATCH"|"NO_MATCH"
                      ),
                      Method = list(),
                      JsonBody = list(
                        MatchPattern = list(
                          All = list(),
                          IncludedPaths = list(
                            "string"
                          )
                        ),
                        MatchScope = "ALL"|"KEY"|"VALUE",
                        InvalidFallbackBehavior = "MATCH"|"NO_MATCH"|"EVALUATE_AS_STRING",
                        OversizeHandling = "CONTINUE"|"MATCH"|"NO_MATCH"
                      ),
                      Headers = list(
                        MatchPattern = list(
                          All = list(),
                          IncludedHeaders = list(
                            "string"
                          ),
                          ExcludedHeaders = list(
                            "string"
                          )
                        ),
                        MatchScope = "ALL"|"KEY"|"VALUE",
                        OversizeHandling = "CONTINUE"|"MATCH"|"NO_MATCH"
                      ),
                      Cookies = list(
                        MatchPattern = list(
                          All = list(),
                          IncludedCookies = list(
                            "string"
                          ),
                          ExcludedCookies = list(
                            "string"
                          )
                        ),
                        MatchScope = "ALL"|"KEY"|"VALUE",
                        OversizeHandling = "CONTINUE"|"MATCH"|"NO_MATCH"
                      )
                    ),
                    TextTransformations = list(
                      list(
                        Priority = 123,
                        Type = "NONE"|"COMPRESS_WHITE_SPACE"|"HTML_ENTITY_DECODE"|"LOWERCASE"|"CMD_LINE"|"URL_DECODE"|"BASE64_DECODE"|"HEX_DECODE"|"MD5"|"REPLACE_COMMENTS"|"ESCAPE_SEQ_DECODE"|"SQL_HEX_DECODE"|"CSS_DECODE"|"JS_DECODE"|"NORMALIZE_PATH"|"NORMALIZE_PATH_WIN"|"REMOVE_NULLS"|"REPLACE_NULLS"|"BASE64_DECODE_EXT"|"URL_DECODE_UNI"|"UTF8_TO_UNICODE"
                      )
                    )
                  ),
                  RateBasedStatement = list(
                    Limit = 123,
                    AggregateKeyType = "IP"|"FORWARDED_IP"|"CUSTOM_KEYS"|"CONSTANT",
                    ScopeDownStatement = list(),
                    ForwardedIPConfig = list(
                      HeaderName = "string",
                      FallbackBehavior = "MATCH"|"NO_MATCH"
                    ),
                    CustomKeys = list(
                      list(
                        Header = list(
                          Name = "string",
                          TextTransformations = list(
                            list(
                              Priority = 123,
                              Type = "NONE"|"COMPRESS_WHITE_SPACE"|"HTML_ENTITY_DECODE"|"LOWERCASE"|"CMD_LINE"|"URL_DECODE"|"BASE64_DECODE"|"HEX_DECODE"|"MD5"|"REPLACE_COMMENTS"|"ESCAPE_SEQ_DECODE"|"SQL_HEX_DECODE"|"CSS_DECODE"|"JS_DECODE"|"NORMALIZE_PATH"|"NORMALIZE_PATH_WIN"|"REMOVE_NULLS"|"REPLACE_NULLS"|"BASE64_DECODE_EXT"|"URL_DECODE_UNI"|"UTF8_TO_UNICODE"
                            )
                          )
                        ),
                        Cookie = list(
                          Name = "string",
                          TextTransformations = list(
                            list(
                              Priority = 123,
                              Type = "NONE"|"COMPRESS_WHITE_SPACE"|"HTML_ENTITY_DECODE"|"LOWERCASE"|"CMD_LINE"|"URL_DECODE"|"BASE64_DECODE"|"HEX_DECODE"|"MD5"|"REPLACE_COMMENTS"|"ESCAPE_SEQ_DECODE"|"SQL_HEX_DECODE"|"CSS_DECODE"|"JS_DECODE"|"NORMALIZE_PATH"|"NORMALIZE_PATH_WIN"|"REMOVE_NULLS"|"REPLACE_NULLS"|"BASE64_DECODE_EXT"|"URL_DECODE_UNI"|"UTF8_TO_UNICODE"
                            )
                          )
                        ),
                        QueryArgument = list(
                          Name = "string",
                          TextTransformations = list(
                            list(
                              Priority = 123,
                              Type = "NONE"|"COMPRESS_WHITE_SPACE"|"HTML_ENTITY_DECODE"|"LOWERCASE"|"CMD_LINE"|"URL_DECODE"|"BASE64_DECODE"|"HEX_DECODE"|"MD5"|"REPLACE_COMMENTS"|"ESCAPE_SEQ_DECODE"|"SQL_HEX_DECODE"|"CSS_DECODE"|"JS_DECODE"|"NORMALIZE_PATH"|"NORMALIZE_PATH_WIN"|"REMOVE_NULLS"|"REPLACE_NULLS"|"BASE64_DECODE_EXT"|"URL_DECODE_UNI"|"UTF8_TO_UNICODE"
                            )
                          )
                        ),
                        QueryString = list(
                          TextTransformations = list(
                            list(
                              Priority = 123,
                              Type = "NONE"|"COMPRESS_WHITE_SPACE"|"HTML_ENTITY_DECODE"|"LOWERCASE"|"CMD_LINE"|"URL_DECODE"|"BASE64_DECODE"|"HEX_DECODE"|"MD5"|"REPLACE_COMMENTS"|"ESCAPE_SEQ_DECODE"|"SQL_HEX_DECODE"|"CSS_DECODE"|"JS_DECODE"|"NORMALIZE_PATH"|"NORMALIZE_PATH_WIN"|"REMOVE_NULLS"|"REPLACE_NULLS"|"BASE64_DECODE_EXT"|"URL_DECODE_UNI"|"UTF8_TO_UNICODE"
                            )
                          )
                        ),
                        HTTPMethod = list(),
                        ForwardedIP = list(),
                        IP = list(),
                        LabelNamespace = list(
                          Namespace = "string"
                        )
                      )
                    )
                  ),
                  AndStatement = list(
                    Statements = list(
                      list()
                    )
                  ),
                  OrStatement = list(
                    Statements = list(
                      list()
                    )
                  ),
                  NotStatement = list(
                    Statement = list()
                  ),
                  ManagedRuleGroupStatement = list(),
                  LabelMatchStatement = list(
                    Scope = "LABEL"|"NAMESPACE",
                    Key = "string"
                  ),
                  RegexMatchStatement = list(
                    RegexString = "string",
                    FieldToMatch = list(
                      SingleHeader = list(
                        Name = "string"
                      ),
                      SingleQueryArgument = list(
                        Name = "string"
                      ),
                      AllQueryArguments = list(),
                      UriPath = list(),
                      QueryString = list(),
                      Body = list(
                        OversizeHandling = "CONTINUE"|"MATCH"|"NO_MATCH"
                      ),
                      Method = list(),
                      JsonBody = list(
                        MatchPattern = list(
                          All = list(),
                          IncludedPaths = list(
                            "string"
                          )
                        ),
                        MatchScope = "ALL"|"KEY"|"VALUE",
                        InvalidFallbackBehavior = "MATCH"|"NO_MATCH"|"EVALUATE_AS_STRING",
                        OversizeHandling = "CONTINUE"|"MATCH"|"NO_MATCH"
                      ),
                      Headers = list(
                        MatchPattern = list(
                          All = list(),
                          IncludedHeaders = list(
                            "string"
                          ),
                          ExcludedHeaders = list(
                            "string"
                          )
                        ),
                        MatchScope = "ALL"|"KEY"|"VALUE",
                        OversizeHandling = "CONTINUE"|"MATCH"|"NO_MATCH"
                      ),
                      Cookies = list(
                        MatchPattern = list(
                          All = list(),
                          IncludedCookies = list(
                            "string"
                          ),
                          ExcludedCookies = list(
                            "string"
                          )
                        ),
                        MatchScope = "ALL"|"KEY"|"VALUE",
                        OversizeHandling = "CONTINUE"|"MATCH"|"NO_MATCH"
                      )
                    ),
                    TextTransformations = list(
                      list(
                        Priority = 123,
                        Type = "NONE"|"COMPRESS_WHITE_SPACE"|"HTML_ENTITY_DECODE"|"LOWERCASE"|"CMD_LINE"|"URL_DECODE"|"BASE64_DECODE"|"HEX_DECODE"|"MD5"|"REPLACE_COMMENTS"|"ESCAPE_SEQ_DECODE"|"SQL_HEX_DECODE"|"CSS_DECODE"|"JS_DECODE"|"NORMALIZE_PATH"|"NORMALIZE_PATH_WIN"|"REMOVE_NULLS"|"REPLACE_NULLS"|"BASE64_DECODE_EXT"|"URL_DECODE_UNI"|"UTF8_TO_UNICODE"
                      )
                    )
                  )
                ),
                ManagedRuleGroupConfigs = list(
                  list(
                    LoginPath = "string",
                    PayloadType = "JSON"|"FORM_ENCODED",
                    UsernameField = list(
                      Identifier = "string"
                    ),
                    PasswordField = list(
                      Identifier = "string"
                    ),
                    AWSManagedRulesBotControlRuleSet = list(
                      InspectionLevel = "COMMON"|"TARGETED"
                    ),
                    AWSManagedRulesATPRuleSet = list(
                      LoginPath = "string",
                      RequestInspection = list(
                        PayloadType = "JSON"|"FORM_ENCODED",
                        UsernameField = list(
                          Identifier = "string"
                        ),
                        PasswordField = list(
                          Identifier = "string"
                        )
                      ),
                      ResponseInspection = list(
                        StatusCode = list(
                          SuccessCodes = list(
                            123
                          ),
                          FailureCodes = list(
                            123
                          )
                        ),
                        Header = list(
                          Name = "string",
                          SuccessValues = list(
                            "string"
                          ),
                          FailureValues = list(
                            "string"
                          )
                        ),
                        BodyContains = list(
                          SuccessStrings = list(
                            "string"
                          ),
                          FailureStrings = list(
                            "string"
                          )
                        ),
                        Json = list(
                          Identifier = "string",
                          SuccessValues = list(
                            "string"
                          ),
                          FailureValues = list(
                            "string"
                          )
                        )
                      )
                    )
                  )
                ),
                RuleActionOverrides = list(
                  list(
                    Name = "string",
                    ActionToUse = list(
                      Block = list(
                        CustomResponse = list(
                          ResponseCode = 123,
                          CustomResponseBodyKey = "string",
                          ResponseHeaders = list(
                            list(
                              Name = "string",
                              Value = "string"
                            )
                          )
                        )
                      ),
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
                      Count = list(
                        CustomRequestHandling = list(
                          InsertHeaders = list(
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
                      Challenge = list(
                        CustomRequestHandling = list(
                          InsertHeaders = list(
                            list(
                              Name = "string",
                              Value = "string"
                            )
                          )
                        )
                      )
                    )
                  )
                )
              ),
              RuleGroupReferenceStatement = list(
                ARN = "string",
                ExcludedRules = list(
                  list(
                    Name = "string"
                  )
                ),
                RuleActionOverrides = list(
                  list(
                    Name = "string",
                    ActionToUse = list(
                      Block = list(
                        CustomResponse = list(
                          ResponseCode = 123,
                          CustomResponseBodyKey = "string",
                          ResponseHeaders = list(
                            list(
                              Name = "string",
                              Value = "string"
                            )
                          )
                        )
                      ),
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
                      Count = list(
                        CustomRequestHandling = list(
                          InsertHeaders = list(
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
                      Challenge = list(
                        CustomRequestHandling = list(
                          InsertHeaders = list(
                            list(
                              Name = "string",
                              Value = "string"
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            ),
            OverrideAction = list(
              Count = list(
                CustomRequestHandling = list(
                  InsertHeaders = list(
                    list(
                      Name = "string",
                      Value = "string"
                    )
                  )
                )
              ),
              None = list()
            ),
            VisibilityConfig = list(
              SampledRequestsEnabled = TRUE|FALSE,
              CloudWatchMetricsEnabled = TRUE|FALSE,
              MetricName = "string"
            )
          )
        ),
        PostProcessFirewallManagerRuleGroups = list(
          list(
            Name = "string",
            Priority = 123,
            FirewallManagerStatement = list(
              ManagedRuleGroupStatement = list(
                VendorName = "string",
                Name = "string",
                Version = "string",
                ExcludedRules = list(
                  list(
                    Name = "string"
                  )
                ),
                ScopeDownStatement = list(
                  ByteMatchStatement = list(
                    SearchString = raw,
                    FieldToMatch = list(
                      SingleHeader = list(
                        Name = "string"
                      ),
                      SingleQueryArgument = list(
                        Name = "string"
                      ),
                      AllQueryArguments = list(),
                      UriPath = list(),
                      QueryString = list(),
                      Body = list(
                        OversizeHandling = "CONTINUE"|"MATCH"|"NO_MATCH"
                      ),
                      Method = list(),
                      JsonBody = list(
                        MatchPattern = list(
                          All = list(),
                          IncludedPaths = list(
                            "string"
                          )
                        ),
                        MatchScope = "ALL"|"KEY"|"VALUE",
                        InvalidFallbackBehavior = "MATCH"|"NO_MATCH"|"EVALUATE_AS_STRING",
                        OversizeHandling = "CONTINUE"|"MATCH"|"NO_MATCH"
                      ),
                      Headers = list(
                        MatchPattern = list(
                          All = list(),
                          IncludedHeaders = list(
                            "string"
                          ),
                          ExcludedHeaders = list(
                            "string"
                          )
                        ),
                        MatchScope = "ALL"|"KEY"|"VALUE",
                        OversizeHandling = "CONTINUE"|"MATCH"|"NO_MATCH"
                      ),
                      Cookies = list(
                        MatchPattern = list(
                          All = list(),
                          IncludedCookies = list(
                            "string"
                          ),
                          ExcludedCookies = list(
                            "string"
                          )
                        ),
                        MatchScope = "ALL"|"KEY"|"VALUE",
                        OversizeHandling = "CONTINUE"|"MATCH"|"NO_MATCH"
                      )
                    ),
                    TextTransformations = list(
                      list(
                        Priority = 123,
                        Type = "NONE"|"COMPRESS_WHITE_SPACE"|"HTML_ENTITY_DECODE"|"LOWERCASE"|"CMD_LINE"|"URL_DECODE"|"BASE64_DECODE"|"HEX_DECODE"|"MD5"|"REPLACE_COMMENTS"|"ESCAPE_SEQ_DECODE"|"SQL_HEX_DECODE"|"CSS_DECODE"|"JS_DECODE"|"NORMALIZE_PATH"|"NORMALIZE_PATH_WIN"|"REMOVE_NULLS"|"REPLACE_NULLS"|"BASE64_DECODE_EXT"|"URL_DECODE_UNI"|"UTF8_TO_UNICODE"
                      )
                    ),
                    PositionalConstraint = "EXACTLY"|"STARTS_WITH"|"ENDS_WITH"|"CONTAINS"|"CONTAINS_WORD"
                  ),
                  SqliMatchStatement = list(
                    FieldToMatch = list(
                      SingleHeader = list(
                        Name = "string"
                      ),
                      SingleQueryArgument = list(
                        Name = "string"
                      ),
                      AllQueryArguments = list(),
                      UriPath = list(),
                      QueryString = list(),
                      Body = list(
                        OversizeHandling = "CONTINUE"|"MATCH"|"NO_MATCH"
                      ),
                      Method = list(),
                      JsonBody = list(
                        MatchPattern = list(
                          All = list(),
                          IncludedPaths = list(
                            "string"
                          )
                        ),
                        MatchScope = "ALL"|"KEY"|"VALUE",
                        InvalidFallbackBehavior = "MATCH"|"NO_MATCH"|"EVALUATE_AS_STRING",
                        OversizeHandling = "CONTINUE"|"MATCH"|"NO_MATCH"
                      ),
                      Headers = list(
                        MatchPattern = list(
                          All = list(),
                          IncludedHeaders = list(
                            "string"
                          ),
                          ExcludedHeaders = list(
                            "string"
                          )
                        ),
                        MatchScope = "ALL"|"KEY"|"VALUE",
                        OversizeHandling = "CONTINUE"|"MATCH"|"NO_MATCH"
                      ),
                      Cookies = list(
                        MatchPattern = list(
                          All = list(),
                          IncludedCookies = list(
                            "string"
                          ),
                          ExcludedCookies = list(
                            "string"
                          )
                        ),
                        MatchScope = "ALL"|"KEY"|"VALUE",
                        OversizeHandling = "CONTINUE"|"MATCH"|"NO_MATCH"
                      )
                    ),
                    TextTransformations = list(
                      list(
                        Priority = 123,
                        Type = "NONE"|"COMPRESS_WHITE_SPACE"|"HTML_ENTITY_DECODE"|"LOWERCASE"|"CMD_LINE"|"URL_DECODE"|"BASE64_DECODE"|"HEX_DECODE"|"MD5"|"REPLACE_COMMENTS"|"ESCAPE_SEQ_DECODE"|"SQL_HEX_DECODE"|"CSS_DECODE"|"JS_DECODE"|"NORMALIZE_PATH"|"NORMALIZE_PATH_WIN"|"REMOVE_NULLS"|"REPLACE_NULLS"|"BASE64_DECODE_EXT"|"URL_DECODE_UNI"|"UTF8_TO_UNICODE"
                      )
                    ),
                    SensitivityLevel = "LOW"|"HIGH"
                  ),
                  XssMatchStatement = list(
                    FieldToMatch = list(
                      SingleHeader = list(
                        Name = "string"
                      ),
                      SingleQueryArgument = list(
                        Name = "string"
                      ),
                      AllQueryArguments = list(),
                      UriPath = list(),
                      QueryString = list(),
                      Body = list(
                        OversizeHandling = "CONTINUE"|"MATCH"|"NO_MATCH"
                      ),
                      Method = list(),
                      JsonBody = list(
                        MatchPattern = list(
                          All = list(),
                          IncludedPaths = list(
                            "string"
                          )
                        ),
                        MatchScope = "ALL"|"KEY"|"VALUE",
                        InvalidFallbackBehavior = "MATCH"|"NO_MATCH"|"EVALUATE_AS_STRING",
                        OversizeHandling = "CONTINUE"|"MATCH"|"NO_MATCH"
                      ),
                      Headers = list(
                        MatchPattern = list(
                          All = list(),
                          IncludedHeaders = list(
                            "string"
                          ),
                          ExcludedHeaders = list(
                            "string"
                          )
                        ),
                        MatchScope = "ALL"|"KEY"|"VALUE",
                        OversizeHandling = "CONTINUE"|"MATCH"|"NO_MATCH"
                      ),
                      Cookies = list(
                        MatchPattern = list(
                          All = list(),
                          IncludedCookies = list(
                            "string"
                          ),
                          ExcludedCookies = list(
                            "string"
                          )
                        ),
                        MatchScope = "ALL"|"KEY"|"VALUE",
                        OversizeHandling = "CONTINUE"|"MATCH"|"NO_MATCH"
                      )
                    ),
                    TextTransformations = list(
                      list(
                        Priority = 123,
                        Type = "NONE"|"COMPRESS_WHITE_SPACE"|"HTML_ENTITY_DECODE"|"LOWERCASE"|"CMD_LINE"|"URL_DECODE"|"BASE64_DECODE"|"HEX_DECODE"|"MD5"|"REPLACE_COMMENTS"|"ESCAPE_SEQ_DECODE"|"SQL_HEX_DECODE"|"CSS_DECODE"|"JS_DECODE"|"NORMALIZE_PATH"|"NORMALIZE_PATH_WIN"|"REMOVE_NULLS"|"REPLACE_NULLS"|"BASE64_DECODE_EXT"|"URL_DECODE_UNI"|"UTF8_TO_UNICODE"
                      )
                    )
                  ),
                  SizeConstraintStatement = list(
                    FieldToMatch = list(
                      SingleHeader = list(
                        Name = "string"
                      ),
                      SingleQueryArgument = list(
                        Name = "string"
                      ),
                      AllQueryArguments = list(),
                      UriPath = list(),
                      QueryString = list(),
                      Body = list(
                        OversizeHandling = "CONTINUE"|"MATCH"|"NO_MATCH"
                      ),
                      Method = list(),
                      JsonBody = list(
                        MatchPattern = list(
                          All = list(),
                          IncludedPaths = list(
                            "string"
                          )
                        ),
                        MatchScope = "ALL"|"KEY"|"VALUE",
                        InvalidFallbackBehavior = "MATCH"|"NO_MATCH"|"EVALUATE_AS_STRING",
                        OversizeHandling = "CONTINUE"|"MATCH"|"NO_MATCH"
                      ),
                      Headers = list(
                        MatchPattern = list(
                          All = list(),
                          IncludedHeaders = list(
                            "string"
                          ),
                          ExcludedHeaders = list(
                            "string"
                          )
                        ),
                        MatchScope = "ALL"|"KEY"|"VALUE",
                        OversizeHandling = "CONTINUE"|"MATCH"|"NO_MATCH"
                      ),
                      Cookies = list(
                        MatchPattern = list(
                          All = list(),
                          IncludedCookies = list(
                            "string"
                          ),
                          ExcludedCookies = list(
                            "string"
                          )
                        ),
                        MatchScope = "ALL"|"KEY"|"VALUE",
                        OversizeHandling = "CONTINUE"|"MATCH"|"NO_MATCH"
                      )
                    ),
                    ComparisonOperator = "EQ"|"NE"|"LE"|"LT"|"GE"|"GT",
                    Size = 123,
                    TextTransformations = list(
                      list(
                        Priority = 123,
                        Type = "NONE"|"COMPRESS_WHITE_SPACE"|"HTML_ENTITY_DECODE"|"LOWERCASE"|"CMD_LINE"|"URL_DECODE"|"BASE64_DECODE"|"HEX_DECODE"|"MD5"|"REPLACE_COMMENTS"|"ESCAPE_SEQ_DECODE"|"SQL_HEX_DECODE"|"CSS_DECODE"|"JS_DECODE"|"NORMALIZE_PATH"|"NORMALIZE_PATH_WIN"|"REMOVE_NULLS"|"REPLACE_NULLS"|"BASE64_DECODE_EXT"|"URL_DECODE_UNI"|"UTF8_TO_UNICODE"
                      )
                    )
                  ),
                  GeoMatchStatement = list(
                    CountryCodes = list(
                      "AF"|"AX"|"AL"|"DZ"|"AS"|"AD"|"AO"|"AI"|"AQ"|"AG"|"AR"|"AM"|"AW"|"AU"|"AT"|"AZ"|"BS"|"BH"|"BD"|"BB"|"BY"|"BE"|"BZ"|"BJ"|"BM"|"BT"|"BO"|"BQ"|"BA"|"BW"|"BV"|"BR"|"IO"|"BN"|"BG"|"BF"|"BI"|"KH"|"CM"|"CA"|"CV"|"KY"|"CF"|"TD"|"CL"|"CN"|"CX"|"CC"|"CO"|"KM"|"CG"|"CD"|"CK"|"CR"|"CI"|"HR"|"CU"|"CW"|"CY"|"CZ"|"DK"|"DJ"|"DM"|"DO"|"EC"|"EG"|"SV"|"GQ"|"ER"|"EE"|"ET"|"FK"|"FO"|"FJ"|"FI"|"FR"|"GF"|"PF"|"TF"|"GA"|"GM"|"GE"|"DE"|"GH"|"GI"|"GR"|"GL"|"GD"|"GP"|"GU"|"GT"|"GG"|"GN"|"GW"|"GY"|"HT"|"HM"|"VA"|"HN"|"HK"|"HU"|"IS"|"IN"|"ID"|"IR"|"IQ"|"IE"|"IM"|"IL"|"IT"|"JM"|"JP"|"JE"|"JO"|"KZ"|"KE"|"KI"|"KP"|"KR"|"KW"|"KG"|"LA"|"LV"|"LB"|"LS"|"LR"|"LY"|"LI"|"LT"|"LU"|"MO"|"MK"|"MG"|"MW"|"MY"|"MV"|"ML"|"MT"|"MH"|"MQ"|"MR"|"MU"|"YT"|"MX"|"FM"|"MD"|"MC"|"MN"|"ME"|"MS"|"MA"|"MZ"|"MM"|"NA"|"NR"|"NP"|"NL"|"NC"|"NZ"|"NI"|"NE"|"NG"|"NU"|"NF"|"MP"|"NO"|"OM"|"PK"|"PW"|"PS"|"PA"|"PG"|"PY"|"PE"|"PH"|"PN"|"PL"|"PT"|"PR"|"QA"|"RE"|"RO"|"RU"|"RW"|"BL"|"SH"|"KN"|"LC"|"MF"|"PM"|"VC"|"WS"|"SM"|"ST"|"SA"|"SN"|"RS"|"SC"|"SL"|"SG"|"SX"|"SK"|"SI"|"SB"|"SO"|"ZA"|"GS"|"SS"|"ES"|"LK"|"SD"|"SR"|"SJ"|"SZ"|"SE"|"CH"|"SY"|"TW"|"TJ"|"TZ"|"TH"|"TL"|"TG"|"TK"|"TO"|"TT"|"TN"|"TR"|"TM"|"TC"|"TV"|"UG"|"UA"|"AE"|"GB"|"US"|"UM"|"UY"|"UZ"|"VU"|"VE"|"VN"|"VG"|"VI"|"WF"|"EH"|"YE"|"ZM"|"ZW"|"XK"
                    ),
                    ForwardedIPConfig = list(
                      HeaderName = "string",
                      FallbackBehavior = "MATCH"|"NO_MATCH"
                    )
                  ),
                  RuleGroupReferenceStatement = list(
                    ARN = "string",
                    ExcludedRules = list(
                      list(
                        Name = "string"
                      )
                    ),
                    RuleActionOverrides = list(
                      list(
                        Name = "string",
                        ActionToUse = list(
                          Block = list(
                            CustomResponse = list(
                              ResponseCode = 123,
                              CustomResponseBodyKey = "string",
                              ResponseHeaders = list(
                                list(
                                  Name = "string",
                                  Value = "string"
                                )
                              )
                            )
                          ),
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
                          Count = list(
                            CustomRequestHandling = list(
                              InsertHeaders = list(
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
                          Challenge = list(
                            CustomRequestHandling = list(
                              InsertHeaders = list(
                                list(
                                  Name = "string",
                                  Value = "string"
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  ),
                  IPSetReferenceStatement = list(
                    ARN = "string",
                    IPSetForwardedIPConfig = list(
                      HeaderName = "string",
                      FallbackBehavior = "MATCH"|"NO_MATCH",
                      Position = "FIRST"|"LAST"|"ANY"
                    )
                  ),
                  RegexPatternSetReferenceStatement = list(
                    ARN = "string",
                    FieldToMatch = list(
                      SingleHeader = list(
                        Name = "string"
                      ),
                      SingleQueryArgument = list(
                        Name = "string"
                      ),
                      AllQueryArguments = list(),
                      UriPath = list(),
                      QueryString = list(),
                      Body = list(
                        OversizeHandling = "CONTINUE"|"MATCH"|"NO_MATCH"
                      ),
                      Method = list(),
                      JsonBody = list(
                        MatchPattern = list(
                          All = list(),
                          IncludedPaths = list(
                            "string"
                          )
                        ),
                        MatchScope = "ALL"|"KEY"|"VALUE",
                        InvalidFallbackBehavior = "MATCH"|"NO_MATCH"|"EVALUATE_AS_STRING",
                        OversizeHandling = "CONTINUE"|"MATCH"|"NO_MATCH"
                      ),
                      Headers = list(
                        MatchPattern = list(
                          All = list(),
                          IncludedHeaders = list(
                            "string"
                          ),
                          ExcludedHeaders = list(
                            "string"
                          )
                        ),
                        MatchScope = "ALL"|"KEY"|"VALUE",
                        OversizeHandling = "CONTINUE"|"MATCH"|"NO_MATCH"
                      ),
                      Cookies = list(
                        MatchPattern = list(
                          All = list(),
                          IncludedCookies = list(
                            "string"
                          ),
                          ExcludedCookies = list(
                            "string"
                          )
                        ),
                        MatchScope = "ALL"|"KEY"|"VALUE",
                        OversizeHandling = "CONTINUE"|"MATCH"|"NO_MATCH"
                      )
                    ),
                    TextTransformations = list(
                      list(
                        Priority = 123,
                        Type = "NONE"|"COMPRESS_WHITE_SPACE"|"HTML_ENTITY_DECODE"|"LOWERCASE"|"CMD_LINE"|"URL_DECODE"|"BASE64_DECODE"|"HEX_DECODE"|"MD5"|"REPLACE_COMMENTS"|"ESCAPE_SEQ_DECODE"|"SQL_HEX_DECODE"|"CSS_DECODE"|"JS_DECODE"|"NORMALIZE_PATH"|"NORMALIZE_PATH_WIN"|"REMOVE_NULLS"|"REPLACE_NULLS"|"BASE64_DECODE_EXT"|"URL_DECODE_UNI"|"UTF8_TO_UNICODE"
                      )
                    )
                  ),
                  RateBasedStatement = list(
                    Limit = 123,
                    AggregateKeyType = "IP"|"FORWARDED_IP"|"CUSTOM_KEYS"|"CONSTANT",
                    ScopeDownStatement = list(),
                    ForwardedIPConfig = list(
                      HeaderName = "string",
                      FallbackBehavior = "MATCH"|"NO_MATCH"
                    ),
                    CustomKeys = list(
                      list(
                        Header = list(
                          Name = "string",
                          TextTransformations = list(
                            list(
                              Priority = 123,
                              Type = "NONE"|"COMPRESS_WHITE_SPACE"|"HTML_ENTITY_DECODE"|"LOWERCASE"|"CMD_LINE"|"URL_DECODE"|"BASE64_DECODE"|"HEX_DECODE"|"MD5"|"REPLACE_COMMENTS"|"ESCAPE_SEQ_DECODE"|"SQL_HEX_DECODE"|"CSS_DECODE"|"JS_DECODE"|"NORMALIZE_PATH"|"NORMALIZE_PATH_WIN"|"REMOVE_NULLS"|"REPLACE_NULLS"|"BASE64_DECODE_EXT"|"URL_DECODE_UNI"|"UTF8_TO_UNICODE"
                            )
                          )
                        ),
                        Cookie = list(
                          Name = "string",
                          TextTransformations = list(
                            list(
                              Priority = 123,
                              Type = "NONE"|"COMPRESS_WHITE_SPACE"|"HTML_ENTITY_DECODE"|"LOWERCASE"|"CMD_LINE"|"URL_DECODE"|"BASE64_DECODE"|"HEX_DECODE"|"MD5"|"REPLACE_COMMENTS"|"ESCAPE_SEQ_DECODE"|"SQL_HEX_DECODE"|"CSS_DECODE"|"JS_DECODE"|"NORMALIZE_PATH"|"NORMALIZE_PATH_WIN"|"REMOVE_NULLS"|"REPLACE_NULLS"|"BASE64_DECODE_EXT"|"URL_DECODE_UNI"|"UTF8_TO_UNICODE"
                            )
                          )
                        ),
                        QueryArgument = list(
                          Name = "string",
                          TextTransformations = list(
                            list(
                              Priority = 123,
                              Type = "NONE"|"COMPRESS_WHITE_SPACE"|"HTML_ENTITY_DECODE"|"LOWERCASE"|"CMD_LINE"|"URL_DECODE"|"BASE64_DECODE"|"HEX_DECODE"|"MD5"|"REPLACE_COMMENTS"|"ESCAPE_SEQ_DECODE"|"SQL_HEX_DECODE"|"CSS_DECODE"|"JS_DECODE"|"NORMALIZE_PATH"|"NORMALIZE_PATH_WIN"|"REMOVE_NULLS"|"REPLACE_NULLS"|"BASE64_DECODE_EXT"|"URL_DECODE_UNI"|"UTF8_TO_UNICODE"
                            )
                          )
                        ),
                        QueryString = list(
                          TextTransformations = list(
                            list(
                              Priority = 123,
                              Type = "NONE"|"COMPRESS_WHITE_SPACE"|"HTML_ENTITY_DECODE"|"LOWERCASE"|"CMD_LINE"|"URL_DECODE"|"BASE64_DECODE"|"HEX_DECODE"|"MD5"|"REPLACE_COMMENTS"|"ESCAPE_SEQ_DECODE"|"SQL_HEX_DECODE"|"CSS_DECODE"|"JS_DECODE"|"NORMALIZE_PATH"|"NORMALIZE_PATH_WIN"|"REMOVE_NULLS"|"REPLACE_NULLS"|"BASE64_DECODE_EXT"|"URL_DECODE_UNI"|"UTF8_TO_UNICODE"
                            )
                          )
                        ),
                        HTTPMethod = list(),
                        ForwardedIP = list(),
                        IP = list(),
                        LabelNamespace = list(
                          Namespace = "string"
                        )
                      )
                    )
                  ),
                  AndStatement = list(
                    Statements = list(
                      list()
                    )
                  ),
                  OrStatement = list(
                    Statements = list(
                      list()
                    )
                  ),
                  NotStatement = list(
                    Statement = list()
                  ),
                  ManagedRuleGroupStatement = list(),
                  LabelMatchStatement = list(
                    Scope = "LABEL"|"NAMESPACE",
                    Key = "string"
                  ),
                  RegexMatchStatement = list(
                    RegexString = "string",
                    FieldToMatch = list(
                      SingleHeader = list(
                        Name = "string"
                      ),
                      SingleQueryArgument = list(
                        Name = "string"
                      ),
                      AllQueryArguments = list(),
                      UriPath = list(),
                      QueryString = list(),
                      Body = list(
                        OversizeHandling = "CONTINUE"|"MATCH"|"NO_MATCH"
                      ),
                      Method = list(),
                      JsonBody = list(
                        MatchPattern = list(
                          All = list(),
                          IncludedPaths = list(
                            "string"
                          )
                        ),
                        MatchScope = "ALL"|"KEY"|"VALUE",
                        InvalidFallbackBehavior = "MATCH"|"NO_MATCH"|"EVALUATE_AS_STRING",
                        OversizeHandling = "CONTINUE"|"MATCH"|"NO_MATCH"
                      ),
                      Headers = list(
                        MatchPattern = list(
                          All = list(),
                          IncludedHeaders = list(
                            "string"
                          ),
                          ExcludedHeaders = list(
                            "string"
                          )
                        ),
                        MatchScope = "ALL"|"KEY"|"VALUE",
                        OversizeHandling = "CONTINUE"|"MATCH"|"NO_MATCH"
                      ),
                      Cookies = list(
                        MatchPattern = list(
                          All = list(),
                          IncludedCookies = list(
                            "string"
                          ),
                          ExcludedCookies = list(
                            "string"
                          )
                        ),
                        MatchScope = "ALL"|"KEY"|"VALUE",
                        OversizeHandling = "CONTINUE"|"MATCH"|"NO_MATCH"
                      )
                    ),
                    TextTransformations = list(
                      list(
                        Priority = 123,
                        Type = "NONE"|"COMPRESS_WHITE_SPACE"|"HTML_ENTITY_DECODE"|"LOWERCASE"|"CMD_LINE"|"URL_DECODE"|"BASE64_DECODE"|"HEX_DECODE"|"MD5"|"REPLACE_COMMENTS"|"ESCAPE_SEQ_DECODE"|"SQL_HEX_DECODE"|"CSS_DECODE"|"JS_DECODE"|"NORMALIZE_PATH"|"NORMALIZE_PATH_WIN"|"REMOVE_NULLS"|"REPLACE_NULLS"|"BASE64_DECODE_EXT"|"URL_DECODE_UNI"|"UTF8_TO_UNICODE"
                      )
                    )
                  )
                ),
                ManagedRuleGroupConfigs = list(
                  list(
                    LoginPath = "string",
                    PayloadType = "JSON"|"FORM_ENCODED",
                    UsernameField = list(
                      Identifier = "string"
                    ),
                    PasswordField = list(
                      Identifier = "string"
                    ),
                    AWSManagedRulesBotControlRuleSet = list(
                      InspectionLevel = "COMMON"|"TARGETED"
                    ),
                    AWSManagedRulesATPRuleSet = list(
                      LoginPath = "string",
                      RequestInspection = list(
                        PayloadType = "JSON"|"FORM_ENCODED",
                        UsernameField = list(
                          Identifier = "string"
                        ),
                        PasswordField = list(
                          Identifier = "string"
                        )
                      ),
                      ResponseInspection = list(
                        StatusCode = list(
                          SuccessCodes = list(
                            123
                          ),
                          FailureCodes = list(
                            123
                          )
                        ),
                        Header = list(
                          Name = "string",
                          SuccessValues = list(
                            "string"
                          ),
                          FailureValues = list(
                            "string"
                          )
                        ),
                        BodyContains = list(
                          SuccessStrings = list(
                            "string"
                          ),
                          FailureStrings = list(
                            "string"
                          )
                        ),
                        Json = list(
                          Identifier = "string",
                          SuccessValues = list(
                            "string"
                          ),
                          FailureValues = list(
                            "string"
                          )
                        )
                      )
                    )
                  )
                ),
                RuleActionOverrides = list(
                  list(
                    Name = "string",
                    ActionToUse = list(
                      Block = list(
                        CustomResponse = list(
                          ResponseCode = 123,
                          CustomResponseBodyKey = "string",
                          ResponseHeaders = list(
                            list(
                              Name = "string",
                              Value = "string"
                            )
                          )
                        )
                      ),
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
                      Count = list(
                        CustomRequestHandling = list(
                          InsertHeaders = list(
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
                      Challenge = list(
                        CustomRequestHandling = list(
                          InsertHeaders = list(
                            list(
                              Name = "string",
                              Value = "string"
                            )
                          )
                        )
                      )
                    )
                  )
                )
              ),
              RuleGroupReferenceStatement = list(
                ARN = "string",
                ExcludedRules = list(
                  list(
                    Name = "string"
                  )
                ),
                RuleActionOverrides = list(
                  list(
                    Name = "string",
                    ActionToUse = list(
                      Block = list(
                        CustomResponse = list(
                          ResponseCode = 123,
                          CustomResponseBodyKey = "string",
                          ResponseHeaders = list(
                            list(
                              Name = "string",
                              Value = "string"
                            )
                          )
                        )
                      ),
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
                      Count = list(
                        CustomRequestHandling = list(
                          InsertHeaders = list(
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
                      Challenge = list(
                        CustomRequestHandling = list(
                          InsertHeaders = list(
                            list(
                              Name = "string",
                              Value = "string"
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            ),
            OverrideAction = list(
              Count = list(
                CustomRequestHandling = list(
                  InsertHeaders = list(
                    list(
                      Name = "string",
                      Value = "string"
                    )
                  )
                )
              ),
              None = list()
            ),
            VisibilityConfig = list(
              SampledRequestsEnabled = TRUE|FALSE,
              CloudWatchMetricsEnabled = TRUE|FALSE,
              MetricName = "string"
            )
          )
        ),
        ManagedByFirewallManager = TRUE|FALSE,
        LabelNamespace = "string",
        CustomResponseBodies = list(
          list(
            ContentType = "TEXT_PLAIN"|"TEXT_HTML"|"APPLICATION_JSON",
            Content = "string"
          )
        ),
        CaptchaConfig = list(
          ImmunityTimeProperty = list(
            ImmunityTime = 123
          )
        ),
        ChallengeConfig = list(
          ImmunityTimeProperty = list(
            ImmunityTime = 123
          )
        ),
        TokenDomains = list(
          "string"
        ),
        AssociationConfig = list(
          RequestBody = list(
            list(
              DefaultSizeInspectionLimit = "KB_16"|"KB_32"|"KB_48"|"KB_64"
            )
          )
        )
      )
    )

### Request syntax

    svc$get_web_acl_for_resource(
      ResourceArn = "string"
    )