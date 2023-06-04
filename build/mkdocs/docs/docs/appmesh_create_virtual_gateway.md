<table style="width: 100%;">
<tbody>
<tr class="odd">
<td>appmesh_create_virtual_gateway</td>
<td style="text-align: right;">R Documentation</td>
</tr>
</tbody>
</table>

## Creates a virtual gateway

### Description

Creates a virtual gateway.

A virtual gateway allows resources outside your mesh to communicate to
resources that are inside your mesh. The virtual gateway represents an
Envoy proxy running in an Amazon ECS task, in a Kubernetes service, or
on an Amazon EC2 instance. Unlike a virtual node, which represents an
Envoy running with an application, a virtual gateway represents Envoy
deployed by itself.

For more information about virtual gateways, see [Virtual
gateways](https://docs.aws.amazon.com/app-mesh/latest/userguide/virtual_gateways.html).

### Usage

    appmesh_create_virtual_gateway(clientToken, meshName, meshOwner, spec,
      tags, virtualGatewayName)

### Arguments

<table>
<colgroup>
<col style="width: 35%" />
<col style="width: 65%" />
</colgroup>
<tbody>
<tr class="odd">
<td><code
id="appmesh_create_virtual_gateway_:_clientToken">clientToken</code></td>
<td><p>Unique, case-sensitive identifier that you provide to ensure the
idempotency of the request. Up to 36 letters, numbers, hyphens, and
underscores are allowed.</p></td>
</tr>
<tr class="even">
<td><code
id="appmesh_create_virtual_gateway_:_meshName">meshName</code></td>
<td><p>[required] The name of the service mesh to create the virtual
gateway in.</p></td>
</tr>
<tr class="odd">
<td><code
id="appmesh_create_virtual_gateway_:_meshOwner">meshOwner</code></td>
<td><p>The Amazon Web Services IAM account ID of the service mesh owner.
If the account ID is not your own, then the account that you specify
must share the mesh with your account before you can create the resource
in the service mesh. For more information about mesh sharing, see <a
href="https://docs.aws.amazon.com/app-mesh/latest/userguide/sharing.html">Working
with shared meshes</a>.</p></td>
</tr>
<tr class="even">
<td><code id="appmesh_create_virtual_gateway_:_spec">spec</code></td>
<td><p>[required] The virtual gateway specification to apply.</p></td>
</tr>
<tr class="odd">
<td><code id="appmesh_create_virtual_gateway_:_tags">tags</code></td>
<td><p>Optional metadata that you can apply to the virtual gateway to
assist with categorization and organization. Each tag consists of a key
and an optional value, both of which you define. Tag keys can have a
maximum character length of 128 characters, and tag values can have a
maximum length of 256 characters.</p></td>
</tr>
<tr class="even">
<td><code
id="appmesh_create_virtual_gateway_:_virtualGatewayName">virtualGatewayName</code></td>
<td><p>[required] The name to use for the virtual gateway.</p></td>
</tr>
</tbody>
</table>

### Value

A list with the following syntax:

    list(
      virtualGateway = list(
        meshName = "string",
        metadata = list(
          arn = "string",
          createdAt = as.POSIXct(
            "2015-01-01"
          ),
          lastUpdatedAt = as.POSIXct(
            "2015-01-01"
          ),
          meshOwner = "string",
          resourceOwner = "string",
          uid = "string",
          version = 123
        ),
        spec = list(
          backendDefaults = list(
            clientPolicy = list(
              tls = list(
                certificate = list(
                  file = list(
                    certificateChain = "string",
                    privateKey = "string"
                  ),
                  sds = list(
                    secretName = "string"
                  )
                ),
                enforce = TRUE|FALSE,
                ports = list(
                  123
                ),
                validation = list(
                  subjectAlternativeNames = list(
                    match = list(
                      exact = list(
                        "string"
                      )
                    )
                  ),
                  trust = list(
                    acm = list(
                      certificateAuthorityArns = list(
                        "string"
                      )
                    ),
                    file = list(
                      certificateChain = "string"
                    ),
                    sds = list(
                      secretName = "string"
                    )
                  )
                )
              )
            )
          ),
          listeners = list(
            list(
              connectionPool = list(
                grpc = list(
                  maxRequests = 123
                ),
                http = list(
                  maxConnections = 123,
                  maxPendingRequests = 123
                ),
                http2 = list(
                  maxRequests = 123
                )
              ),
              healthCheck = list(
                healthyThreshold = 123,
                intervalMillis = 123,
                path = "string",
                port = 123,
                protocol = "http"|"http2"|"grpc",
                timeoutMillis = 123,
                unhealthyThreshold = 123
              ),
              portMapping = list(
                port = 123,
                protocol = "http"|"http2"|"grpc"
              ),
              tls = list(
                certificate = list(
                  acm = list(
                    certificateArn = "string"
                  ),
                  file = list(
                    certificateChain = "string",
                    privateKey = "string"
                  ),
                  sds = list(
                    secretName = "string"
                  )
                ),
                mode = "STRICT"|"PERMISSIVE"|"DISABLED",
                validation = list(
                  subjectAlternativeNames = list(
                    match = list(
                      exact = list(
                        "string"
                      )
                    )
                  ),
                  trust = list(
                    file = list(
                      certificateChain = "string"
                    ),
                    sds = list(
                      secretName = "string"
                    )
                  )
                )
              )
            )
          ),
          logging = list(
            accessLog = list(
              file = list(
                format = list(
                  json = list(
                    list(
                      key = "string",
                      value = "string"
                    )
                  ),
                  text = "string"
                ),
                path = "string"
              )
            )
          )
        ),
        status = list(
          status = "ACTIVE"|"INACTIVE"|"DELETED"
        ),
        virtualGatewayName = "string"
      )
    )

### Request syntax

    svc$create_virtual_gateway(
      clientToken = "string",
      meshName = "string",
      meshOwner = "string",
      spec = list(
        backendDefaults = list(
          clientPolicy = list(
            tls = list(
              certificate = list(
                file = list(
                  certificateChain = "string",
                  privateKey = "string"
                ),
                sds = list(
                  secretName = "string"
                )
              ),
              enforce = TRUE|FALSE,
              ports = list(
                123
              ),
              validation = list(
                subjectAlternativeNames = list(
                  match = list(
                    exact = list(
                      "string"
                    )
                  )
                ),
                trust = list(
                  acm = list(
                    certificateAuthorityArns = list(
                      "string"
                    )
                  ),
                  file = list(
                    certificateChain = "string"
                  ),
                  sds = list(
                    secretName = "string"
                  )
                )
              )
            )
          )
        ),
        listeners = list(
          list(
            connectionPool = list(
              grpc = list(
                maxRequests = 123
              ),
              http = list(
                maxConnections = 123,
                maxPendingRequests = 123
              ),
              http2 = list(
                maxRequests = 123
              )
            ),
            healthCheck = list(
              healthyThreshold = 123,
              intervalMillis = 123,
              path = "string",
              port = 123,
              protocol = "http"|"http2"|"grpc",
              timeoutMillis = 123,
              unhealthyThreshold = 123
            ),
            portMapping = list(
              port = 123,
              protocol = "http"|"http2"|"grpc"
            ),
            tls = list(
              certificate = list(
                acm = list(
                  certificateArn = "string"
                ),
                file = list(
                  certificateChain = "string",
                  privateKey = "string"
                ),
                sds = list(
                  secretName = "string"
                )
              ),
              mode = "STRICT"|"PERMISSIVE"|"DISABLED",
              validation = list(
                subjectAlternativeNames = list(
                  match = list(
                    exact = list(
                      "string"
                    )
                  )
                ),
                trust = list(
                  file = list(
                    certificateChain = "string"
                  ),
                  sds = list(
                    secretName = "string"
                  )
                )
              )
            )
          )
        ),
        logging = list(
          accessLog = list(
            file = list(
              format = list(
                json = list(
                  list(
                    key = "string",
                    value = "string"
                  )
                ),
                text = "string"
              ),
              path = "string"
            )
          )
        )
      ),
      tags = list(
        list(
          key = "string",
          value = "string"
        )
      ),
      virtualGatewayName = "string"
    )