<table style="width: 100%;">
<tbody>
<tr class="odd">
<td>appmesh_update_virtual_router</td>
<td style="text-align: right;">R Documentation</td>
</tr>
</tbody>
</table>

## Updates an existing virtual router in a specified service mesh

### Description

Updates an existing virtual router in a specified service mesh.

### Usage

    appmesh_update_virtual_router(clientToken, meshName, meshOwner, spec,
      virtualRouterName)

### Arguments

<table>
<colgroup>
<col style="width: 35%" />
<col style="width: 65%" />
</colgroup>
<tbody>
<tr class="odd">
<td><code
id="appmesh_update_virtual_router_:_clientToken">clientToken</code></td>
<td><p>Unique, case-sensitive identifier that you provide to ensure the
idempotency of the request. Up to 36 letters, numbers, hyphens, and
underscores are allowed.</p></td>
</tr>
<tr class="even">
<td><code
id="appmesh_update_virtual_router_:_meshName">meshName</code></td>
<td><p>[required] The name of the service mesh that the virtual router
resides in.</p></td>
</tr>
<tr class="odd">
<td><code
id="appmesh_update_virtual_router_:_meshOwner">meshOwner</code></td>
<td><p>The Amazon Web Services IAM account ID of the service mesh owner.
If the account ID is not your own, then it's the ID of the account that
shared the mesh with your account. For more information about mesh
sharing, see <a
href="https://docs.aws.amazon.com/app-mesh/latest/userguide/sharing.html">Working
with shared meshes</a>.</p></td>
</tr>
<tr class="even">
<td><code id="appmesh_update_virtual_router_:_spec">spec</code></td>
<td><p>[required] The new virtual router specification to apply. This
overwrites the existing data.</p></td>
</tr>
<tr class="odd">
<td><code
id="appmesh_update_virtual_router_:_virtualRouterName">virtualRouterName</code></td>
<td><p>[required] The name of the virtual router to update.</p></td>
</tr>
</tbody>
</table>

### Value

A list with the following syntax:

    list(
      virtualRouter = list(
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
          listeners = list(
            list(
              portMapping = list(
                port = 123,
                protocol = "http"|"tcp"|"http2"|"grpc"
              )
            )
          )
        ),
        status = list(
          status = "ACTIVE"|"INACTIVE"|"DELETED"
        ),
        virtualRouterName = "string"
      )
    )

### Request syntax

    svc$update_virtual_router(
      clientToken = "string",
      meshName = "string",
      meshOwner = "string",
      spec = list(
        listeners = list(
          list(
            portMapping = list(
              port = 123,
              protocol = "http"|"tcp"|"http2"|"grpc"
            )
          )
        )
      ),
      virtualRouterName = "string"
    )