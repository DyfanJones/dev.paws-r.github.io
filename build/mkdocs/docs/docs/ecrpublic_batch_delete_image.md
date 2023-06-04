<table style="width: 100%;">
<tbody>
<tr class="odd">
<td>ecrpublic_batch_delete_image</td>
<td style="text-align: right;">R Documentation</td>
</tr>
</tbody>
</table>

## Deletes a list of specified images that are within a repository in a public registry

### Description

Deletes a list of specified images that are within a repository in a
public registry. Images are specified with either an `imageTag` or
`imageDigest`.

You can remove a tag from an image by specifying the image's tag in your
request. When you remove the last tag from an image, the image is
deleted from your repository.

You can completely delete an image (and all of its tags) by specifying
the digest of the image in your request.

### Usage

    ecrpublic_batch_delete_image(registryId, repositoryName, imageIds)

### Arguments

<table>
<colgroup>
<col style="width: 35%" />
<col style="width: 65%" />
</colgroup>
<tbody>
<tr class="odd">
<td><code
id="ecrpublic_batch_delete_image_:_registryId">registryId</code></td>
<td><p>The Amazon Web Services account ID, or registry alias, that's
associated with the registry that contains the image to delete. If you
do not specify a registry, the default public registry is
assumed.</p></td>
</tr>
<tr class="even">
<td><code
id="ecrpublic_batch_delete_image_:_repositoryName">repositoryName</code></td>
<td><p>[required] The repository in a public registry that contains the
image to delete.</p></td>
</tr>
<tr class="odd">
<td><code
id="ecrpublic_batch_delete_image_:_imageIds">imageIds</code></td>
<td><p>[required] A list of image ID references that correspond to
images to delete. The format of the <code>imageIds</code> reference is
<code>imageTag=tag</code> or <code>imageDigest=digest</code>.</p></td>
</tr>
</tbody>
</table>

### Value

A list with the following syntax:

    list(
      imageIds = list(
        list(
          imageDigest = "string",
          imageTag = "string"
        )
      ),
      failures = list(
        list(
          imageId = list(
            imageDigest = "string",
            imageTag = "string"
          ),
          failureCode = "InvalidImageDigest"|"InvalidImageTag"|"ImageTagDoesNotMatchDigest"|"ImageNotFound"|"MissingDigestAndTag"|"ImageReferencedByManifestList"|"KmsError",
          failureReason = "string"
        )
      )
    )

### Request syntax

    svc$batch_delete_image(
      registryId = "string",
      repositoryName = "string",
      imageIds = list(
        list(
          imageDigest = "string",
          imageTag = "string"
        )
      )
    )