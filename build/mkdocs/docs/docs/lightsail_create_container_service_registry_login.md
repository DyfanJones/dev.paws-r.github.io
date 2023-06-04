<table style="width: 100%;">
<tbody>
<tr class="odd">
<td>lightsail_create_container_service_registry_login</td>
<td style="text-align: right;">R Documentation</td>
</tr>
</tbody>
</table>

## Creates a temporary set of log in credentials that you can use to log in to the Docker process on your local machine

### Description

Creates a temporary set of log in credentials that you can use to log in
to the Docker process on your local machine. After you're logged in, you
can use the native Docker commands to push your local container images
to the container image registry of your Amazon Lightsail account so that
you can use them with your Lightsail container service. The log in
credentials expire 12 hours after they are created, at which point you
will need to create a new set of log in credentials.

You can only push container images to the container service registry of
your Lightsail account. You cannot pull container images or perform any
other container image management actions on the container service
registry.

After you push your container images to the container image registry of
your Lightsail account, use the `register_container_image` action to
register the pushed images to a specific Lightsail container service.

This action is not required if you install and use the Lightsail Control
(lightsailctl) plugin to push container images to your Lightsail
container service. For more information, see Pushing and managing
container images on your Amazon Lightsail container services in the
*Amazon Lightsail Developer Guide*.

### Usage

    lightsail_create_container_service_registry_login()

### Value

A list with the following syntax:

    list(
      registryLogin = list(
        username = "string",
        password = "string",
        expiresAt = as.POSIXct(
          "2015-01-01"
        ),
        registry = "string"
      )
    )

### Request syntax

    svc$create_container_service_registry_login()