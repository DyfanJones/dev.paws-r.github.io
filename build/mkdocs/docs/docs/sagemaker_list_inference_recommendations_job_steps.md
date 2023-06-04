<table style="width: 100%;">
<tbody>
<tr class="odd">
<td>sagemaker_list_inference_recommendations_job_steps</td>
<td style="text-align: right;">R Documentation</td>
</tr>
</tbody>
</table>

## Returns a list of the subtasks for an Inference Recommender job

### Description

Returns a list of the subtasks for an Inference Recommender job.

The supported subtasks are benchmarks, which evaluate the performance of
your model on different instance types.

### Usage

    sagemaker_list_inference_recommendations_job_steps(JobName, Status,
      StepType, MaxResults, NextToken)

### Arguments

<table>
<colgroup>
<col style="width: 35%" />
<col style="width: 65%" />
</colgroup>
<tbody>
<tr class="odd">
<td><code
id="sagemaker_list_inference_recommendations_job_steps_:_JobName">JobName</code></td>
<td><p>[required] The name for the Inference Recommender job.</p></td>
</tr>
<tr class="even">
<td><code
id="sagemaker_list_inference_recommendations_job_steps_:_Status">Status</code></td>
<td><p>A filter to return benchmarks of a specified status. If this
field is left empty, then all benchmarks are returned.</p></td>
</tr>
<tr class="odd">
<td><code
id="sagemaker_list_inference_recommendations_job_steps_:_StepType">StepType</code></td>
<td><p>A filter to return details about the specified type of
subtask.</p>
<p><code>BENCHMARK</code>: Evaluate the performance of your model on
different instance types.</p></td>
</tr>
<tr class="even">
<td><code
id="sagemaker_list_inference_recommendations_job_steps_:_MaxResults">MaxResults</code></td>
<td><p>The maximum number of results to return.</p></td>
</tr>
<tr class="odd">
<td><code
id="sagemaker_list_inference_recommendations_job_steps_:_NextToken">NextToken</code></td>
<td><p>A token that you can specify to return more results from the
list. Specify this field if you have a token that was returned from a
previous request.</p></td>
</tr>
</tbody>
</table>

### Value

A list with the following syntax:

    list(
      Steps = list(
        list(
          StepType = "BENCHMARK",
          JobName = "string",
          Status = "PENDING"|"IN_PROGRESS"|"COMPLETED"|"FAILED"|"STOPPING"|"STOPPED",
          InferenceBenchmark = list(
            Metrics = list(
              CostPerHour = 123.0,
              CostPerInference = 123.0,
              MaxInvocations = 123,
              ModelLatency = 123,
              CpuUtilization = 123.0,
              MemoryUtilization = 123.0
            ),
            EndpointConfiguration = list(
              EndpointName = "string",
              VariantName = "string",
              InstanceType = "ml.t2.medium"|"ml.t2.large"|"ml.t2.xlarge"|"ml.t2.2xlarge"|"ml.m4.xlarge"|"ml.m4.2xlarge"|"ml.m4.4xlarge"|"ml.m4.10xlarge"|"ml.m4.16xlarge"|"ml.m5.large"|"ml.m5.xlarge"|"ml.m5.2xlarge"|"ml.m5.4xlarge"|"ml.m5.12xlarge"|"ml.m5.24xlarge"|"ml.m5d.large"|"ml.m5d.xlarge"|"ml.m5d.2xlarge"|"ml.m5d.4xlarge"|"ml.m5d.12xlarge"|"ml.m5d.24xlarge"|"ml.c4.large"|"ml.c4.xlarge"|"ml.c4.2xlarge"|"ml.c4.4xlarge"|"ml.c4.8xlarge"|"ml.p2.xlarge"|"ml.p2.8xlarge"|"ml.p2.16xlarge"|"ml.p3.2xlarge"|"ml.p3.8xlarge"|"ml.p3.16xlarge"|"ml.c5.large"|"ml.c5.xlarge"|"ml.c5.2xlarge"|"ml.c5.4xlarge"|"ml.c5.9xlarge"|"ml.c5.18xlarge"|"ml.c5d.large"|"ml.c5d.xlarge"|"ml.c5d.2xlarge"|"ml.c5d.4xlarge"|"ml.c5d.9xlarge"|"ml.c5d.18xlarge"|"ml.g4dn.xlarge"|"ml.g4dn.2xlarge"|"ml.g4dn.4xlarge"|"ml.g4dn.8xlarge"|"ml.g4dn.12xlarge"|"ml.g4dn.16xlarge"|"ml.r5.large"|"ml.r5.xlarge"|"ml.r5.2xlarge"|"ml.r5.4xlarge"|"ml.r5.12xlarge"|"ml.r5.24xlarge"|"ml.r5d.large"|"ml.r5d.xlarge"|"ml.r5d.2xlarge"|"ml.r5d.4xlarge"|"ml.r5d.12xlarge"|"ml.r5d.24xlarge"|"ml.inf1.xlarge"|"ml.inf1.2xlarge"|"ml.inf1.6xlarge"|"ml.inf1.24xlarge"|"ml.c6i.large"|"ml.c6i.xlarge"|"ml.c6i.2xlarge"|"ml.c6i.4xlarge"|"ml.c6i.8xlarge"|"ml.c6i.12xlarge"|"ml.c6i.16xlarge"|"ml.c6i.24xlarge"|"ml.c6i.32xlarge"|"ml.g5.xlarge"|"ml.g5.2xlarge"|"ml.g5.4xlarge"|"ml.g5.8xlarge"|"ml.g5.12xlarge"|"ml.g5.16xlarge"|"ml.g5.24xlarge"|"ml.g5.48xlarge"|"ml.p4d.24xlarge"|"ml.c7g.large"|"ml.c7g.xlarge"|"ml.c7g.2xlarge"|"ml.c7g.4xlarge"|"ml.c7g.8xlarge"|"ml.c7g.12xlarge"|"ml.c7g.16xlarge"|"ml.m6g.large"|"ml.m6g.xlarge"|"ml.m6g.2xlarge"|"ml.m6g.4xlarge"|"ml.m6g.8xlarge"|"ml.m6g.12xlarge"|"ml.m6g.16xlarge"|"ml.m6gd.large"|"ml.m6gd.xlarge"|"ml.m6gd.2xlarge"|"ml.m6gd.4xlarge"|"ml.m6gd.8xlarge"|"ml.m6gd.12xlarge"|"ml.m6gd.16xlarge"|"ml.c6g.large"|"ml.c6g.xlarge"|"ml.c6g.2xlarge"|"ml.c6g.4xlarge"|"ml.c6g.8xlarge"|"ml.c6g.12xlarge"|"ml.c6g.16xlarge"|"ml.c6gd.large"|"ml.c6gd.xlarge"|"ml.c6gd.2xlarge"|"ml.c6gd.4xlarge"|"ml.c6gd.8xlarge"|"ml.c6gd.12xlarge"|"ml.c6gd.16xlarge"|"ml.c6gn.large"|"ml.c6gn.xlarge"|"ml.c6gn.2xlarge"|"ml.c6gn.4xlarge"|"ml.c6gn.8xlarge"|"ml.c6gn.12xlarge"|"ml.c6gn.16xlarge"|"ml.r6g.large"|"ml.r6g.xlarge"|"ml.r6g.2xlarge"|"ml.r6g.4xlarge"|"ml.r6g.8xlarge"|"ml.r6g.12xlarge"|"ml.r6g.16xlarge"|"ml.r6gd.large"|"ml.r6gd.xlarge"|"ml.r6gd.2xlarge"|"ml.r6gd.4xlarge"|"ml.r6gd.8xlarge"|"ml.r6gd.12xlarge"|"ml.r6gd.16xlarge"|"ml.p4de.24xlarge"|"ml.trn1.2xlarge"|"ml.trn1.32xlarge"|"ml.inf2.xlarge"|"ml.inf2.8xlarge"|"ml.inf2.24xlarge"|"ml.inf2.48xlarge",
              InitialInstanceCount = 123
            ),
            ModelConfiguration = list(
              InferenceSpecificationName = "string",
              EnvironmentParameters = list(
                list(
                  Key = "string",
                  ValueType = "string",
                  Value = "string"
                )
              ),
              CompilationJobName = "string"
            ),
            FailureReason = "string",
            EndpointMetrics = list(
              MaxInvocations = 123,
              ModelLatency = 123
            ),
            InvocationEndTime = as.POSIXct(
              "2015-01-01"
            ),
            InvocationStartTime = as.POSIXct(
              "2015-01-01"
            )
          )
        )
      ),
      NextToken = "string"
    )

### Request syntax

    svc$list_inference_recommendations_job_steps(
      JobName = "string",
      Status = "PENDING"|"IN_PROGRESS"|"COMPLETED"|"FAILED"|"STOPPING"|"STOPPED",
      StepType = "BENCHMARK",
      MaxResults = 123,
      NextToken = "string"
    )
