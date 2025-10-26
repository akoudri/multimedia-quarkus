package com.akfc.notifications.jobs;

import io.quarkus.scheduler.Scheduled;
import jakarta.batch.operations.JobOperator;
import jakarta.batch.runtime.BatchRuntime;
import jakarta.batch.runtime.BatchStatus;
import jakarta.batch.runtime.JobExecution;
import jakarta.enterprise.context.ApplicationScoped;
import org.jboss.logging.Logger;

import java.util.Properties;

/**
 * Scheduler for periodic cleanup of read notifications.
 *
 * Triggers the JBeret batch job to delete old read notifications
 * from the users database on a scheduled basis.
 */
@ApplicationScoped
public class NotificationCleanupScheduler {

    private static final Logger LOG = Logger.getLogger(NotificationCleanupScheduler.class);
    private static final String JOB_NAME = "cleanup-read-notifications";

    /**
     * Scheduled job to clean up read notifications.
     *
     * Runs daily at 2 AM by default (configured in application.properties).
     * Can be configured to run more frequently for testing.
     */
    @Scheduled(cron = "{cleanup.cron}")
    public void cleanupReadNotifications() {
        LOG.info("Starting scheduled notification cleanup job...");

        try {
            // Get the JBeret JobOperator
            JobOperator jobOperator = BatchRuntime.getJobOperator();

            // Set job parameters
            Properties jobParameters = new Properties();
            jobParameters.setProperty("retention.days", "7");

            // Start the batch job
            long executionId = jobOperator.start(JOB_NAME, jobParameters);
            LOG.infof("Batch job '%s' started with execution ID: %d", JOB_NAME, executionId);

            // Wait for job completion (with timeout)
            JobExecution jobExecution = waitForCompletion(jobOperator, executionId, 60000); // 60 seconds timeout

            if (jobExecution != null) {
                BatchStatus batchStatus = jobExecution.getBatchStatus();
                String exitStatus = jobExecution.getExitStatus();

                LOG.infof("Batch job completed with status: %s, exit status: %s",
                         batchStatus, exitStatus);

                if (batchStatus == BatchStatus.COMPLETED) {
                    LOG.info("Notification cleanup completed successfully");
                } else {
                    LOG.warnf("Notification cleanup finished with status: %s", batchStatus);
                }
            } else {
                LOG.warn("Job execution timeout - cleanup may still be running");
            }

        } catch (Exception e) {
            LOG.errorf(e, "Error during scheduled notification cleanup");
        }
    }

    /**
     * Wait for job completion with timeout.
     *
     * @param jobOperator The job operator
     * @param executionId Execution ID
     * @param timeoutMs Timeout in milliseconds
     * @return JobExecution if completed, null if timeout
     */
    private JobExecution waitForCompletion(JobOperator jobOperator, long executionId, long timeoutMs) {
        long startTime = System.currentTimeMillis();

        while (System.currentTimeMillis() - startTime < timeoutMs) {
            try {
                JobExecution jobExecution = jobOperator.getJobExecution(executionId);
                BatchStatus batchStatus = jobExecution.getBatchStatus();

                if (batchStatus == BatchStatus.COMPLETED ||
                    batchStatus == BatchStatus.FAILED ||
                    batchStatus == BatchStatus.STOPPED ||
                    batchStatus == BatchStatus.ABANDONED) {
                    return jobExecution;
                }

                // Wait a bit before checking again
                Thread.sleep(1000);

            } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
                LOG.warn("Job wait interrupted");
                return null;
            } catch (Exception e) {
                LOG.errorf(e, "Error checking job status");
                return null;
            }
        }

        LOG.warn("Job execution timeout reached");
        return null;
    }

    /**
     * Manual trigger for testing purposes.
     * Can be called via REST endpoint or management interface.
     *
     * @param retentionDays Number of days to retain read notifications
     * @return Execution ID of the started job
     */
    public long triggerCleanup(int retentionDays) {
        LOG.infof("Manually triggering notification cleanup with retention: %d days", retentionDays);

        try {
            JobOperator jobOperator = BatchRuntime.getJobOperator();

            Properties jobParameters = new Properties();
            jobParameters.setProperty("retention.days", String.valueOf(retentionDays));

            long executionId = jobOperator.start(JOB_NAME, jobParameters);
            LOG.infof("Batch job started with execution ID: %d", executionId);

            return executionId;

        } catch (Exception e) {
            LOG.errorf(e, "Error triggering manual cleanup");
            throw new RuntimeException("Failed to trigger cleanup job", e);
        }
    }
}
