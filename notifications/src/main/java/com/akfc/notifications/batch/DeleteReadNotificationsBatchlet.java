package com.akfc.notifications.batch;

import jakarta.batch.api.AbstractBatchlet;
import jakarta.batch.api.BatchProperty;
import jakarta.batch.runtime.context.JobContext;
import jakarta.enterprise.context.Dependent;
import jakarta.inject.Inject;
import jakarta.inject.Named;
import jakarta.persistence.EntityManager;
import jakarta.transaction.Transactional;
import org.jboss.logging.Logger;

import java.time.LocalDateTime;

/**
 * Batchlet for deleting read notifications that are older than retention period.
 *
 * JSR-352 batchlet that executes as a single unit of resource to clean up
 * old read notifications from the database.
 */
@Named("deleteReadNotificationsBatchlet")
@Dependent
public class DeleteReadNotificationsBatchlet extends AbstractBatchlet {

    private static final Logger LOG = Logger.getLogger(DeleteReadNotificationsBatchlet.class);

    @Inject
    EntityManager entityManager;

    @Inject
    JobContext jobContext;

    /**
     * Number of days to retain read notifications.
     * Injected from job XML properties.
     */
    @Inject
    @BatchProperty(name = "retention.days")
    String retentionDays;

    /**
     * Process the batch job - delete read notifications older than retention period.
     *
     * @return "COMPLETED" if successful, "FAILED" if error occurs
     */
    @Override
    @Transactional
    public String process() throws Exception {
        LOG.info("Starting cleanup of read notifications...");

        try {
            // Parse retention days (default to 7 if not specified)
            int days = 7;
            if (retentionDays != null && !retentionDays.isEmpty()) {
                try {
                    days = Integer.parseInt(retentionDays);
                } catch (NumberFormatException e) {
                    LOG.warnf("Invalid retention.days value '%s', using default: 7", retentionDays);
                }
            }

            LOG.infof("Retention period: %d days", days);

            // Calculate cutoff date
            LocalDateTime cutoffDate = LocalDateTime.now().minusDays(days);
            LOG.infof("Deleting notifications read before: %s", cutoffDate);

            // Delete read notifications older than cutoff date
            // Using JPQL to delete directly without loading into memory
            int deletedCount = entityManager.createQuery(
                "DELETE FROM Notification n " +
                "WHERE n.isRead = true " +
                "AND n.readAt IS NOT NULL " +
                "AND n.readAt < :cutoffDate"
            )
            .setParameter("cutoffDate", cutoffDate)
            .executeUpdate();

            LOG.infof("Successfully deleted %d read notifications older than %d days",
                     deletedCount, days);

            // Store result in job context for reporting
            jobContext.setExitStatus(String.format("Deleted %d notifications", deletedCount));

            return "COMPLETED";

        } catch (Exception e) {
            LOG.errorf(e, "Error during notification cleanup");
            jobContext.setExitStatus("FAILED: " + e.getMessage());
            throw e;
        }
    }

    /**
     * Stop the batchlet (called if job is stopped).
     */
    @Override
    public void stop() throws Exception {
        LOG.warn("Cleanup job stopped by user");
    }
}
