package com.akfc.catalog.integration.googlebooks;

import com.akfc.catalog.data.Resource;
import com.akfc.catalog.data.ResourceRepository;
import io.quarkus.cache.CacheInvalidateAll;
import io.quarkus.runtime.StartupEvent;
import jakarta.enterprise.context.ApplicationScoped;
import jakarta.enterprise.event.Observes;
import jakarta.inject.Inject;
import jakarta.transaction.Transactional;
import org.eclipse.microprofile.config.inject.ConfigProperty;
import org.eclipse.microprofile.rest.client.inject.RestClient;
import org.jboss.logging.Logger;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

/**
 * Service that synchronizes books from Google Books API at application startup.
 *
 * This service:
 * 1. Observes the StartupEvent to trigger on application start
 * 2. Fetches books for configured authors from Google Books API
 * 3. Converts API responses to Resource entities
 * 4. Saves books to database, avoiding duplicates
 *
 * Configuration:
 * - googlebooks.sync.enabled: Enable/disable sync (default: true)
 * - googlebooks.sync.authors: Comma-separated list of authors to sync
 * - googlebooks.sync.max-results-per-author: Max books per author (default: 40)
 */
@ApplicationScoped
public class GoogleBooksSyncService {

    private static final Logger LOG = Logger.getLogger(GoogleBooksSyncService.class);

    @Inject
    @RestClient
    GoogleBooksClient googleBooksClient;

    @Inject
    GoogleBooksMapper googleBooksMapper;

    @Inject
    ResourceRepository resourceRepository;

    @ConfigProperty(name = "googlebooks.sync.enabled", defaultValue = "true")
    boolean syncEnabled;

    @ConfigProperty(name = "googlebooks.sync.authors", defaultValue = "Jules Verne,Victor Hugo,Ali Koudri")
    String authorsToSync;

    @ConfigProperty(name = "googlebooks.sync.max-results-per-author", defaultValue = "40")
    int maxResultsPerAuthor;

    /**
     * Trigger book synchronization when application starts.
     *
     * This method observes the StartupEvent and is called automatically by Quarkus
     * after the application has started and all beans are initialized.
     *
     * @param event Startup event (automatically injected by CDI)
     */
    public void onStart(@Observes StartupEvent event) {
        if (!syncEnabled) {
            LOG.info("Google Books sync is disabled via configuration");
            return;
        }

        LOG.info("=".repeat(80));
        LOG.info("Starting Google Books synchronization...");
        LOG.info("=".repeat(80));

        try {
            syncBooks();
        } catch (Exception e) {
            LOG.error("Failed to sync books from Google Books API", e);
            // Don't fail application startup if sync fails
        }

        LOG.info("=".repeat(80));
        LOG.info("Google Books synchronization completed");
        LOG.info("=".repeat(80));
    }

    /**
     * Synchronize books for all configured authors.
     *
     * IMPORTANT: Invalidates all resource caches because this method creates
     * resources directly via repository (bypassing service layer cache annotations).
     */
    @Transactional
    @CacheInvalidateAll(cacheName = "all-resources")
    @CacheInvalidateAll(cacheName = "resources-by-type")
    @CacheInvalidateAll(cacheName = "resources-by-keyword")
    public void syncBooks() {
        List<String> authors = parseAuthors(authorsToSync);

        LOG.infof("Syncing books for %d authors: %s", authors.size(), String.join(", ", authors));

        int totalBooksFetched = 0;
        int totalBooksSaved = 0;
        int totalDuplicates = 0;

        for (String author : authors) {
            try {
                SyncResult result = syncBooksForAuthor(author);
                totalBooksFetched += result.fetched;
                totalBooksSaved += result.saved;
                totalDuplicates += result.duplicates;
            } catch (Exception e) {
                LOG.errorf(e, "Failed to sync books for author: %s", author);
                // Continue with next author
            }
        }

        LOG.infof("Sync summary: %d books fetched, %d saved, %d duplicates skipped",
            totalBooksFetched, totalBooksSaved, totalDuplicates);
    }

    /**
     * Synchronize books for a single author.
     *
     * @param authorName Author name to sync
     * @return Sync result statistics
     */
    private SyncResult syncBooksForAuthor(String authorName) {
        LOG.infof("Fetching books for author: %s", authorName);

        try {
            // Call Google Books API
            GoogleBooksResponse response = googleBooksClient.searchByAuthor(authorName, maxResultsPerAuthor);

            if (response == null || response.items == null || response.items.isEmpty()) {
                LOG.warnf("No books found for author: %s", authorName);
                return new SyncResult(0, 0, 0);
            }

            LOG.infof("Found %d books for author: %s", response.items.size(), authorName);

            // Convert to Resource entities
            List<Resource> resources = googleBooksMapper.toResources(response);
            LOG.infof("Successfully converted %d/%d books to resources",
                resources.size(), response.items.size());

            // Save to database
            int saved = 0;
            int duplicates = 0;

            for (Resource resource : resources) {
                if (isDuplicate(resource)) {
                    duplicates++;
                    LOG.debugf("Skipping duplicate: %s by %s", resource.title, resource.creator);
                } else {
                    resourceRepository.createResource(
                        resource.title,
                        resource.type,
                        resource.year,
                        resource.creator,
                        resource.keywords,
                        resource.illustrationUrl,
                        "google-books-sync"
                    );
                    saved++;
                    LOG.debugf("Saved: %s by %s (%d)", resource.title, resource.creator, resource.year);
                }
            }

            LOG.infof("Saved %d new books for author %s (%d duplicates skipped)",
                saved, authorName, duplicates);

            return new SyncResult(response.items.size(), saved, duplicates);

        } catch (Exception e) {
            LOG.errorf(e, "Error syncing books for author: %s", authorName);
            throw e;
        }
    }

    /**
     * Check if a resource already exists in the database.
     * Considers a book duplicate if same title and creator already exists.
     *
     * @param resource Resource to check
     * @return true if duplicate, false otherwise
     */
    private boolean isDuplicate(Resource resource) {
        // Search for existing books with same title and creator
        List<Resource> existing = Resource.find(
            "LOWER(title) = LOWER(?1) AND LOWER(creator) = LOWER(?2) AND archived = false",
            resource.title,
            resource.creator
        ).list();

        return !existing.isEmpty();
    }

    /**
     * Parse comma-separated author names from configuration.
     *
     * @param authorsConfig Comma-separated author names
     * @return List of author names
     */
    private List<String> parseAuthors(String authorsConfig) {
        if (authorsConfig == null || authorsConfig.isBlank()) {
            return new ArrayList<>();
        }

        return Arrays.stream(authorsConfig.split(","))
            .map(String::trim)
            .filter(s -> !s.isEmpty())
            .toList();
    }

    /**
     * Result statistics for a sync operation.
     */
    private static class SyncResult {
        final int fetched;
        final int saved;
        final int duplicates;

        SyncResult(int fetched, int saved, int duplicates) {
            this.fetched = fetched;
            this.saved = saved;
            this.duplicates = duplicates;
        }
    }
}
