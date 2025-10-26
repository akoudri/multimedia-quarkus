package com.akfc.catalog.data;

import io.quarkus.hibernate.orm.panache.PanacheEntityBase;
import jakarta.persistence.*;
import jakarta.validation.constraints.*;
import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;
import org.hibernate.annotations.CreationTimestamp;
import org.hibernate.annotations.UpdateTimestamp;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;

/**
 * Entity representing a multimedia resource (book, movie, or music).
 *
 * This entity uses the ACTIVE RECORD pattern, inherited from PanacheEntityBase.
 * Business logic and persistence methods are defined directly in the entity.
 *
 * Lifecycle Management:
 * - Automatic timestamps using Hibernate annotations
 * - Status tracking for resource availability
 * - Soft delete with archived flag
 * - Full validation with Jakarta Bean Validation
 *
 * Examples:
 * 1. Book: id=1, title="1984", type=BOOK, year=1949, creator="George Orwell",
 *    keywords=["dystopia", "surveillance", "totalitarianism"], status=AVAILABLE
 *
 * 2. Movie: id=2, title="Inception", type=MOVIE, year=2010, creator="Christopher Nolan",
 *    keywords=["sci-fi", "thriller", "dreams"], status=AVAILABLE
 */
@Entity
@Table(name = "resources", indexes = {
    @Index(name = "idx_resource_title", columnList = "title"),
    @Index(name = "idx_resource_type", columnList = "type"),
    @Index(name = "idx_resource_year", columnList = "year"),
    @Index(name = "idx_resource_status", columnList = "status"),
    @Index(name = "idx_resource_archived", columnList = "archived")
})
@Cache(usage = CacheConcurrencyStrategy.READ_WRITE)
public class Resource extends PanacheEntityBase {

    // ========== PRIMARY KEY ==========

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    public Long id;

    // ========== BUSINESS FIELDS ==========

    /**
     * Title of the resource.
     * Required field with length constraints.
     */
    @NotBlank(message = "Title is required")
    @Size(min = 1, max = 255, message = "Title must be between 1 and 255 characters")
    @Column(nullable = false, length = 255)
    public String title;

    /**
     * Type of resource: BOOK, MOVIE, or MUSIC.
     * Stored as string in database for readability.
     */
    @NotNull(message = "Type is required")
    @Enumerated(EnumType.STRING)
    @Column(nullable = false, length = 20)
    public ResourceType type;

    /**
     * Publication/Release year.
     * Validated to be within reasonable bounds.
     */
    @NotNull(message = "Year is required")
    @Min(value = 1000, message = "Year must be at least 1000")
    @Max(value = 9999, message = "Year must be at most 9999")
    @Column(nullable = false)
    public Integer year;

    /**
     * Creator of the resource (author/director/artist).
     * Indexed for search performance.
     */
    @NotBlank(message = "Creator is required")
    @Size(min = 1, max = 255, message = "Creator name must be between 1 and 255 characters")
    @Column(nullable = false, length = 255)
    public String creator;

    /**
     * Keywords for search and categorization.
     * Stored in a separate table with eager loading for search optimization.
     */
    @ElementCollection(fetch = FetchType.EAGER)
    @CollectionTable(name = "resource_keywords", joinColumns = @JoinColumn(name = "resource_id"))
    @Column(name = "keyword", length = 100)
    public List<String> keywords = new ArrayList<>();

    /**
     * Optional URL to resource illustration/cover.
     */
    @Size(max = 500, message = "Illustration URL must not exceed 500 characters")
    @Column(name = "illustration_url", length = 500)
    public String illustrationUrl;

    // ========== STATUS & LIFECYCLE FIELDS ==========

    /**
     * Current status of the resource.
     * Determines if resource is available for borrowing/viewing.
     * Defaults to AVAILABLE on creation.
     */
    @NotNull(message = "Status is required")
    @Enumerated(EnumType.STRING)
    @Column(nullable = false, length = 20)
    public ResourceStatus status;

    /**
     * Soft delete flag.
     * When true, resource is hidden from normal queries but retained in database.
     * Allows for data recovery and audit trail.
     */
    @Column(nullable = false)
    public Boolean archived = false;

    /**
     * Reason for archiving (if archived).
     * Useful for audit and reporting.
     */
    @Size(max = 500, message = "Archive reason must not exceed 500 characters")
    @Column(name = "archive_reason", length = 500)
    public String archiveReason;

    // ========== AUDIT/TIMESTAMP FIELDS ==========

    /**
     * Automatic creation timestamp.
     * Set once when entity is first persisted, never updated.
     * Uses Hibernate's @CreationTimestamp for automatic management.
     */
    @CreationTimestamp
    @Column(name = "created_at", nullable = false, updatable = false)
    public LocalDateTime createdAt;

    /**
     * Automatic update timestamp.
     * Updated every time entity is modified.
     * Uses Hibernate's @UpdateTimestamp for automatic management.
     */
    @UpdateTimestamp
    @Column(name = "updated_at", nullable = false)
    public LocalDateTime updatedAt;

    /**
     * User who created this resource (for audit trail).
     * In a real application, this would reference a User ID.
     */
    @Size(max = 100, message = "Created by must not exceed 100 characters")
    @Column(name = "created_by", length = 100)
    public String createdBy;

    /**
     * User who last modified this resource (for audit trail).
     */
    @Size(max = 100, message = "Modified by must not exceed 100 characters")
    @Column(name = "modified_by", length = 100)
    public String modifiedBy;

    // ========== JPA LIFECYCLE CALLBACKS ==========

    /**
     * Called automatically before entity is persisted for the first time.
     * Initializes default values for status and archived flag.
     *
     * Note: @CreationTimestamp and @UpdateTimestamp handle timestamp fields automatically,
     * so we don't need to set them manually here.
     */
    @PrePersist
    protected void onCreate() {
        // Set default status if not explicitly set
        if (status == null) {
            status = ResourceStatus.AVAILABLE;
        }

        // Ensure archived flag is set
        if (archived == null) {
            archived = false;
        }
    }

    /**
     * Called automatically before entity is updated.
     * Can be used for validation or additional business logic.
     *
     * Example: Prevent updates to archived resources
     */
    @PreUpdate
    protected void onUpdate() {
        // Business rule: Cannot modify essential fields of archived resources
        if (Boolean.TRUE.equals(archived) && modifiedBy == null) {
            throw new IllegalStateException("Cannot update archived resource without specifying modifier");
        }
    }

    // ========== ACTIVE RECORD PATTERN - BUSINESS METHODS ==========

    /**
     * Find all resources by type.
     *
     * Active Record pattern: static method on entity.
     *
     * @param type Resource type filter
     * @return List of resources matching the type
     */
    public static List<Resource> findByType(ResourceType type) {
        return list("type = ?1 AND archived = false", type);
    }

    /**
     * Find all resources by creator (case-insensitive partial match).
     * Excludes archived resources.
     *
     * @param creator Creator name (partial match supported)
     * @return List of resources by creator
     */
    public static List<Resource> findByCreator(String creator) {
        return list("LOWER(creator) LIKE LOWER(?1) AND archived = false",
                    "%" + creator + "%");
    }

    /**
     * Find all resources by year.
     * Excludes archived resources.
     *
     * @param year Publication year
     * @return List of resources from that year
     */
    public static List<Resource> findByYear(Integer year) {
        return list("year = ?1 AND archived = false", year);
    }

    /**
     * Find all resources containing a specific keyword.
     * Uses JOIN to search through the keywords collection.
     * Excludes archived resources.
     *
     * @param keyword Keyword to search for (partial match)
     * @return List of resources with matching keyword
     */
    public static List<Resource> findByKeyword(String keyword) {
        return find("SELECT DISTINCT r FROM Resource r JOIN r.keywords k " +
                   "WHERE LOWER(k) LIKE LOWER(?1) AND r.archived = false",
                   "%" + keyword + "%").list();
    }

    /**
     * Search resources by title (case-insensitive partial match).
     * Excludes archived resources.
     *
     * @param title Title to search for (partial match)
     * @return List of resources with matching title
     */
    public static List<Resource> searchByTitle(String title) {
        return list("LOWER(title) LIKE LOWER(?1) AND archived = false",
                    "%" + title + "%");
    }

    /**
     * BUSINESS METHOD: Find all available resources by type.
     *
     * This is a typical business query combining multiple criteria:
     * - Resources of specific type
     * - Only available status
     * - Not archived
     *
     * @param type Resource type
     * @return List of available resources
     */
    public static List<Resource> findAvailableByType(ResourceType type) {
        return list("type = ?1 AND status = ?2 AND archived = false",
                   type, ResourceStatus.AVAILABLE);
    }

    /**
     * BUSINESS METHOD: Find recently added resources.
     *
     * Returns resources created within the last N days.
     * Useful for "New Arrivals" features.
     * Ordered by creation date descending (newest first).
     *
     * @param days Number of days to look back
     * @return List of recent resources
     */
    public static List<Resource> findRecentlyAdded(int days) {
        LocalDateTime cutoffDate = LocalDateTime.now().minusDays(days);
        return list("createdAt >= ?1 AND archived = false ORDER BY createdAt DESC",
                   cutoffDate);
    }

    /**
     * BUSINESS METHOD: Count available resources by type.
     *
     * @param type Resource type
     * @return Count of available resources
     */
    public static long countAvailableByType(ResourceType type) {
        return count("type = ?1 AND status = ?2 AND archived = false",
                    type, ResourceStatus.AVAILABLE);
    }

    // ========== PAGINATION EXAMPLES (Active Record Pattern) ==========

    /**
     * PAGINATION: Find all resources with pagination.
     *
     * Active Record pattern pagination using Panache's Page API.
     *
     * @param pageIndex Page number (0-based)
     * @param pageSize Number of items per page
     * @return List of resources for the requested page
     */
    public static List<Resource> findAllPaginated(int pageIndex, int pageSize) {
        return findAll()
            .page(io.quarkus.panache.common.Page.of(pageIndex, pageSize))
            .list();
    }

    /**
     * PAGINATION: Find resources by type with pagination and sorting.
     *
     * @param type Resource type filter
     * @param pageIndex Page number (0-based)
     * @param pageSize Items per page
     * @return Paginated and sorted resources
     */
    public static List<Resource> findByTypePaginated(ResourceType type, int pageIndex, int pageSize) {
        return find("type = ?1 AND archived = false",
                   io.quarkus.panache.common.Sort.by("createdAt").descending(),
                   type)
            .page(io.quarkus.panache.common.Page.of(pageIndex, pageSize))
            .list();
    }

    /**
     * PAGINATION: Search resources with pagination.
     *
     * Combines search with pagination for user-friendly browsing.
     *
     * @param searchTerm Search term for title or creator
     * @param pageIndex Page number (0-based)
     * @param pageSize Items per page
     * @return Paginated search results
     */
    public static List<Resource> searchPaginated(String searchTerm, int pageIndex, int pageSize) {
        return find("(LOWER(title) LIKE LOWER(?1) OR LOWER(creator) LIKE LOWER(?1)) " +
                   "AND archived = false",
                   io.quarkus.panache.common.Sort.by("title").ascending(),
                   "%" + searchTerm + "%")
            .page(io.quarkus.panache.common.Page.of(pageIndex, pageSize))
            .list();
    }

    /**
     * PAGINATION: Get total number of pages for a query.
     *
     * @param pageSize Items per page
     * @return Total number of pages
     */
    public static int getTotalPages(int pageSize) {
        return find("archived = false")
            .page(io.quarkus.panache.common.Page.ofSize(pageSize))
            .pageCount();
    }

    // ========== INSTANCE METHODS FOR LIFECYCLE MANAGEMENT ==========

    // ========== CREATE ==========

    /**
     * Create a new resource and persist it to the database.
     *
     * This is a convenient factory method that:
     * - Creates a new Resource instance
     * - Sets all required fields
     * - Automatically sets createdBy audit field
     * - Persists to database
     * - Returns the created resource with generated ID
     *
     * @param title Resource title
     * @param type Resource type (BOOK, MOVIE, MUSIC)
     * @param year Publication/release year
     * @param creator Author/director/artist name
     * @param keywords List of keywords for search
     * @param illustrationUrl Optional URL to cover/poster image
     * @param createdBy User creating the resource
     * @return The created and persisted resource
     */
    public static Resource create(String title, ResourceType type, Integer year,
                                  String creator, List<String> keywords,
                                  String illustrationUrl, String createdBy) {
        Resource resource = new Resource();
        resource.title = title;
        resource.type = type;
        resource.year = year;
        resource.creator = creator;
        resource.keywords = keywords != null ? new ArrayList<>(keywords) : new ArrayList<>();
        resource.illustrationUrl = illustrationUrl;
        resource.createdBy = createdBy;
        // status and archived will be set by @PrePersist

        resource.persist();
        return resource;
    }

    /**
     * Simplified create method without optional fields.
     *
     * @param title Resource title
     * @param type Resource type
     * @param year Publication year
     * @param creator Creator name
     * @param createdBy User creating the resource
     * @return The created and persisted resource
     */
    public static Resource create(String title, ResourceType type, Integer year,
                                  String creator, String createdBy) {
        return create(title, type, year, creator, null, null, createdBy);
    }

    // ========== UPDATE ==========

    /**
     * Update this resource's basic information.
     *
     * Updates title, year, creator, and keywords. Does not change type or status.
     *
     * @param title New title (or same if not changing)
     * @param year New year
     * @param creator New creator
     * @param keywords New keywords list
     * @param illustrationUrl New illustration URL
     * @param modifiedBy User making the update
     */
    public void update(String title, Integer year, String creator,
                      List<String> keywords, String illustrationUrl,
                      String modifiedBy) {
        if (Boolean.TRUE.equals(archived)) {
            throw new IllegalStateException("Cannot update archived resource. Restore it first.");
        }

        this.title = title;
        this.year = year;
        this.creator = creator;
        this.keywords = keywords != null ? new ArrayList<>(keywords) : new ArrayList<>();
        this.illustrationUrl = illustrationUrl;
        this.modifiedBy = modifiedBy;

        // updatedAt will be set automatically by @UpdateTimestamp
        this.persist();
    }

    /**
     * Update only the title of this resource.
     *
     * @param newTitle New title
     * @param modifiedBy User making the change
     */
    public void updateTitle(String newTitle, String modifiedBy) {
        if (Boolean.TRUE.equals(archived)) {
            throw new IllegalStateException("Cannot update archived resource");
        }
        this.title = newTitle;
        this.modifiedBy = modifiedBy;
        this.persist();
    }

    /**
     * Add a keyword to this resource.
     *
     * @param keyword Keyword to add
     * @param modifiedBy User making the change
     */
    public void addKeyword(String keyword, String modifiedBy) {
        if (Boolean.TRUE.equals(archived)) {
            throw new IllegalStateException("Cannot update archived resource");
        }
        if (!this.keywords.contains(keyword)) {
            this.keywords.add(keyword);
            this.modifiedBy = modifiedBy;
            this.persist();
        }
    }

    /**
     * Remove a keyword from this resource.
     *
     * @param keyword Keyword to remove
     * @param modifiedBy User making the change
     */
    public void removeKeyword(String keyword, String modifiedBy) {
        if (Boolean.TRUE.equals(archived)) {
            throw new IllegalStateException("Cannot update archived resource");
        }
        if (this.keywords.remove(keyword)) {
            this.modifiedBy = modifiedBy;
            this.persist();
        }
    }

    // ========== STATUS CHANGES ==========

    /**
     * Mark resource as unavailable.
     *
     * @param modifiedBy User making the change
     */
    public void markUnavailable(String modifiedBy) {
        if (Boolean.TRUE.equals(archived)) {
            throw new IllegalStateException("Cannot change status of archived resource");
        }
        this.status = ResourceStatus.UNAVAILABLE;
        this.modifiedBy = modifiedBy;
        this.persist();
    }

    /**
     * Mark resource as available.
     *
     * @param modifiedBy User making the change
     */
    public void markAvailable(String modifiedBy) {
        if (Boolean.TRUE.equals(archived)) {
            throw new IllegalStateException("Cannot mark archived resource as available");
        }
        this.status = ResourceStatus.AVAILABLE;
        this.modifiedBy = modifiedBy;
        this.persist();
    }

    /**
     * Mark resource as reserved.
     *
     * @param modifiedBy User making the change
     */
    public void markReserved(String modifiedBy) {
        if (Boolean.TRUE.equals(archived)) {
            throw new IllegalStateException("Cannot change status of archived resource");
        }
        this.status = ResourceStatus.RESERVED;
        this.modifiedBy = modifiedBy;
        this.persist();
    }

    // ========== SOFT DELETE / ARCHIVE ==========

    /**
     * Archive this resource (soft delete).
     *
     * Soft delete keeps the record in the database but marks it as archived.
     * The resource will be excluded from normal queries and marked as unavailable.
     *
     * @param reason Reason for archiving
     * @param archivedBy User performing the archive
     */
    public void archive(String reason, String archivedBy) {
        this.archived = true;
        this.archiveReason = reason;
        this.modifiedBy = archivedBy;
        this.status = ResourceStatus.UNAVAILABLE;
        this.persist();
    }

    /**
     * Restore an archived resource.
     *
     * Unarchives the resource and sets it back to available status.
     *
     * @param restoredBy User performing the restoration
     */
    public void restore(String restoredBy) {
        this.archived = false;
        this.archiveReason = null;
        this.modifiedBy = restoredBy;
        this.status = ResourceStatus.AVAILABLE;
        this.persist();
    }

    // ========== HARD DELETE ==========

    /**
     * Permanently delete this resource from the database.
     *
     * WARNING: This is a hard delete and cannot be undone!
     * Use archive() for soft delete instead in most cases.
     *
     * This should only be used when:
     * - Data retention policies require permanent deletion
     * - The resource was created by mistake
     * - Legal requirements mandate data removal
     *
     * @throws IllegalStateException if resource is not archived first
     */
    public void permanentlyDelete() {
        if (Boolean.FALSE.equals(archived)) {
            throw new IllegalStateException(
                "Cannot permanently delete a non-archived resource. Archive it first for safety.");
        }
        this.delete();
    }

    /**
     * Force delete without archiving first.
     *
     * DANGEROUS: Use with extreme caution!
     *
     * @param confirmedBy User confirming the deletion
     */
    public void forceDelete(String confirmedBy) {
        // Log or audit this dangerous operation
        this.modifiedBy = confirmedBy;
        this.delete();
    }
}
