package com.akfc.catalog.data;

import io.quarkus.hibernate.orm.panache.PanacheRepository;
import jakarta.enterprise.context.ApplicationScoped;
import jakarta.persistence.EntityManager;
import jakarta.persistence.criteria.*;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;

/**
 * Repository for Resource entity using the REPOSITORY PATTERN.
 *
 * This class demonstrates an alternative to Active Record pattern.
 * Instead of placing business logic in the entity, we separate concerns:
 * - Entity: Pure data model
 * - Repository: Data access and query logic
 *
 * Advantages of Repository Pattern:
 * - Better separation of concerns
 * - Easier to test (can mock repository)
 * - More suitable for complex domain models
 * - Cleaner entity classes
 *
 * This repository is automatically discovered by Quarkus CDI due to @ApplicationScoped.
 */
@ApplicationScoped
public class ResourceRepository implements PanacheRepository<Resource> {

    /**
     * EntityManager for advanced queries (Criteria API, native queries).
     * Automatically injected by Quarkus.
     */
    EntityManager em;

    // ========== CRUD OPERATIONS ==========

    /**
     * CREATE: Create a new resource and persist it.
     *
     * This is the repository pattern approach to creating resources.
     * Alternatively, you can use Resource.create() for Active Record pattern.
     *
     * @param title Resource title
     * @param type Resource type
     * @param year Publication year
     * @param creator Creator name
     * @param keywords Keywords list
     * @param illustrationUrl Illustration URL
     * @param createdBy User creating the resource
     * @return The created and persisted resource
     */
    public Resource createResource(String title, ResourceType type, Integer year,
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

        persist(resource);
        return resource;
    }

    /**
     * READ: Find all non-archived resources.
     *
     * @return List of active resources
     */
    public List<Resource> findAllActive() {
        return list("archived", false);
    }

    /**
     * READ: Find resource by ID, only if not archived.
     *
     * @param id Resource ID
     * @return Resource or null if not found or archived
     */
    public Resource findActiveById(Long id) {
        return find("id = ?1 AND archived = false", id).firstResult();
    }

    /**
     * READ: Find resource by ID, including archived ones.
     *
     * @param id Resource ID
     * @return Resource or null if not found
     */
    public Resource findByIdIncludingArchived(Long id) {
        return findById(id);
    }

    /**
     * UPDATE: Update an existing resource.
     *
     * @param id Resource ID to update
     * @param title New title
     * @param year New year
     * @param creator New creator
     * @param keywords New keywords
     * @param illustrationUrl New illustration URL
     * @param modifiedBy User making the update
     * @return Updated resource
     * @throws IllegalArgumentException if resource not found
     */
    public Resource updateResource(Long id, String title, Integer year,
                                   String creator, List<String> keywords,
                                   String illustrationUrl, String modifiedBy) {
        Resource resource = findActiveById(id);
        if (resource == null) {
            throw new IllegalArgumentException("Resource not found or archived: " + id);
        }

        resource.update(title, year, creator, keywords, illustrationUrl, modifiedBy);
        return resource;
    }

    /**
     * DELETE (soft): Archive a resource.
     *
     * @param id Resource ID to archive
     * @param reason Reason for archiving
     * @param archivedBy User archiving the resource
     * @return Archived resource
     * @throws IllegalArgumentException if resource not found
     */
    public Resource archiveResource(Long id, String reason, String archivedBy) {
        Resource resource = findActiveById(id);
        if (resource == null) {
            throw new IllegalArgumentException("Resource not found or already archived: " + id);
        }

        resource.archive(reason, archivedBy);
        return resource;
    }

    /**
     * UNDELETE: Restore an archived resource.
     *
     * @param id Resource ID to restore
     * @param restoredBy User restoring the resource
     * @return Restored resource
     * @throws IllegalArgumentException if resource not found
     */
    public Resource restoreResource(Long id, String restoredBy) {
        Resource resource = findById(id);
        if (resource == null) {
            throw new IllegalArgumentException("Resource not found: " + id);
        }
        if (Boolean.FALSE.equals(resource.archived)) {
            throw new IllegalStateException("Resource is not archived: " + id);
        }

        resource.restore(restoredBy);
        return resource;
    }

    /**
     * DELETE (hard): Permanently delete a resource.
     *
     * WARNING: This cannot be undone!
     *
     * @param id Resource ID to delete
     * @throws IllegalArgumentException if resource not found
     * @throws IllegalStateException if resource not archived first
     */
    public void permanentlyDeleteResource(Long id) {
        Resource resource = findById(id);
        if (resource == null) {
            throw new IllegalArgumentException("Resource not found: " + id);
        }

        resource.permanentlyDelete();
    }

    /**
     * EXISTS: Check if resource exists by ID.
     *
     * @param id Resource ID
     * @return True if exists and not archived
     */
    public boolean existsById(Long id) {
        return count("id = ?1 AND archived = false", id) > 0;
    }

    /**
     * EXISTS: Check if resource exists by ID, including archived.
     *
     * @param id Resource ID
     * @return True if exists (including archived)
     */
    public boolean existsByIdIncludingArchived(Long id) {
        return count("id", id) > 0;
    }

    // ========== CUSTOM BUSINESS QUERIES ==========

    /**
     * BUSINESS METHOD 1: Find resources by multiple criteria with optional filters.
     *
     * This method demonstrates building dynamic queries based on provided parameters.
     * Uses Panache's flexible query building.
     *
     * @param type Optional resource type filter
     * @param status Optional status filter
     * @param yearFrom Optional minimum year
     * @param yearTo Optional maximum year
     * @return List of resources matching all provided criteria
     */
    public List<Resource> findByCriteria(ResourceType type, ResourceStatus status,
                                         Integer yearFrom, Integer yearTo) {
        StringBuilder query = new StringBuilder("archived = false");
        List<Object> params = new ArrayList<>();

        if (type != null) {
            query.append(" AND type = ?").append(params.size() + 1);
            params.add(type);
        }

        if (status != null) {
            query.append(" AND status = ?").append(params.size() + 1);
            params.add(status);
        }

        if (yearFrom != null) {
            query.append(" AND year >= ?").append(params.size() + 1);
            params.add(yearFrom);
        }

        if (yearTo != null) {
            query.append(" AND year <= ?").append(params.size() + 1);
            params.add(yearTo);
        }

        return list(query.toString(), params.toArray());
    }

    /**
     * BUSINESS METHOD 2: Find popular resources by keyword frequency.
     *
     * This demonstrates a complex query joining with the keywords collection
     * and grouping to find resources with most keywords.
     *
     * @param minKeywords Minimum number of keywords
     * @return List of resources sorted by keyword count (descending)
     */
    public List<Resource> findPopularByKeywordCount(int minKeywords) {
        return find(
            "SELECT r FROM Resource r " +
            "WHERE r.archived = false " +
            "AND SIZE(r.keywords) >= ?1 " +
            "ORDER BY SIZE(r.keywords) DESC",
            minKeywords
        ).list();
    }

    // ========== CRITERIA API EXAMPLE ==========

    /**
     * CRITERIA API: Find resources with dynamic filters using JPA Criteria API.
     *
     * The Criteria API provides type-safe, programmatic query construction.
     * It's more verbose than JPQL but offers:
     * - Compile-time type checking
     * - IDE auto-completion
     * - Easier dynamic query building
     * - Better refactoring support
     *
     * This example finds resources matching all provided criteria:
     * - Type (optional)
     * - Status (optional)
     * - Created after a specific date (optional)
     * - Title contains text (optional, case-insensitive)
     *
     * @param type Optional resource type
     * @param status Optional status
     * @param createdAfter Optional date filter
     * @param titleContains Optional title search
     * @return List of resources matching all criteria
     */
    public List<Resource> findWithCriteriaApi(ResourceType type,
                                              ResourceStatus status,
                                              LocalDateTime createdAfter,
                                              String titleContains) {
        // Step 1: Get CriteriaBuilder from EntityManager
        CriteriaBuilder cb = em.getCriteriaBuilder();

        // Step 2: Create CriteriaQuery for Resource entity
        CriteriaQuery<Resource> cq = cb.createQuery(Resource.class);

        // Step 3: Define the root entity (FROM clause)
        Root<Resource> resource = cq.from(Resource.class);

        // Step 4: Build predicates (WHERE conditions) dynamically
        List<Predicate> predicates = new ArrayList<>();

        // Always exclude archived resources
        predicates.add(cb.isFalse(resource.get("archived")));

        // Add type filter if provided
        if (type != null) {
            predicates.add(cb.equal(resource.get("type"), type));
        }

        // Add status filter if provided
        if (status != null) {
            predicates.add(cb.equal(resource.get("status"), status));
        }

        // Add creation date filter if provided
        if (createdAfter != null) {
            predicates.add(cb.greaterThanOrEqualTo(
                resource.get("createdAt"), createdAfter
            ));
        }

        // Add title search if provided (case-insensitive LIKE)
        if (titleContains != null && !titleContains.isEmpty()) {
            predicates.add(cb.like(
                cb.lower(resource.get("title")),
                "%" + titleContains.toLowerCase() + "%"
            ));
        }

        // Step 5: Combine all predicates with AND
        cq.where(cb.and(predicates.toArray(new Predicate[0])));

        // Step 6: Add ordering (optional)
        cq.orderBy(cb.desc(resource.get("createdAt")));

        // Step 7: Execute query and return results
        return em.createQuery(cq).getResultList();
    }

    /**
     * CRITERIA API: Advanced example with aggregation.
     *
     * Find resources grouped by type with count and average year.
     * Demonstrates GROUP BY and aggregate functions with Criteria API.
     *
     * @return List of Object arrays: [type, count, avgYear]
     */
    public List<Object[]> getStatisticsByType() {
        CriteriaBuilder cb = em.getCriteriaBuilder();
        CriteriaQuery<Object[]> cq = cb.createQuery(Object[].class);
        Root<Resource> resource = cq.from(Resource.class);

        // SELECT type, COUNT(*), AVG(year)
        cq.multiselect(
            resource.get("type"),
            cb.count(resource),
            cb.avg(resource.get("year"))
        );

        // WHERE archived = false
        cq.where(cb.isFalse(resource.get("archived")));

        // GROUP BY type
        cq.groupBy(resource.get("type"));

        // ORDER BY count DESC
        cq.orderBy(cb.desc(cb.count(resource)));

        return em.createQuery(cq).getResultList();
    }

    /**
     * CRITERIA API: Complex join example.
     *
     * Find resources that have specific keywords using Criteria API join.
     *
     * @param keyword Keyword to search for
     * @return List of resources containing the keyword
     */
    public List<Resource> findByKeywordWithCriteria(String keyword) {
        CriteriaBuilder cb = em.getCriteriaBuilder();
        CriteriaQuery<Resource> cq = cb.createQuery(Resource.class);
        Root<Resource> resource = cq.from(Resource.class);

        // Join with keywords collection
        Join<Resource, String> keywords = resource.join("keywords", JoinType.INNER);

        // WHERE keyword LIKE '%search%' AND archived = false
        cq.where(
            cb.and(
                cb.like(cb.lower(keywords.as(String.class)),
                       "%" + keyword.toLowerCase() + "%"),
                cb.isFalse(resource.get("archived"))
            )
        );

        // DISTINCT to avoid duplicates when resource has multiple matching keywords
        cq.distinct(true);

        return em.createQuery(cq).getResultList();
    }

    // ========== PAGINATION WITH REPOSITORY PATTERN ==========

    /**
     * PAGINATION: Find resources with pagination using repository pattern.
     *
     * @param pageIndex Page number (0-based)
     * @param pageSize Items per page
     * @return Paginated resources
     */
    public List<Resource> findAllPaginated(int pageIndex, int pageSize) {
        return findAll()
            .page(io.quarkus.panache.common.Page.of(pageIndex, pageSize))
            .list();
    }

    /**
     * PAGINATION: Advanced pagination with Criteria API.
     *
     * Returns paginated results with metadata for UI display.
     *
     * @param type Optional type filter
     * @param status Optional status filter
     * @param pageIndex Page number (0-based)
     * @param pageSize Items per page
     * @return Paginated result with metadata
     */
    public PaginatedResult<Resource> findResourcesPaginated(ResourceType type,
                                                            ResourceStatus status,
                                                            int pageIndex,
                                                            int pageSize) {
        CriteriaBuilder cb = em.getCriteriaBuilder();

        // Build query
        CriteriaQuery<Resource> cq = cb.createQuery(Resource.class);
        Root<Resource> resource = cq.from(Resource.class);

        List<Predicate> predicates = new ArrayList<>();
        predicates.add(cb.isFalse(resource.get("archived")));

        if (type != null) {
            predicates.add(cb.equal(resource.get("type"), type));
        }
        if (status != null) {
            predicates.add(cb.equal(resource.get("status"), status));
        }

        cq.where(cb.and(predicates.toArray(new Predicate[0])));
        cq.orderBy(cb.desc(resource.get("createdAt")));

        // Execute with pagination
        List<Resource> results = em.createQuery(cq)
            .setFirstResult(pageIndex * pageSize)
            .setMaxResults(pageSize)
            .getResultList();

        // Get total count
        CriteriaQuery<Long> countQuery = cb.createQuery(Long.class);
        Root<Resource> countRoot = countQuery.from(Resource.class);

        List<Predicate> countPredicates = new ArrayList<>();
        countPredicates.add(cb.isFalse(countRoot.get("archived")));
        if (type != null) {
            countPredicates.add(cb.equal(countRoot.get("type"), type));
        }
        if (status != null) {
            countPredicates.add(cb.equal(countRoot.get("status"), status));
        }

        countQuery.select(cb.count(countRoot));
        countQuery.where(cb.and(countPredicates.toArray(new Predicate[0])));

        Long totalCount = em.createQuery(countQuery).getSingleResult();
        int totalPages = (int) Math.ceil((double) totalCount / pageSize);

        return new PaginatedResult<>(results, pageIndex, pageSize, totalPages, totalCount);
    }

    // ========== BULK OPERATIONS ==========

    /**
     * Archive multiple resources at once.
     *
     * @param ids List of resource IDs to archive
     * @param reason Reason for archiving
     * @param archivedBy User performing the operation
     * @return Number of resources archived
     */
    public int archiveMultiple(List<Long> ids, String reason, String archivedBy) {
        return update(
            "archived = true, archiveReason = ?1, modifiedBy = ?2, status = ?3 " +
            "WHERE id IN ?4 AND archived = false",
            reason, archivedBy, ResourceStatus.UNAVAILABLE, ids
        );
    }

    /**
     * Update status for multiple resources.
     *
     * @param ids List of resource IDs
     * @param newStatus New status to set
     * @param modifiedBy User performing the operation
     * @return Number of resources updated
     */
    public int updateStatusBulk(List<Long> ids, ResourceStatus newStatus, String modifiedBy) {
        return update(
            "status = ?1, modifiedBy = ?2 WHERE id IN ?3 AND archived = false",
            newStatus, modifiedBy, ids
        );
    }

    /**
     * Inner class for paginated results with metadata.
     */
    public static class PaginatedResult<T> {
        public final List<T> data;
        public final int currentPage;
        public final int pageSize;
        public final int totalPages;
        public final long totalItems;

        public PaginatedResult(List<T> data, int currentPage, int pageSize,
                              int totalPages, long totalItems) {
            this.data = data;
            this.currentPage = currentPage;
            this.pageSize = pageSize;
            this.totalPages = totalPages;
            this.totalItems = totalItems;
        }

        public boolean hasNextPage() {
            return currentPage < totalPages - 1;
        }

        public boolean hasPreviousPage() {
            return currentPage > 0;
        }
    }
}
