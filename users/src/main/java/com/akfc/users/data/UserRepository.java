package com.akfc.users.data;

import io.quarkus.hibernate.orm.panache.PanacheRepository;
import io.quarkus.panache.common.Page;
import io.quarkus.panache.common.Sort;
import jakarta.enterprise.context.ApplicationScoped;
import jakarta.persistence.EntityManager;
import jakarta.persistence.criteria.*;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Repository for User entity using the REPOSITORY PATTERN.
 *
 * This demonstrates an alternative to Active Record pattern where
 * data access logic is separated from the entity.
 *
 * Includes examples of:
 * - Pagination with Panache
 * - Criteria API for dynamic queries
 * - Bulk operations
 * - Statistics and aggregations
 */
@ApplicationScoped
public class UserRepository implements PanacheRepository<User> {

    /**
     * EntityManager for advanced queries (Criteria API, native queries).
     * Automatically injected by Quarkus.
     */
    EntityManager em;

    // ========== PAGINATION EXAMPLES ==========

    /**
     * PAGINATION EXAMPLE 1: Simple pagination with page and size.
     *
     * Panache provides built-in pagination support through the Page class.
     * This is the most common and easiest way to paginate results.
     *
     * @param pageIndex Page number (0-based)
     * @param pageSize Number of items per page
     * @return List of users for the requested page
     */
    public List<User> findAllPaginated(int pageIndex, int pageSize) {
        // Create a Page object with index and size
        // Page indices are 0-based (first page is 0)
        return findAll()
            .page(Page.of(pageIndex, pageSize))
            .list();
    }

    /**
     * PAGINATION EXAMPLE 2: Pagination with sorting.
     *
     * Combines pagination with sorting for ordered results.
     * Sort can be by one or multiple fields, ascending or descending.
     *
     * @param pageIndex Page number (0-based)
     * @param pageSize Number of items per page
     * @param sortBy Field to sort by
     * @param ascending Sort direction
     * @return Sorted and paginated list of users
     */
    public List<User> findAllPaginatedAndSorted(int pageIndex, int pageSize,
                                                 String sortBy, boolean ascending) {
        Sort sort = ascending ? Sort.by(sortBy).ascending()
                              : Sort.by(sortBy).descending();

        return findAll(sort)
            .page(Page.of(pageIndex, pageSize))
            .list();
    }

    /**
     * PAGINATION EXAMPLE 3: Pagination with filtering.
     *
     * Demonstrates pagination combined with WHERE clause filtering.
     * Very common pattern for search results.
     *
     * @param role Role filter
     * @param pageIndex Page number (0-based)
     * @param pageSize Number of items per page
     * @return Filtered and paginated results
     */
    public List<User> findByRolePaginated(UserRole role, int pageIndex, int pageSize) {
        return find("role = ?1 AND archived = false", role)
            .page(Page.of(pageIndex, pageSize))
            .list();
    }

    /**
     * PAGINATION EXAMPLE 4: Get total page count.
     *
     * To build pagination UI, you need to know how many pages exist.
     * Panache provides pageCount() method for this.
     *
     * @param pageSize Items per page
     * @return Total number of pages
     */
    public int getTotalPages(int pageSize) {
        return find("archived = false")
            .page(Page.ofSize(pageSize))
            .pageCount();
    }

    /**
     * PAGINATION EXAMPLE 5: Get total count of items.
     *
     * Often needed to display "Showing X of Y results" in UI.
     *
     * @return Total number of non-archived users
     */
    public long getTotalCount() {
        return count("archived = false");
    }

    /**
     * PAGINATION EXAMPLE 6: Advanced pagination with multiple filters.
     *
     * Real-world example combining pagination, sorting, and multiple filters.
     * This is a typical "search users" endpoint implementation.
     *
     * @param role Optional role filter
     * @param status Optional status filter
     * @param nameSearch Optional name search
     * @param pageIndex Page number (0-based)
     * @param pageSize Items per page
     * @param sortBy Field to sort by
     * @param ascending Sort direction
     * @return Paginated search results
     */
    public PaginatedResult<User> searchUsers(UserRole role,
                                             AccountStatus status,
                                             String nameSearch,
                                             int pageIndex,
                                             int pageSize,
                                             String sortBy,
                                             boolean ascending) {
        // Build dynamic query
        StringBuilder query = new StringBuilder("archived = false");
        List<Object> params = new ArrayList<>();

        if (role != null) {
            query.append(" AND role = ?").append(params.size() + 1);
            params.add(role);
        }

        if (status != null) {
            query.append(" AND accountStatus = ?").append(params.size() + 1);
            params.add(status);
        }

        if (nameSearch != null && !nameSearch.isEmpty()) {
            query.append(" AND (LOWER(firstName) LIKE LOWER(?").append(params.size() + 1).append(")");
            query.append(" OR LOWER(lastName) LIKE LOWER(?").append(params.size() + 1).append("))");
            params.add("%" + nameSearch + "%");
        }

        // Create sort
        Sort sort = ascending ? Sort.by(sortBy).ascending()
                              : Sort.by(sortBy).descending();

        // Execute paginated query
        var paged = find(query.toString(), sort, params.toArray())
            .page(Page.of(pageIndex, pageSize));

        // Create result object with pagination metadata
        return new PaginatedResult<>(
            paged.list(),           // Current page items
            pageIndex,              // Current page index
            pageSize,               // Page size
            paged.pageCount(),      // Total pages
            paged.count()           // Total items
        );
    }

    // ========== CRITERIA API WITH PAGINATION ==========

    /**
     * CRITERIA API + PAGINATION: Dynamic query with type-safety and pagination.
     *
     * Demonstrates using Criteria API for complex queries with pagination.
     * Criteria API provides compile-time type safety that JPQL doesn't offer.
     *
     * @param role Optional role filter
     * @param status Optional status filter
     * @param registeredAfter Optional registration date filter
     * @param pageIndex Page number (0-based)
     * @param pageSize Items per page
     * @return Paginated results
     */
    public PaginatedResult<User> findWithCriteriaApiPaginated(UserRole role,
                                                               AccountStatus status,
                                                               LocalDate registeredAfter,
                                                               int pageIndex,
                                                               int pageSize) {
        CriteriaBuilder cb = em.getCriteriaBuilder();

        // Query for data
        CriteriaQuery<User> cq = cb.createQuery(User.class);
        Root<User> user = cq.from(User.class);

        // Build predicates
        List<Predicate> predicates = buildPredicates(cb, user, role, status, registeredAfter);

        cq.where(cb.and(predicates.toArray(new Predicate[0])));
        cq.orderBy(cb.desc(user.get("createdAt")));

        // Execute with pagination
        List<User> results = em.createQuery(cq)
            .setFirstResult(pageIndex * pageSize)  // Offset
            .setMaxResults(pageSize)                // Limit
            .getResultList();

        // Query for total count
        CriteriaQuery<Long> countQuery = cb.createQuery(Long.class);
        Root<User> countRoot = countQuery.from(User.class);
        List<Predicate> countPredicates = buildPredicates(cb, countRoot, role, status, registeredAfter);

        countQuery.select(cb.count(countRoot));
        countQuery.where(cb.and(countPredicates.toArray(new Predicate[0])));

        Long totalCount = em.createQuery(countQuery).getSingleResult();
        int totalPages = (int) Math.ceil((double) totalCount / pageSize);

        return new PaginatedResult<>(results, pageIndex, pageSize, totalPages, totalCount);
    }

    /**
     * Helper method to build predicates for Criteria API.
     * Extracted to avoid duplication between data and count queries.
     */
    private List<Predicate> buildPredicates(CriteriaBuilder cb,
                                           Root<User> user,
                                           UserRole role,
                                           AccountStatus status,
                                           LocalDate registeredAfter) {
        List<Predicate> predicates = new ArrayList<>();

        // Always exclude archived
        predicates.add(cb.isFalse(user.get("archived")));

        if (role != null) {
            predicates.add(cb.equal(user.get("role"), role));
        }

        if (status != null) {
            predicates.add(cb.equal(user.get("accountStatus"), status));
        }

        if (registeredAfter != null) {
            predicates.add(cb.greaterThanOrEqualTo(
                user.get("registrationDate"), registeredAfter
            ));
        }

        return predicates;
    }

    // ========== CRUD OPERATIONS ==========

    /**
     * CREATE: Create a new user and persist it.
     *
     * This is the repository pattern approach to creating users.
     * Alternatively, you can use User.create() for Active Record pattern.
     *
     * @param email User's email
     * @param firstName First name
     * @param lastName Last name
     * @param phoneNumber Phone number (optional)
     * @param createdBy User creating the account
     * @return The created and persisted user
     */
    public User createUser(String email, String firstName, String lastName,
                          String phoneNumber, String createdBy) {
        User user = new User();
        user.email = email;
        user.firstName = firstName;
        user.lastName = lastName;
        user.registrationDate = LocalDate.now();
        user.phoneNumber = phoneNumber;
        user.createdBy = createdBy;

        persist(user);
        return user;
    }

    /**
     * CREATE: Create a moderator user directly.
     *
     * @param email User's email
     * @param firstName First name
     * @param lastName Last name
     * @param createdBy Administrator creating the account
     * @return The created moderator
     */
    public User createModerator(String email, String firstName, String lastName, String createdBy) {
        User user = createUser(email, firstName, lastName, null, createdBy);
        user.role = UserRole.MODERATOR;
        user.accountStatus = AccountStatus.ACTIVE;
        user.statusChangedAt = LocalDateTime.now();
        persist(user);
        return user;
    }

    /**
     * READ: Find all active users (not archived).
     */
    public List<User> findAllActive() {
        return list("archived", false);
    }

    /**
     * READ: Find user by ID, only if not archived.
     */
    public User findActiveById(Long id) {
        return find("id = ?1 AND archived = false", id).firstResult();
    }

    /**
     * READ: Find user by ID, including archived ones.
     *
     * @param id User ID
     * @return User or null if not found
     */
    public User findByIdIncludingArchived(Long id) {
        return findById(id);
    }

    /**
     * READ: Find user by email address.
     *
     * @param email Email address (case-insensitive)
     * @return User or null if not found
     */
    public User findByEmail(String email) {
        return find("LOWER(email) = LOWER(?1) AND archived = false", email).firstResult();
    }

    /**
     * UPDATE: Update an existing user.
     *
     * @param id User ID to update
     * @param firstName New first name
     * @param lastName New last name
     * @param phoneNumber New phone number
     * @param modifiedBy User making the update
     * @return Updated user
     * @throws IllegalArgumentException if user not found
     */
    public User updateUser(Long id, String firstName, String lastName,
                          String phoneNumber, String modifiedBy) {
        User user = findActiveById(id);
        if (user == null) {
            throw new IllegalArgumentException("User not found or archived: " + id);
        }

        user.update(firstName, lastName, phoneNumber, modifiedBy);
        return user;
    }

    /**
     * DELETE (soft): Archive a user.
     *
     * @param id User ID to archive
     * @param reason Reason for archiving
     * @param archivedBy Administrator archiving the user
     * @return Archived user
     * @throws IllegalArgumentException if user not found
     */
    public User archiveUser(Long id, String reason, String archivedBy) {
        User user = findActiveById(id);
        if (user == null) {
            throw new IllegalArgumentException("User not found or already archived: " + id);
        }

        user.archive(reason, archivedBy);
        return user;
    }

    /**
     * UNDELETE: Restore an archived user.
     *
     * @param id User ID to restore
     * @param restoredBy Administrator restoring the user
     * @return Restored user
     * @throws IllegalArgumentException if user not found
     */
    public User restoreUser(Long id, String restoredBy) {
        User user = findById(id);
        if (user == null) {
            throw new IllegalArgumentException("User not found: " + id);
        }
        if (Boolean.FALSE.equals(user.archived)) {
            throw new IllegalStateException("User is not archived: " + id);
        }

        user.restore(restoredBy);
        return user;
    }

    /**
     * DELETE (hard): Permanently delete a user.
     *
     * WARNING: This cannot be undone!
     *
     * @param id User ID to delete
     * @throws IllegalArgumentException if user not found
     * @throws IllegalStateException if user not archived first
     */
    public void permanentlyDeleteUser(Long id) {
        User user = findById(id);
        if (user == null) {
            throw new IllegalArgumentException("User not found: " + id);
        }

        user.permanentlyDelete();
    }

    /**
     * EXISTS: Check if user exists by ID.
     *
     * @param id User ID
     * @return True if exists and not archived
     */
    public boolean existsById(Long id) {
        return count("id = ?1 AND archived = false", id) > 0;
    }

    /**
     * EXISTS: Check if user exists by email.
     *
     * @param email Email address
     * @return True if exists and not archived
     */
    public boolean existsByEmail(String email) {
        return count("LOWER(email) = LOWER(?1) AND archived = false", email) > 0;
    }

    // ========== BUSINESS METHODS ==========

    /**
     * BUSINESS METHOD 1: Find users by email domain.
     *
     * Useful for organizational filtering or analytics.
     *
     * @param domain Email domain (e.g., "gmail.com")
     * @return List of users with that email domain
     */
    public List<User> findByEmailDomain(String domain) {
        return list("LOWER(email) LIKE LOWER(?1) AND archived = false",
                   "%@" + domain);
    }

    /**
     * BUSINESS METHOD 2: Count users by email domain.
     *
     * Returns statistics on user email domains.
     *
     * @return Map of domain to user count
     */
    public List<Object[]> countByEmailDomain() {
        return em.createQuery(
            "SELECT SUBSTRING(u.email, LOCATE('@', u.email) + 1), COUNT(u) " +
            "FROM User u " +
            "WHERE u.archived = false " +
            "GROUP BY SUBSTRING(u.email, LOCATE('@', u.email) + 1) " +
            "ORDER BY COUNT(u) DESC",
            Object[].class
        ).getResultList();
    }

    /**
     * Find active users registered after a specific date using Criteria API.
     *
     * @param date Cutoff date
     * @return List of users registered after the date
     */
    public List<User> findActiveUsersRegisteredAfter(LocalDate date) {
        CriteriaBuilder cb = em.getCriteriaBuilder();
        CriteriaQuery<User> cq = cb.createQuery(User.class);
        Root<User> user = cq.from(User.class);

        cq.where(
            cb.and(
                cb.isFalse(user.get("archived")),
                cb.equal(user.get("accountStatus"), AccountStatus.ACTIVE),
                cb.greaterThanOrEqualTo(user.get("registrationDate"), date)
            )
        );

        cq.orderBy(cb.desc(user.get("registrationDate")));

        return em.createQuery(cq).getResultList();
    }

    /**
     * Find users without recent activity (inactive users).
     *
     * @param days Number of days of inactivity
     * @return List of inactive users with pagination
     */
    public List<User> findUsersWithoutActivity(int days, int pageIndex, int pageSize) {
        LocalDateTime cutoffDate = LocalDateTime.now().minusDays(days);

        return find("(lastLoginAt IS NULL OR lastLoginAt < ?1) " +
                   "AND accountStatus = ?2 AND archived = false " +
                   "ORDER BY lastLoginAt ASC NULLS FIRST",
                   cutoffDate, AccountStatus.ACTIVE)
            .page(Page.of(pageIndex, pageSize))
            .list();
    }

    // ========== BULK OPERATIONS ==========

    /**
     * Activate multiple user accounts at once.
     *
     * @param userIds List of user IDs to activate
     * @param activatedBy Administrator performing the operation
     * @return Number of users activated
     */
    public int activateMultiple(List<Long> userIds, String activatedBy) {
        return update(
            "accountStatus = ?1, statusChangedAt = ?2, modifiedBy = ?3 " +
            "WHERE id IN ?4 AND archived = false",
            AccountStatus.ACTIVE,
            LocalDateTime.now(),
            activatedBy,
            userIds
        );
    }

    /**
     * Suspend multiple user accounts at once.
     *
     * @param userIds List of user IDs to suspend
     * @param reason Reason for suspension
     * @param suspendedBy Administrator performing the operation
     * @return Number of users suspended
     */
    public int suspendMultiple(List<Long> userIds, String reason, String suspendedBy) {
        return update(
            "accountStatus = ?1, statusReason = ?2, statusChangedAt = ?3, modifiedBy = ?4 " +
            "WHERE id IN ?5 AND archived = false",
            AccountStatus.SUSPENDED,
            reason,
            LocalDateTime.now(),
            suspendedBy,
            userIds
        );
    }

    // ========== STATISTICS ==========

    /**
     * Get user registration statistics by month.
     *
     * @param year Year to analyze
     * @return Map of month to registration count
     */
    public Map<Integer, Long> getRegistrationStatsByMonth(int year) {
        List<Object[]> results = em.createQuery(
            "SELECT MONTH(u.registrationDate), COUNT(u) " +
            "FROM User u " +
            "WHERE YEAR(u.registrationDate) = ?1 AND u.archived = false " +
            "GROUP BY MONTH(u.registrationDate) " +
            "ORDER BY MONTH(u.registrationDate)",
            Object[].class
        )
        .setParameter(1, year)
        .getResultList();

        Map<Integer, Long> stats = new HashMap<>();
        for (Object[] row : results) {
            stats.put((Integer) row[0], (Long) row[1]);
        }
        return stats;
    }

    /**
     * Get user count by role and status.
     *
     * @return List of [role, status, count]
     */
    public List<Object[]> getUserCountByRoleAndStatus() {
        return em.createQuery(
            "SELECT u.role, u.accountStatus, COUNT(u) " +
            "FROM User u " +
            "WHERE u.archived = false " +
            "GROUP BY u.role, u.accountStatus " +
            "ORDER BY u.role, u.accountStatus",
            Object[].class
        ).getResultList();
    }

    /**
     * Inner class for paginated results with metadata.
     *
     * This provides a clean way to return pagination information
     * along with the actual data.
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

        public int getStartItem() {
            return currentPage * pageSize + 1;
        }

        public int getEndItem() {
            return Math.min((currentPage + 1) * pageSize, (int) totalItems);
        }
    }
}
