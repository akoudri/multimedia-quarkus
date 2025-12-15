package com.akfc.catalog.web;

import com.akfc.catalog.data.Resource;
import com.akfc.catalog.data.ResourceType;
import com.akfc.catalog.dto.CreateResourceRequest;
import com.akfc.catalog.services.ResourceService;
import com.microsoft.playwright.BrowserContext;
import com.microsoft.playwright.Locator;
import com.microsoft.playwright.Page;
import com.microsoft.playwright.options.LoadState;
import io.quarkiverse.playwright.InjectPlaywright;
import io.quarkiverse.playwright.WithPlaywright;
import io.quarkus.test.junit.QuarkusTest;
import jakarta.inject.Inject;
import jakarta.transaction.Transactional;
import org.eclipse.microprofile.config.inject.ConfigProperty;
import org.junit.jupiter.api.*;
import org.junit.jupiter.api.Disabled;

import java.util.Arrays;

import static com.microsoft.playwright.assertions.PlaywrightAssertions.assertThat;
import static org.junit.jupiter.api.Assertions.*;

/**
 * End-to-End tests for CatalogWebController using Playwright.
 *
 * These tests simulate real user interactions with the web interface:
 * - Navigation through pages
 * - Form submissions
 * - Search and filtering
 * - CRUD operations
 *
 * Playwright provides a powerful API for browser automation and assertions.
 *
 * @see <a href="https://docs.quarkiverse.io/quarkus-playwright/dev/index.html">Quarkus Playwright Documentation</a>
 */
@QuarkusTest
@WithPlaywright
@DisplayName("CatalogWebController E2E Tests (Playwright)")
@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
@Disabled("Requires Docker Dev Services - disabled due to Docker API version incompatibility")
class CatalogWebControllerE2ETest {

    @InjectPlaywright
    BrowserContext context;

    @Inject
    ResourceService resourceService;

    @ConfigProperty(name = "quarkus.http.test-port", defaultValue = "8081")
    int port;

    private Page page;
    private String baseUrl;
    private Long testResourceId;

    @BeforeEach
    @Transactional
    void setUp() {
        // Clean database before each test
        Resource.deleteAll();

        // Create a test resource
        CreateResourceRequest request = new CreateResourceRequest();
        request.title = "The Hobbit";
        request.type = ResourceType.BOOK;
        request.year = 1937;
        request.creator = "J.R.R. Tolkien";
        request.keywords = Arrays.asList("fantasy", "adventure");
        request.illustrationUrl = "http://example.com/hobbit.jpg";

        testResourceId = resourceService.createResource(request, "test").id;

        // Set up Playwright page
        page = context.newPage();
        baseUrl = "http://localhost:" + port + "/web/catalog";
    }

    @AfterEach
    void tearDown() {
        if (page != null) {
            page.close();
        }
    }

    // ========================================
    // LIST PAGE TESTS
    // ========================================

    @Test
    @Order(1)
    @DisplayName("Should display catalog list page with header")
    void shouldDisplayCatalogListPage() {
        // Navigate to catalog list
        page.navigate(baseUrl);
        page.waitForLoadState(LoadState.NETWORKIDLE);

        // Verify page title and header
        assertThat(page.locator("h1")).containsText("Catalog Resources");
        assertThat(page.locator("text=Manage your multimedia library collection")).isVisible();
    }

    @Test
    @Order(2)
    @DisplayName("Should display test resource in the list")
    void shouldDisplayTestResourceInList() {
        page.navigate(baseUrl);
        page.waitForLoadState(LoadState.NETWORKIDLE);

        // Verify resource card is displayed
        assertThat(page.locator(".card-title:has-text('The Hobbit')")).isVisible();
        assertThat(page.locator("text=J.R.R. Tolkien")).isVisible();
        assertThat(page.locator("text=1937")).isVisible();
    }

    @Test
    @Order(3)
    @DisplayName("Should show Add New Resource button")
    void shouldShowAddNewResourceButton() {
        page.navigate(baseUrl);
        page.waitForLoadState(LoadState.NETWORKIDLE);

        Locator addButton = page.locator("a:has-text('Add New Resource')");
        assertThat(addButton).isVisible();
        assertTrue(addButton.getAttribute("href").contains("/web/catalog/new"));
    }

    @Test
    @Order(4)
    @DisplayName("Should display empty state when no resources")
    @Transactional
    void shouldDisplayEmptyState() {
        // Delete all resources
        Resource.deleteAll();

        page.navigate(baseUrl);
        page.waitForLoadState(LoadState.NETWORKIDLE);

        // Verify empty state message
        assertThat(page.locator("text=No resources found")).isVisible();
        assertThat(page.locator("a:has-text('Add First Resource')")).isVisible();
    }

    // ========================================
    // SEARCH AND FILTER TESTS
    // ========================================

    @Test
    @Order(5)
    @DisplayName("Should filter resources by search query")
    void shouldFilterBySearchQuery() {
        page.navigate(baseUrl);
        page.waitForLoadState(LoadState.NETWORKIDLE);

        // Enter search query
        page.fill("#search", "Hobbit");
        page.click("button:has-text('Search')");
        page.waitForLoadState(LoadState.NETWORKIDLE);

        // Verify search results
        assertThat(page.locator(".card-title:has-text('The Hobbit')")).isVisible();
        assertThat(page.locator("text=for \"Hobbit\"")).isVisible();
    }

    @Test
    @Order(6)
    @DisplayName("Should filter resources by type")
    void shouldFilterByType() {
        page.navigate(baseUrl);
        page.waitForLoadState(LoadState.NETWORKIDLE);

        // Select type filter
        page.selectOption("#type", "BOOK");
        page.click("button:has-text('Search')");
        page.waitForLoadState(LoadState.NETWORKIDLE);

        // Verify filter is applied
        assertThat(page.locator(".badge:has-text('BOOK')")).isVisible();
        assertThat(page.locator(".card-title:has-text('The Hobbit')")).isVisible();
    }

    @Test
    @Order(7)
    @DisplayName("Should show no results for non-matching search")
    void shouldShowNoResultsForNonMatchingSearch() {
        page.navigate(baseUrl);
        page.waitForLoadState(LoadState.NETWORKIDLE);

        // Enter non-matching search query
        page.fill("#search", "NonExistentResource12345");
        page.click("button:has-text('Search')");
        page.waitForLoadState(LoadState.NETWORKIDLE);

        // Verify no results message
        assertThat(page.locator("text=No resources found")).isVisible();
    }

    @Test
    @Order(8)
    @DisplayName("Should clear filters when clicking Clear Filters")
    void shouldClearFilters() {
        // Navigate with filter
        page.navigate(baseUrl + "?search=Hobbit&type=BOOK");
        page.waitForLoadState(LoadState.NETWORKIDLE);

        // Click clear filters
        page.click("a:has-text('Clear Filters')");
        page.waitForLoadState(LoadState.NETWORKIDLE);

        // Verify filters are cleared (URL should not contain search/type)
        assertFalse(page.url().contains("search="));
        assertFalse(page.url().contains("type="));
    }

    // ========================================
    // VIEW DETAILS TESTS
    // ========================================

    @Test
    @Order(9)
    @DisplayName("Should navigate to resource detail view")
    void shouldNavigateToDetailView() {
        page.navigate(baseUrl);
        page.waitForLoadState(LoadState.NETWORKIDLE);

        // Click view button
        page.click("a:has-text('View')");
        page.waitForLoadState(LoadState.NETWORKIDLE);

        // Verify we're on detail page
        assertTrue(page.url().contains("/web/catalog/" + testResourceId));
        assertThat(page.locator("h2:has-text('The Hobbit')")).isVisible();
        assertThat(page.locator("text=J.R.R. Tolkien")).isVisible();
    }

    @Test
    @Order(10)
    @DisplayName("Should display resource metadata in detail view")
    void shouldDisplayResourceMetadata() {
        page.navigate(baseUrl + "/" + testResourceId);
        page.waitForLoadState(LoadState.NETWORKIDLE);

        // Verify metadata section
        assertThat(page.locator("text=Metadata")).isVisible();
        assertThat(page.locator("code:has-text('" + testResourceId + "')")).isVisible();
    }

    @Test
    @Order(11)
    @DisplayName("Should show actions panel in detail view")
    void shouldShowActionsPanel() {
        page.navigate(baseUrl + "/" + testResourceId);
        page.waitForLoadState(LoadState.NETWORKIDLE);

        // Verify actions
        assertThat(page.locator("a:has-text('Edit Resource')")).isVisible();
        assertThat(page.locator("button:has-text('Delete Resource')")).isVisible();
    }

    @Test
    @Order(12)
    @DisplayName("Should navigate back to catalog from detail view")
    void shouldNavigateBackToCatalog() {
        page.navigate(baseUrl + "/" + testResourceId);
        page.waitForLoadState(LoadState.NETWORKIDLE);

        // Click back button
        page.click("a:has-text('Back to Catalog')");
        page.waitForLoadState(LoadState.NETWORKIDLE);

        // Verify we're back on list page
        assertEquals(baseUrl, page.url());
    }

    // ========================================
    // CREATE RESOURCE TESTS
    // ========================================

    @Test
    @Order(13)
    @DisplayName("Should navigate to create form")
    void shouldNavigateToCreateForm() {
        page.navigate(baseUrl);
        page.waitForLoadState(LoadState.NETWORKIDLE);

        // Click add new resource button
        page.click("a:has-text('Add New Resource')");
        page.waitForLoadState(LoadState.NETWORKIDLE);

        // Verify we're on create form
        assertTrue(page.url().contains("/web/catalog/new"));
        assertThat(page.locator("h1:has-text('Create New Resource')")).isVisible();
    }

    @Test
    @Order(14)
    @DisplayName("Should display create form with all fields")
    void shouldDisplayCreateFormFields() {
        page.navigate(baseUrl + "/new");
        page.waitForLoadState(LoadState.NETWORKIDLE);

        // Verify all form fields are present
        assertThat(page.locator("#title")).isVisible();
        assertThat(page.locator("#type")).isVisible();
        assertThat(page.locator("#creator")).isVisible();
        assertThat(page.locator("#year")).isVisible();
        assertThat(page.locator("#illustrationUrl")).isVisible();

        // Verify submit button
        assertThat(page.locator("button:has-text('Create Resource')")).isVisible();
    }

    @Test
    @Order(15)
    @DisplayName("Should create new resource via form")
    void shouldCreateNewResource() {
        page.navigate(baseUrl + "/new");
        page.waitForLoadState(LoadState.NETWORKIDLE);

        // Fill the form
        page.fill("#title", "1984");
        page.selectOption("#type", "BOOK");
        page.fill("#creator", "George Orwell");
        page.fill("#year", "1949");
        page.fill("#illustrationUrl", "http://example.com/1984.jpg");

        // Submit the form
        page.click("button:has-text('Create Resource')");
        page.waitForLoadState(LoadState.NETWORKIDLE);

        // Verify redirect to detail page with success message
        assertTrue(page.url().contains("/web/catalog/"));
        assertThat(page.locator("h2:has-text('1984')")).isVisible();
    }

    @Test
    @Order(16)
    @DisplayName("Should cancel create and return to list")
    void shouldCancelCreate() {
        page.navigate(baseUrl + "/new");
        page.waitForLoadState(LoadState.NETWORKIDLE);

        // Click cancel
        page.click("a:has-text('Cancel')");
        page.waitForLoadState(LoadState.NETWORKIDLE);

        // Verify we're back on list page
        assertEquals(baseUrl, page.url());
    }

    // ========================================
    // EDIT RESOURCE TESTS
    // ========================================

    @Test
    @Order(17)
    @DisplayName("Should navigate to edit form")
    void shouldNavigateToEditForm() {
        page.navigate(baseUrl);
        page.waitForLoadState(LoadState.NETWORKIDLE);

        // Click edit button
        page.click("a:has-text('Edit')");
        page.waitForLoadState(LoadState.NETWORKIDLE);

        // Verify we're on edit form
        assertTrue(page.url().contains("/edit"));
        assertThat(page.locator("h1:has-text('Edit Resource')")).isVisible();
    }

    @Test
    @Order(18)
    @DisplayName("Should pre-fill edit form with existing values")
    void shouldPreFillEditForm() {
        page.navigate(baseUrl + "/" + testResourceId + "/edit");
        page.waitForLoadState(LoadState.NETWORKIDLE);

        // Verify form is pre-filled
        assertEquals("The Hobbit", page.inputValue("#title"));
        assertEquals("J.R.R. Tolkien", page.inputValue("#creator"));
        assertEquals("1937", page.inputValue("#year"));
    }

    @Test
    @Order(19)
    @DisplayName("Should show type as read-only in edit form")
    void shouldShowTypeAsReadOnlyInEditForm() {
        page.navigate(baseUrl + "/" + testResourceId + "/edit");
        page.waitForLoadState(LoadState.NETWORKIDLE);

        // Verify type is displayed but not editable
        assertThat(page.locator(".badge:has-text('BOOK')")).isVisible();
        assertThat(page.locator("text=cannot be changed")).isVisible();
    }

    @Test
    @Order(20)
    @DisplayName("Should update resource via edit form")
    void shouldUpdateResource() {
        page.navigate(baseUrl + "/" + testResourceId + "/edit");
        page.waitForLoadState(LoadState.NETWORKIDLE);

        // Update title
        page.fill("#title", "The Hobbit (Updated)");

        // Submit the form
        page.click("button:has-text('Save Changes')");
        page.waitForLoadState(LoadState.NETWORKIDLE);

        // Verify redirect to detail page with updated title
        assertTrue(page.url().contains("/web/catalog/" + testResourceId));
        assertThat(page.locator("h2:has-text('The Hobbit (Updated)')")).isVisible();
    }

    // ========================================
    // DELETE RESOURCE TESTS
    // ========================================

    @Test
    @Order(21)
    @DisplayName("Should show delete button in list")
    void shouldShowDeleteButtonInList() {
        page.navigate(baseUrl);
        page.waitForLoadState(LoadState.NETWORKIDLE);

        assertThat(page.locator("button:has-text('Delete')")).isVisible();
    }

    @Test
    @Order(22)
    @DisplayName("Should delete resource from list page")
    void shouldDeleteResourceFromList() {
        page.navigate(baseUrl);
        page.waitForLoadState(LoadState.NETWORKIDLE);

        // Set up dialog handler to accept confirmation
        page.onDialog(dialog -> dialog.accept());

        // Click delete button
        page.click("button:has-text('Delete')");
        page.waitForLoadState(LoadState.NETWORKIDLE);

        // Verify redirect to list with success message
        assertEquals(baseUrl, page.url().split("\\?")[0]);
        // Resource should no longer be visible
        assertThat(page.locator(".card-title:has-text('The Hobbit')")).not().isVisible();
    }

    @Test
    @Order(23)
    @DisplayName("Should delete resource from detail page")
    @Transactional
    void shouldDeleteResourceFromDetailPage() {
        // Create a new resource for this test
        CreateResourceRequest request = new CreateResourceRequest();
        request.title = "Resource To Delete";
        request.type = ResourceType.MOVIE;
        request.year = 2020;
        request.creator = "Test Director";

        Long resourceToDeleteId = resourceService.createResource(request, "test").id;

        page.navigate(baseUrl + "/" + resourceToDeleteId);
        page.waitForLoadState(LoadState.NETWORKIDLE);

        // Set up dialog handler
        page.onDialog(dialog -> dialog.accept());

        // Click delete button
        page.click("button:has-text('Delete Resource')");
        page.waitForLoadState(LoadState.NETWORKIDLE);

        // Verify redirect to list
        assertEquals(baseUrl, page.url().split("\\?")[0]);
    }

    // ========================================
    // NAVIGATION AND BREADCRUMB TESTS
    // ========================================

    @Test
    @Order(24)
    @DisplayName("Should display breadcrumb navigation in detail view")
    void shouldDisplayBreadcrumbInDetailView() {
        page.navigate(baseUrl + "/" + testResourceId);
        page.waitForLoadState(LoadState.NETWORKIDLE);

        // Verify breadcrumb
        assertThat(page.locator("nav[aria-label='breadcrumb']")).isVisible();
        assertThat(page.locator(".breadcrumb-item a:has-text('Catalog')")).isVisible();
    }

    @Test
    @Order(25)
    @DisplayName("Should navigate via breadcrumb")
    void shouldNavigateViaBreadcrumb() {
        page.navigate(baseUrl + "/" + testResourceId);
        page.waitForLoadState(LoadState.NETWORKIDLE);

        // Click breadcrumb link
        page.click(".breadcrumb-item a:has-text('Catalog')");
        page.waitForLoadState(LoadState.NETWORKIDLE);

        // Verify navigation to list
        assertEquals(baseUrl, page.url());
    }

    // ========================================
    // SUCCESS/ERROR MESSAGE TESTS
    // ========================================

    @Test
    @Order(26)
    @DisplayName("Should display success message")
    void shouldDisplaySuccessMessage() {
        page.navigate(baseUrl + "?message=Resource%20created%20successfully");
        page.waitForLoadState(LoadState.NETWORKIDLE);

        assertThat(page.locator("text=Resource created successfully")).isVisible();
    }

    @Test
    @Order(27)
    @DisplayName("Should display error message")
    void shouldDisplayErrorMessage() {
        page.navigate(baseUrl + "?error=Something%20went%20wrong");
        page.waitForLoadState(LoadState.NETWORKIDLE);

        assertThat(page.locator("text=Something went wrong")).isVisible();
    }

    // ========================================
    // PAGINATION TESTS
    // ========================================

    @Test
    @Order(28)
    @DisplayName("Should display pagination info")
    @Transactional
    void shouldDisplayPaginationInfo() {
        // Create multiple resources
        for (int i = 1; i <= 15; i++) {
            CreateResourceRequest req = new CreateResourceRequest();
            req.title = "Resource " + i;
            req.type = ResourceType.BOOK;
            req.year = 2000 + i;
            req.creator = "Author " + i;
            resourceService.createResource(req, "test");
        }

        page.navigate(baseUrl);
        page.waitForLoadState(LoadState.NETWORKIDLE);

        // Verify pagination info is displayed
        assertThat(page.locator("text=Page 1 of")).isVisible();
    }

    @Test
    @Order(29)
    @DisplayName("Should navigate to next page")
    @Transactional
    void shouldNavigateToNextPage() {
        // Create enough resources for pagination
        for (int i = 1; i <= 15; i++) {
            CreateResourceRequest req = new CreateResourceRequest();
            req.title = "Paginated Resource " + i;
            req.type = ResourceType.BOOK;
            req.year = 2000 + i;
            req.creator = "Author " + i;
            resourceService.createResource(req, "test");
        }

        page.navigate(baseUrl);
        page.waitForLoadState(LoadState.NETWORKIDLE);

        // Click next page
        Locator nextButton = page.locator("a:has-text('Next')");
        if (nextButton.isVisible()) {
            nextButton.click();
            page.waitForLoadState(LoadState.NETWORKIDLE);

            // Verify page changed
            assertTrue(page.url().contains("page=1"));
        }
    }

    // ========================================
    // RESOURCE TYPE BADGE TESTS
    // ========================================

    @Test
    @Order(30)
    @DisplayName("Should display correct badge color for resource type")
    void shouldDisplayCorrectBadgeColor() {
        page.navigate(baseUrl);
        page.waitForLoadState(LoadState.NETWORKIDLE);

        // BOOK type should have primary (blue) badge
        Locator bookBadge = page.locator(".badge.bg-primary:has-text('BOOK')");
        assertThat(bookBadge).isVisible();
    }

    // ========================================
    // RESPONSIVE AND UI TESTS
    // ========================================

    @Test
    @Order(31)
    @DisplayName("Should display resource cards in grid layout")
    void shouldDisplayCardsInGrid() {
        page.navigate(baseUrl);
        page.waitForLoadState(LoadState.NETWORKIDLE);

        // Verify card grid structure
        assertThat(page.locator(".row.g-4")).isVisible();
        assertThat(page.locator(".resource-card")).isVisible();
    }

    @Test
    @Order(32)
    @DisplayName("Should have working form validation attributes")
    void shouldHaveFormValidationAttributes() {
        page.navigate(baseUrl + "/new");
        page.waitForLoadState(LoadState.NETWORKIDLE);

        // Verify required attributes
        Locator titleInput = page.locator("#title");
        assertEquals("true", titleInput.getAttribute("required"));

        Locator typeSelect = page.locator("#type");
        assertEquals("true", typeSelect.getAttribute("required"));
    }

    // ========================================
    // ERROR HANDLING TESTS
    // ========================================

    @Test
    @Order(33)
    @DisplayName("Should redirect when viewing non-existent resource")
    void shouldRedirectForNonExistentResource() {
        page.navigate(baseUrl + "/99999");
        page.waitForLoadState(LoadState.NETWORKIDLE);

        // Should redirect to list with error
        assertTrue(page.url().contains("/web/catalog"));
        assertTrue(page.url().contains("error="));
    }

    @Test
    @Order(34)
    @DisplayName("Should redirect when editing non-existent resource")
    void shouldRedirectForNonExistentEdit() {
        page.navigate(baseUrl + "/99999/edit");
        page.waitForLoadState(LoadState.NETWORKIDLE);

        // Should redirect to list with error
        assertTrue(page.url().contains("/web/catalog"));
        assertTrue(page.url().contains("error="));
    }
}
