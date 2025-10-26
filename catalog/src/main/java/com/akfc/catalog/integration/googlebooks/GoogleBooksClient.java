package com.akfc.catalog.integration.googlebooks;

import jakarta.ws.rs.GET;
import jakarta.ws.rs.Path;
import jakarta.ws.rs.QueryParam;
import org.eclipse.microprofile.rest.client.inject.RegisterRestClient;

/**
 * REST Client for Google Books API.
 *
 * Documentation: https://developers.google.com/books/docs/v1/using
 *
 * Base URL: https://www.googleapis.com/books/v1
 *
 * Usage example:
 * <pre>
 * {@code
 * @Inject
 * @RestClient
 * GoogleBooksClient googleBooksClient;
 *
 * GoogleBooksResponse response = googleBooksClient.searchBooks("inauthor:Jules Verne", 40);
 * }
 * </pre>
 */
@Path("/volumes")
@RegisterRestClient(configKey = "google-books-api")
public interface GoogleBooksClient {

    /**
     * Search for books using the Google Books API.
     *
     * @param query Search query. Supports various prefixes:
     *              - inauthor:name (search by author)
     *              - intitle:title (search by title)
     *              - isbn:number (search by ISBN)
     *              - subject:subject (search by subject)
     * @param maxResults Maximum number of results to return (default: 10, max: 40)
     * @return Google Books API response with list of volumes
     *
     * Example queries:
     * - "inauthor:Jules Verne" - Books by Jules Verne
     * - "inauthor:Victor Hugo" - Books by Victor Hugo
     * - "intitle:Harry Potter" - Books with title containing "Harry Potter"
     */
    @GET
    GoogleBooksResponse searchBooks(
        @QueryParam("q") String query,
        @QueryParam("maxResults") Integer maxResults
    );

    /**
     * Search for books using default max results (40).
     *
     * @param query Search query
     * @return Google Books API response
     */
    default GoogleBooksResponse searchBooks(String query) {
        return searchBooks(query, 40);
    }

    /**
     * Search books by author name.
     *
     * @param authorName Author name to search for
     * @param maxResults Maximum number of results
     * @return Google Books API response
     */
    default GoogleBooksResponse searchByAuthor(String authorName, Integer maxResults) {
        return searchBooks("inauthor:" + authorName, maxResults);
    }

    /**
     * Search books by author name with default max results (40).
     *
     * @param authorName Author name to search for
     * @return Google Books API response
     */
    default GoogleBooksResponse searchByAuthor(String authorName) {
        return searchByAuthor(authorName, 40);
    }
}
