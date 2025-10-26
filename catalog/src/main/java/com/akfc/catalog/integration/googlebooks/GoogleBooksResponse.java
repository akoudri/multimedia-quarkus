package com.akfc.catalog.integration.googlebooks;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

import java.util.List;

/**
 * Root response from Google Books API.
 *
 * Example API call:
 * https://www.googleapis.com/books/v1/volumes?q=inauthor:Jules+Verne
 *
 * Sample response structure:
 * {
 *   "kind": "books#volumes",
 *   "totalItems": 1234,
 *   "items": [ ... ]
 * }
 */
@JsonIgnoreProperties(ignoreUnknown = true)
public class GoogleBooksResponse {

    /**
     * Total number of items available (may be more than returned).
     */
    public Integer totalItems;

    /**
     * List of book volumes returned by the search.
     */
    public List<BookVolume> items;

    /**
     * Individual book volume from Google Books API.
     */
    @JsonIgnoreProperties(ignoreUnknown = true)
    public static class BookVolume {

        /**
         * Volume ID from Google Books.
         */
        public String id;

        /**
         * Main book information.
         */
        public VolumeInfo volumeInfo;
    }

    /**
     * Detailed information about a book volume.
     */
    @JsonIgnoreProperties(ignoreUnknown = true)
    public static class VolumeInfo {

        /**
         * Book title.
         */
        public String title;

        /**
         * Book subtitle (optional).
         */
        public String subtitle;

        /**
         * List of authors.
         */
        public List<String> authors;

        /**
         * Publisher name.
         */
        public String publisher;

        /**
         * Publication date (format may vary: YYYY, YYYY-MM, YYYY-MM-DD).
         */
        public String publishedDate;

        /**
         * Book description/synopsis.
         */
        public String description;

        /**
         * Page count.
         */
        public Integer pageCount;

        /**
         * Main category.
         */
        public List<String> categories;

        /**
         * Average rating (0.0 to 5.0).
         */
        public Double averageRating;

        /**
         * Number of ratings.
         */
        public Integer ratingsCount;

        /**
         * Image links (thumbnail, small thumbnail, etc.).
         */
        public ImageLinks imageLinks;

        /**
         * Industry identifiers (ISBN_10, ISBN_13, etc.).
         */
        public List<IndustryIdentifier> industryIdentifiers;

        /**
         * Main language of the book.
         */
        public String language;
    }

    /**
     * Image links for book cover.
     */
    @JsonIgnoreProperties(ignoreUnknown = true)
    public static class ImageLinks {

        /**
         * Small thumbnail URL.
         */
        public String smallThumbnail;

        /**
         * Thumbnail URL (preferred).
         */
        public String thumbnail;
    }

    /**
     * Industry identifier (ISBN, etc.).
     */
    @JsonIgnoreProperties(ignoreUnknown = true)
    public static class IndustryIdentifier {

        /**
         * Type of identifier (ISBN_10, ISBN_13, ISSN, etc.).
         */
        public String type;

        /**
         * Identifier value.
         */
        public String identifier;
    }
}
