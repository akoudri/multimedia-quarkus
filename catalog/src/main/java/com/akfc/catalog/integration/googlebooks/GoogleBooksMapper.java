package com.akfc.catalog.integration.googlebooks;

import com.akfc.catalog.data.Resource;
import com.akfc.catalog.data.ResourceType;
import jakarta.enterprise.context.ApplicationScoped;
import org.jboss.logging.Logger;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Mapper to convert Google Books API responses to Resource entities.
 *
 * Handles data transformation and cleanup from Google Books API format
 * to our internal Resource entity format.
 */
@ApplicationScoped
public class GoogleBooksMapper {

    private static final Logger LOG = Logger.getLogger(GoogleBooksMapper.class);

    // Pattern to extract year from various date formats (YYYY, YYYY-MM, YYYY-MM-DD)
    private static final Pattern YEAR_PATTERN = Pattern.compile("^(\\d{4})");

    /**
     * Convert Google Books volume to Resource entity.
     *
     * @param volume Google Books volume from API
     * @return Resource entity, or null if conversion fails
     */
    public Resource toResource(GoogleBooksResponse.BookVolume volume) {
        if (volume == null || volume.volumeInfo == null) {
            return null;
        }

        GoogleBooksResponse.VolumeInfo info = volume.volumeInfo;

        try {
            Resource resource = new Resource();

            // Title (required)
            resource.title = extractTitle(info);
            if (resource.title == null || resource.title.isBlank()) {
                LOG.warnf("Skipping volume %s: no title", volume.id);
                return null;
            }

            // Type (always BOOK for Google Books API)
            resource.type = ResourceType.BOOK;

            // Year (required)
            resource.year = extractYear(info.publishedDate);
            if (resource.year == null) {
                LOG.warnf("Skipping book '%s': no valid year", resource.title);
                return null;
            }

            // Creator/Author (required)
            resource.creator = extractCreator(info.authors);
            if (resource.creator == null || resource.creator.isBlank()) {
                LOG.warnf("Skipping book '%s': no author", resource.title);
                return null;
            }

            // Keywords (from categories)
            resource.keywords = extractKeywords(info.categories);

            // Illustration URL
            resource.illustrationUrl = extractImageUrl(info.imageLinks);

            // Audit fields
            resource.createdBy = "google-books-sync";
            resource.archived = false;

            return resource;

        } catch (Exception e) {
            LOG.errorf(e, "Error converting volume %s to Resource", volume.id);
            return null;
        }
    }

    /**
     * Extract title from volume info.
     * Combines title and subtitle if available.
     *
     * @param info Volume info
     * @return Book title
     */
    private String extractTitle(GoogleBooksResponse.VolumeInfo info) {
        if (info.title == null) {
            return null;
        }

        String title = info.title.trim();

        // Add subtitle if available and not too long
        if (info.subtitle != null && !info.subtitle.isBlank()) {
            String subtitle = info.subtitle.trim();
            // Limit combined length to 250 chars (leaving room for ": ")
            if (title.length() + subtitle.length() + 2 <= 250) {
                title = title + ": " + subtitle;
            }
        }

        // Truncate if too long (max 255 chars for title field)
        if (title.length() > 255) {
            title = title.substring(0, 252) + "...";
        }

        return title;
    }

    /**
     * Extract year from published date string.
     * Handles various formats: "YYYY", "YYYY-MM", "YYYY-MM-DD".
     *
     * @param publishedDate Published date string from API
     * @return Year as integer, or null if invalid
     */
    private Integer extractYear(String publishedDate) {
        if (publishedDate == null || publishedDate.isBlank()) {
            return null;
        }

        Matcher matcher = YEAR_PATTERN.matcher(publishedDate.trim());
        if (matcher.find()) {
            try {
                int year = Integer.parseInt(matcher.group(1));
                // Validate year range
                if (year >= 1000 && year <= 9999) {
                    return year;
                }
            } catch (NumberFormatException e) {
                LOG.debugf("Invalid year format: %s", publishedDate);
            }
        }

        return null;
    }

    /**
     * Extract creator (author) from authors list.
     * Combines multiple authors with commas.
     *
     * @param authors List of authors
     * @return Combined author string, or null if empty
     */
    private String extractCreator(List<String> authors) {
        if (authors == null || authors.isEmpty()) {
            return null;
        }

        // Join multiple authors with ", "
        String creator = String.join(", ", authors);

        // Truncate if too long (max 255 chars)
        if (creator.length() > 255) {
            creator = creator.substring(0, 252) + "...";
        }

        return creator;
    }

    /**
     * Extract keywords from categories.
     * Limits to first 5 categories.
     *
     * @param categories List of categories
     * @return List of keywords
     */
    private List<String> extractKeywords(List<String> categories) {
        List<String> keywords = new ArrayList<>();

        if (categories != null && !categories.isEmpty()) {
            // Take first 5 categories as keywords
            int limit = Math.min(5, categories.size());
            for (int i = 0; i < limit; i++) {
                String category = categories.get(i);
                if (category != null && !category.isBlank()) {
                    // Clean up category (remove "/" and make lowercase)
                    String keyword = category.trim()
                        .replace("/", " ")
                        .toLowerCase();

                    // Truncate if too long (max 100 chars per keyword)
                    if (keyword.length() > 100) {
                        keyword = keyword.substring(0, 100);
                    }

                    keywords.add(keyword);
                }
            }
        }

        return keywords;
    }

    /**
     * Extract image URL from image links.
     * Prefers thumbnail over smallThumbnail.
     *
     * @param imageLinks Image links from API
     * @return Image URL, or null if not available
     */
    private String extractImageUrl(GoogleBooksResponse.ImageLinks imageLinks) {
        if (imageLinks == null) {
            return null;
        }

        // Prefer thumbnail over smallThumbnail
        String url = imageLinks.thumbnail != null ? imageLinks.thumbnail : imageLinks.smallThumbnail;

        // Truncate if too long (max 500 chars)
        if (url != null && url.length() > 500) {
            url = url.substring(0, 500);
        }

        return url;
    }

    /**
     * Convert list of volumes to list of resources.
     * Filters out null results from failed conversions.
     *
     * @param response Google Books API response
     * @return List of Resource entities
     */
    public List<Resource> toResources(GoogleBooksResponse response) {
        List<Resource> resources = new ArrayList<>();

        if (response == null || response.items == null) {
            return resources;
        }

        for (GoogleBooksResponse.BookVolume volume : response.items) {
            Resource resource = toResource(volume);
            if (resource != null) {
                resources.add(resource);
            }
        }

        return resources;
    }
}
