package com.akfc.catalog.dto;

import java.util.List;

/**
 * Enriched resource response containing resource information
 * along with reviews and user details aggregated from other services.
 */
public class EnrichedResourceResponse {

    // Resource information
    public Long id;
    public String title;
    public String type;
    public Integer year;
    public String creator;
    public List<String> keywords;
    public String illustrationUrl;
    public String status;

    // Enriched review information (with nested user data)
    public List<ReviewDto> reviews;

    // Review statistics
    public ReviewStatistics statistics;

    public EnrichedResourceResponse() {
    }

    /**
     * Statistics about reviews for this resource.
     */
    public static class ReviewStatistics {
        public int totalReviews;
        public Double averageRating;
        public int fiveStarCount;
        public int fourStarCount;
        public int threeStarCount;
        public int twoStarCount;
        public int oneStarCount;

        public ReviewStatistics() {
        }

        public ReviewStatistics(List<ReviewDto> reviews) {
            this.totalReviews = reviews.size();

            if (!reviews.isEmpty()) {
                this.averageRating = reviews.stream()
                        .mapToInt(r -> r.rating)
                        .average()
                        .orElse(0.0);

                this.fiveStarCount = (int) reviews.stream().filter(r -> r.rating == 5).count();
                this.fourStarCount = (int) reviews.stream().filter(r -> r.rating == 4).count();
                this.threeStarCount = (int) reviews.stream().filter(r -> r.rating == 3).count();
                this.twoStarCount = (int) reviews.stream().filter(r -> r.rating == 2).count();
                this.oneStarCount = (int) reviews.stream().filter(r -> r.rating == 1).count();
            } else {
                this.averageRating = null;
            }
        }
    }
}
