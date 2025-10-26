package com.akfc.reviews.dto;

import com.akfc.reviews.data.Review;
import com.akfc.reviews.data.ReviewStatus;

import java.time.LocalDate;
import java.time.LocalDateTime;

/**
 * DTO for review response.
 * Contains all review information for API responses.
 */
public class ReviewResponse {

    public Long id;
    public Long resourceId;
    public Long userId;
    public Integer rating;
    public String comment;
    public LocalDate publicationDate;
    public ReviewStatus status;
    public String moderatedBy;
    public LocalDateTime moderatedAt;
    public Boolean archived;
    public LocalDateTime createdAt;
    public LocalDateTime updatedAt;

    /**
     * Create response DTO from entity.
     */
    public static ReviewResponse from(Review review) {
        ReviewResponse response = new ReviewResponse();
        response.id = review.id;
        response.resourceId = review.resourceId;
        response.userId = review.userId;
        response.rating = review.rating;
        response.comment = review.comment;
        response.publicationDate = review.publicationDate;
        response.status = review.status;
        response.moderatedBy = review.moderatedBy;
        response.moderatedAt = review.moderatedAt;
        response.archived = review.archived;
        response.createdAt = review.createdAt;
        response.updatedAt = review.updatedAt;
        return response;
    }
}
