package com.akfc.reviews.dto;

import jakarta.validation.constraints.*;

/**
 * DTO for creating a new review.
 */
public class CreateReviewRequest {

    @NotNull(message = "Work ID is required")
    public Long resourceId;

    @NotNull(message = "User ID is required")
    public Long userId;

    @NotNull(message = "Rating is required")
    @Min(value = 1, message = "Rating must be at least 1")
    @Max(value = 5, message = "Rating must be at most 5")
    public Integer rating;

    @NotBlank(message = "Comment is required")
    @Size(min = 10, max = 2000, message = "Comment must be between 10 and 2000 characters")
    public String comment;
}
