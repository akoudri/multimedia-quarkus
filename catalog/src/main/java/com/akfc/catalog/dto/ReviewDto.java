package com.akfc.catalog.dto;

import com.fasterxml.jackson.annotation.JsonProperty;

import java.time.LocalDate;
import java.time.LocalDateTime;

/**
 * DTO for Review data received from the reviews service.
 * This is used when enriching catalog resources with review information.
 */
public class ReviewDto {

    public Long id;

    @JsonProperty("resourceId")
    public Long resourceId;

    @JsonProperty("userId")
    public Long userId;

    public Integer rating;

    public String comment;

    @JsonProperty("publicationDate")
    public LocalDate publicationDate;

    public String status;

    @JsonProperty("createdAt")
    public LocalDateTime createdAt;

    // Nested user info will be added later when we aggregate
    public UserDto user;

    public ReviewDto() {
    }
}
