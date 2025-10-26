package com.akfc.catalog.dto;

import jakarta.validation.constraints.*;

import java.util.List;

/**
 * DTO for updating an existing resource.
 */
public class UpdateResourceRequest {

    @NotBlank(message = "Title is required")
    @Size(min = 1, max = 255, message = "Title must be between 1 and 255 characters")
    public String title;

    @NotNull(message = "Year is required")
    @Min(value = 1000, message = "Year must be at least 1000")
    @Max(value = 9999, message = "Year must be at most 9999")
    public Integer year;

    @NotBlank(message = "Creator is required")
    @Size(min = 1, max = 255, message = "Creator must be between 1 and 255 characters")
    public String creator;

    public List<String> keywords;

    @Size(max = 500, message = "Illustration URL must not exceed 500 characters")
    public String illustrationUrl;
}
