package com.akfc.users.dto;

import jakarta.validation.constraints.*;

/**
 * DTO for updating an existing user.
 */
public class UpdateUserRequest {

    @NotBlank(message = "First name is required")
    @Size(min = 1, max = 100, message = "First name must be between 1 and 100 characters")
    public String firstName;

    @NotBlank(message = "Last name is required")
    @Size(min = 1, max = 100, message = "Last name must be between 1 and 100 characters")
    public String lastName;

    @Pattern(regexp = "^\\+?[1-9]\\d{1,14}$",
             message = "Phone number must be valid international format")
    @Size(max = 20, message = "Phone number must not exceed 20 characters")
    public String phoneNumber;
}
